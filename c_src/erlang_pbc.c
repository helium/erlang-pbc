/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the OpenSSL license (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

/*#define PBC_DEBUG 1*/
#include "erl_nif.h"
#include <pbc/pbc.h>
#include <string.h>
#include <stdbool.h>

static ErlNifResourceType *PBC_GROUP_RESOURCE;
static ErlNifResourceType *PBC_ELEMENT_RESOURCE;

static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_group_mismatch;
static ERL_NIF_TERM atom_enotsup;
static bool missed_pp_counts;
static unsigned int missed_pp_threshold;

void group_destructor(ErlNifEnv *env, void *res);
void element_destructor(ErlNifEnv *env, void *res);

struct pbc_group {
    pairing_t pairing;
    bool initialized;
};

enum field {G1, G2, GT, Zr};

struct pbc_element {
    element_t element;
    pairing_pp_t p;
    element_pp_t pp;
    enum field field;
    bool initialized;
    bool pp_initialized;
    bool p_initialized;
    unsigned int missed_pp;
    struct pbc_group *group;
};

static ERL_NIF_TERM
mk_atom(ErlNifEnv * env, const char * atom)
{
    ERL_NIF_TERM ret;

    if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM
group_new(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_is_binary(env, argv[0])) {
        return enif_make_badarg(env);
    }
    ErlNifBinary bin;
    char *param_buf;

    if (!enif_inspect_binary(env, argv[0], &bin))
    {
        return enif_make_badarg(env);
    }

    // copy the non null-terminated binary data into a null terminated char array
    param_buf = malloc(bin.size+1);
    strncpy(param_buf, (const char*)bin.data, bin.size);
    param_buf[bin.size] = '\0';

    // allocate the resource
    struct pbc_group* group = enif_alloc_resource(PBC_GROUP_RESOURCE, sizeof(struct pbc_group));
    // initialize the group
    int res = pairing_init_set_str(group->pairing, param_buf);
    free(param_buf);
    if (res != 0) {
        group->initialized = false;
        // always release the resource, BEAM will GC it
        enif_release_resource(group);
        // params were bad, sorry
        return enif_raise_exception(env, atom_enotsup);
    }
    group->initialized = true;
    // return a resource handle to Erlang
    ERL_NIF_TERM term = enif_make_resource(env, group);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(group);
    return term;
}

static ERL_NIF_TERM
pbc_element_new(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    // check the group is G1, G2, GT or Zr
    char groupname[3];
    if (!enif_get_atom(env, argv[0], groupname, 3, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }


    if (strncmp("G1", groupname, 2) != 0 && strncmp("G2", groupname, 2) != 0 && strncmp("GT", groupname, 2) != 0 && strncmp("Zr", groupname, 2) != 0) {
        return enif_make_badarg(env);
    }

    // all elements know their pairing, so we can use an element to initialize another element in the same pairing, even in a different group
    struct pbc_element *element_in;
    struct pbc_group *group;
    if (enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_in)) {
        group = element_in->group;
    } else if (enif_get_resource(env, argv[1], PBC_GROUP_RESOURCE, (void**)&group)) {
        // nothing to actually do here, weee
    } else {
        return enif_make_badarg(env);
    }

    // increment the reference count on the group
    enif_keep_resource(group);

    struct pbc_element* element = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));

    if (strncmp("G1", groupname, 2) == 0) {
        element_init_G1(element->element, group->pairing);
        element->field = G1;
    } else if (strncmp("G2", groupname, 2) == 0) {
        element_init_G2(element->element, group->pairing);
        element->field = G2;
    } else if (strncmp("GT", groupname, 2) == 0) {
        element_init_GT(element->element, group->pairing);
        element->field = GT;
    } else if (strncmp("Zr", groupname, 2) == 0) {
        element_init_Zr(element->element, group->pairing);
        element->field = Zr;
    }

    element->initialized = true;
    element->p_initialized = false;
    element->pp_initialized = false;
    element->missed_pp = 0;
    element->group = group;

    ERL_NIF_TERM term = enif_make_resource(env, element);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element);
    return term;
}

static ERL_NIF_TERM
pbc_element_add(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

    if (element_a->element->field != element_b->element->field) {
        return enif_raise_exception(env, atom_group_mismatch);
    }

#ifdef PBC_DEBUG
    element_printf("%B + %B ", element_a->element, element_b->element);
#endif
    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_add(element_new->element, element_a->element, element_b->element);
    element_new->field = element_a->field;

#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_sub(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

    if (element_a->element->field != element_b->element->field) {
        return enif_raise_exception(env, atom_group_mismatch);
    }

#ifdef PBC_DEBUG
    element_printf("%B - %B ", element_a->element, element_b->element);
#endif

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_sub(element_new->element, element_a->element, element_b->element);
    element_new->field = element_a->field;

#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_mul(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

#ifdef PBC_DEBUG
    element_printf("Mul %B * %B ", element_a->element, element_b->element);
#endif

    struct pbc_element* element_new = NULL;
    if (element_a->element->field != element_b->element->field) {
        // one of them has to be in Zr
        if (element_a->element->field == element_a->group->pairing->Zr && element_b->element->field != element_b->group->pairing->Zr) {
            element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
            element_init_same_as(element_new->element, element_b->element);
            element_mul_zn(element_new->element, element_b->element, element_a->element);
            element_new->field = element_b->field;
        } else if (element_a->element->field != element_a->group->pairing->Zr && element_b->element->field == element_b->group->pairing->Zr) {
            element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
            element_init_same_as(element_new->element, element_a->element);
            element_mul_zn(element_new->element, element_a->element, element_b->element);
            element_new->field = element_a->field;
        } else {
            // error
#ifdef PBC_DEBUG
            printf("= ERROR\n");
#endif
            return enif_raise_exception(env, atom_group_mismatch);
        }
    } else {
        element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
        element_init_same_as(element_new->element, element_a->element);
        element_mul(element_new->element, element_a->element, element_b->element);
        element_new->field = element_a->field;
    }
#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_set_mpz(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    mpz_t n;
    mpz_init(n);
    mpz_import(n, (bin.size - 1)/4, 1, 4, 1, 0, bin.data+1);
    if (bin.data[0] == 0xff) {
        mpz_neg(n, n);
    }
#ifdef PBC_DEBUG
    printf("SET ELEMENT TO ");
    mpz_out_str(NULL, 10, n);
    printf("\n");
#endif

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_set_mpz(element_new->element, n);
    element_new->field = element_a->field;
    mpz_clear(n);

#ifdef PBC_DEBUG
    element_printf("set %B to %B\n", element_a->element, element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_mul_mpz(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    mpz_t n;
    mpz_init(n);
    mpz_import(n, (bin.size - 1)/4, 1, 4, 1, 0, bin.data+1);
    if (bin.data[0] == 0xff) {
        mpz_neg(n, n);
    }
#ifdef PBC_DEBUG
    element_printf("MUL %B BY ", element_a->element);
    mpz_out_str(NULL, 10, n);
#endif

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_mul_mpz(element_new->element, element_a->element, n);
    element_new->field = element_a->field;
    mpz_clear(n);

#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_div(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

    if (element_a->element->field != element_b->element->field) {
        return enif_raise_exception(env, atom_group_mismatch);
    }

#ifdef PBC_DEBUG
    element_printf("%B div %B ", element_a->element, element_b->element);
#endif
    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_div(element_new->element, element_a->element, element_b->element);
    element_new->field = element_a->field;
#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_pow_mpz(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    mpz_t n;
    mpz_init(n);
    mpz_import(n, (bin.size - 1)/4, 1, 4, 1, 0, bin.data+1);
    if (bin.data[0] == 0xff) {
        mpz_neg(n, n);
    }

#ifdef PBC_DEBUG
    element_printf("%B pow ", element_a->element);
    mpz_out_str(NULL, 10, n);
#endif

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    if (element_a->pp_initialized) {
        element_pp_pow(element_new->element, n, element_a->pp);
    } else {
        if (missed_pp_counts) {
            element_a->missed_pp++;
            if (element_a->missed_pp > missed_pp_threshold) {
                enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[0]));
            }
        }
        element_pow_mpz(element_new->element, element_a->element, n);
    }
    element_new->field = element_a->field;
    mpz_clear(n);
#ifdef PBC_DEBUG
    element_printf(" = %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_pow_zn(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

#ifdef PBC_DEBUG
    element_printf("%B pow %B ", element_a->element, element_b->element);
#endif

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    if (element_a->pp_initialized) {
        element_pp_pow_zn(element_new->element, element_b->element, element_a->pp);
    } else {
        if (missed_pp_counts) {
            element_a->missed_pp++;
            if (element_a->missed_pp > missed_pp_threshold) {
                enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[0]));
            }
        }
        element_pow_zn(element_new->element, element_a->element, element_b->element);
    }
    element_new->field = element_a->field;
#ifdef PBC_DEBUG
    element_printf("= %B\n", element_new->element);
#endif

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_neg(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element->element);
    element_neg(element_new->element, element->element);
    element_new->field = element->field;

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_random(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element->element);
    element_random(element_new->element);
    element_new->field = element->field;

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}


static ERL_NIF_TERM
pbc_element_to_string(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    char buf[4096];
    element_snprint(buf, 4096, element->element);
    buf[4095] = '\0';
    return enif_make_string(env, buf, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM
pbc_group_order(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    mpz_t *r;
    struct pbc_element *element;
    struct pbc_group *group;
    if (enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        r = &(element->group->pairing->r);
    } else if (enif_get_resource(env, argv[0], PBC_GROUP_RESOURCE, (void**)&group)) {
        r = &(group->pairing->r);
    } else {
        return enif_make_badarg(env);
    }

    // the size of the group order, in bits
    size_t sz = mpz_sizeinbase(*r, 2);
    ERL_NIF_TERM term = enif_make_long(env, (long int)sz);
    return term;
}

static ERL_NIF_TERM
pbc_element_from_hash(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element->element);
    element_from_hash(element_new->element, bin.data, bin.size);
    element_new->field = element->field;

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_element_to_binary(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    int bytes = element_length_in_bytes(element->element);

    ErlNifBinary bin;
    if (!enif_alloc_binary(bytes+1, &bin)) {
        return enif_make_badarg(env);
    }

    bin.data[0] = (uint8_t) element->field;
    element_to_bytes(bin.data+1, element->element);
    return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM
pbc_binary_to_element(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    struct pbc_group *group;
    if (enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        group = element->group;
    } else if (!enif_get_resource(env, argv[0], PBC_GROUP_RESOURCE, (void**)&group)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    switch ((enum field) bin.data[0]) {
        case G1:
            element_init_G1(element_new->element, group->pairing);
            break;
        case G2:
            element_init_G2(element_new->element, group->pairing);
            break;
        case GT:
            element_init_GT(element_new->element, group->pairing);
            break;
        case Zr:
            element_init_Zr(element_new->element, group->pairing);
            break;
    }
    element_from_bytes(element_new->element, bin.data+1);
    element_new->field = (uint8_t) bin.data[0];

    // increment the reference count on the group
    enif_keep_resource(group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->group = group;
    element_new->missed_pp = 0;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_elements_to_binary(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if (!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }
    unsigned int in_len = 0;

    if (!enif_get_list_length(env, argv[0], &in_len)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM in_list = argv[0];
    ERL_NIF_TERM out_list = enif_make_list(env, 0);
    ERL_NIF_TERM cur;
    struct pbc_element *element;

    while(enif_get_list_cell(env, in_list, &cur, &in_list)) {
        if (!enif_get_resource(env, cur, PBC_ELEMENT_RESOURCE, (void**)&element)) {
            return enif_make_badarg(env);
        }

        int bytes = element_length_in_bytes(element->element);

        ErlNifBinary bin;
        if (!enif_alloc_binary(bytes+1, &bin)) {
            return enif_make_badarg(env);
        }

        bin.data[0] = (uint8_t) element->field;
        element_to_bytes(bin.data+1, element->element);
        out_list = enif_make_list_cell(env, enif_make_binary(env, &bin), out_list);
    }

    ErlNifBinary result;

    enif_term_to_binary(env, out_list, &result);

    return enif_make_binary(env, &result);
}

static ERL_NIF_TERM
pbc_binary_to_elements(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if (!enif_is_binary(env, argv[1])) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    struct pbc_group *group;
    if (enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        group = element->group;
    } else if (!enif_get_resource(env, argv[0], PBC_GROUP_RESOURCE, (void**)&group)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[1], &bin)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM in_list = argv[0];
    ERL_NIF_TERM out_list = enif_make_list(env, 0);
    ERL_NIF_TERM cur;

    if (!enif_binary_to_term(env, bin.data, bin.size, &in_list, ERL_NIF_BIN2TERM_SAFE)) {
        return enif_make_badarg(env);
    }

    unsigned int in_len = 0;

    if (!enif_get_list_length(env, in_list, &in_len)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM args[2];
    args[0] = argv[0];

    while(enif_get_list_cell(env, in_list, &cur, &in_list)) {
        if (!enif_inspect_binary(env, cur, &bin)) {
            return enif_make_badarg(env);
        }

        args[1] = cur;

        cur = pbc_binary_to_element(env, argc, args);

        out_list = enif_make_list_cell(env, cur, out_list);
    }

    return out_list;
}

static ERL_NIF_TERM
pbc_element_cmp(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

    if (element_cmp(element_a->element, element_b->element) == 0) {
        return mk_atom(env, "true");
    } else {
        return mk_atom(env, "false");
    }
}


static ERL_NIF_TERM
pbc_element_is0(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    if (element_is0(element->element) == 0) {
        return mk_atom(env, "false");
    } else {
        return mk_atom(env, "true");
    }
}


static ERL_NIF_TERM
pbc_element_is1(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    if (element_is1(element->element) == 0) {
        return mk_atom(env, "false");
    } else {
        return mk_atom(env, "true");
    }
}

static ERL_NIF_TERM
pbc_element_pairing(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_a;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element_a)) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element_b;
    if (!enif_get_resource(env, argv[1], PBC_ELEMENT_RESOURCE, (void**)&element_b)) {
        return enif_make_badarg(env);
    }

    struct pbc_element* element_new = NULL;

    if (pairing_is_symmetric(element_a->group->pairing) && element_a->element->field == element_b->element->field) {
        // symmetric pairings need both operands in the same field
        element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
        element_init_GT(element_new->element, element_a->group->pairing);
        if (element_a->p_initialized) {
            pairing_pp_apply(element_new->element, element_b->element, element_a->p);
        } else if (element_b->p_initialized) {
            pairing_pp_apply(element_new->element, element_a->element, element_b->p);
        } else {
            if (missed_pp_counts && element_a->field == G1) {
                element_a->missed_pp++;
                element_b->missed_pp++;
                if (element_a->missed_pp > missed_pp_threshold) {
                    enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[0]));
                } else if (element_b->missed_pp > missed_pp_threshold) {
                    enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[1]));
                }
            }
            element_pairing(element_new->element, element_a->element, element_b->element);
        }
    } else if ((!pairing_is_symmetric(element_a->group->pairing)) && element_a->element->field != element_b->element->field) {
        // assymetric pairings need one element from G1 and one from G2
        if (element_a->element->field == element_a->group->pairing->G1 && element_b->element->field == element_b->group->pairing->G2) {
            element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
            element_init_GT(element_new->element, element_a->group->pairing);
            if (element_a->p_initialized) {
                pairing_pp_apply(element_new->element, element_b->element, element_a->p);
            } else {
                if (missed_pp_counts && element_a->field == G1) {
                    element_a->missed_pp++;
                    if (element_a->missed_pp > missed_pp_threshold) {
                        enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[0]));
                    }
                }
                element_pairing(element_new->element, element_a->element, element_b->element);
            }
        } else if (element_a->element->field == element_a->group->pairing->G2 && element_b->element->field == element_b->group->pairing->G1) {
            element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
            element_init_GT(element_new->element, element_a->group->pairing);
            if (element_b->p_initialized) {
                pairing_pp_apply(element_new->element, element_a->element, element_b->p);
            } else {
                if (missed_pp_counts && element_b->field == G1) {
                    element_b->missed_pp++;
                    if (element_b->missed_pp > missed_pp_threshold) {
                        enif_raise_exception(env, enif_make_tuple2(env, mk_atom(env, "pp_threshold"), argv[1]));
                    }
                }
                element_pairing(element_new->element, element_b->element, element_a->element);
            }
        } else {
            return enif_raise_exception(env, atom_group_mismatch);
        }
    } else {
        return enif_raise_exception(env, atom_group_mismatch);
    }

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->p_initialized = false;
    element_new->pp_initialized = false;
    element_new->missed_pp = 0;
    element_new->group = element_a->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
}

static ERL_NIF_TERM
pbc_pairing_is_symmetric(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    struct pbc_group *group;
    if (enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        group = element->group;
    } else if (!enif_get_resource(env, argv[0], PBC_GROUP_RESOURCE, (void**)&group)) {
        return enif_make_badarg(env);
    }

    if (pairing_is_symmetric(group->pairing)) {
        return mk_atom(env, "true");
    } else {
        return mk_atom(env, "false");
    }
}

static ERL_NIF_TERM
pbc_element_pp_init(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element)) {
        return enif_make_badarg(env);
    }

    if (element->pp_initialized) {
        return mk_atom(env, "ok");
    }

    element_pp_init(element->pp, element->element);
    element->pp_initialized = true;

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
pbc_pairing_pp_init(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    struct pbc_element *element;
    if (!enif_get_resource(env, argv[0], PBC_ELEMENT_RESOURCE, (void**)&element) || element->field != G1) {
        return enif_make_badarg(env);
    }

    if (element->p_initialized) {
        return mk_atom(env, "ok");
    }

    pairing_pp_init(element->p, element->element, element->group->pairing);
    element->p_initialized = true;

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
enable_pp_counts(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    // first argument should be a boolean
    char atombuf[10];
    if (!enif_is_atom(env, argv[0]) || enif_get_atom(env, argv[0], atombuf, 10, ERL_NIF_LATIN1) == 0) {
        return enif_make_badarg(env);
    }

    // second argument should be the threshold
    int threshold;
    if (!enif_get_int(env, argv[1], &threshold) || threshold < 0) {
        return enif_make_badarg(env);
    }

    if (strncmp("true", atombuf, 5) == 0) {
        missed_pp_counts = true;
    } else if (strncmp("false", atombuf, 6) == 0) {
        missed_pp_counts = false;
    } else {
        return enif_make_badarg(env);
    }

    missed_pp_threshold = threshold;

    return mk_atom(env, "ok");
}

void group_destructor(ErlNifEnv *env, void *res) {
    (void)env;
    struct pbc_group *group = (struct pbc_group *) res;
    if (group->initialized) {
        pairing_clear(group->pairing);
    }
}

void element_destructor(ErlNifEnv *env, void *res) {
    (void)env;
    struct pbc_element *element = (struct pbc_element *) res;
    if (element->initialized) {
        if (element->p_initialized) {
            pairing_pp_clear(element->p);
        }
        if (element->pp_initialized) {
            element_pp_clear(element->pp);
        }
        element_clear(element->element);
        // decrement reference count
        enif_release_resource(element->group);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"group_new_nif", 1, group_new, 0},
    {"group_order", 1, pbc_group_order, 0},
    {"element_set_mpz_nif", 2, pbc_element_set_mpz, 0},
    {"element_add_nif", 2, pbc_element_add, 0},
    {"element_sub_nif", 2, pbc_element_sub, 0},
    {"element_mul_nif", 2, pbc_element_mul, 0},
    {"element_mul_mpz_nif", 2, pbc_element_mul_mpz, 0},
    {"element_div_nif", 2, pbc_element_div, 0},
    {"element_pow_mpz", 2, pbc_element_pow_mpz, 0},
    {"element_pow_zn", 2, pbc_element_pow_zn, 0},
    {"element_neg", 1, pbc_element_neg, 0},
    {"element_new", 2, pbc_element_new, 0},
    {"element_from_hash_nif", 2, pbc_element_from_hash, 0},
    {"element_random", 1, pbc_element_random, 0},
    {"element_to_string", 1, pbc_element_to_string, 0},
    {"element_to_binary", 1, pbc_element_to_binary, 0},
    {"binary_to_element", 2, pbc_binary_to_element, 0},
    {"elements_to_binary", 1, pbc_elements_to_binary, 0},
    {"binary_to_elements", 2, pbc_binary_to_elements, 0},
    {"element_cmp", 2, pbc_element_cmp, 0},
    {"element_pairing", 2, pbc_element_pairing, 0},
    {"pairing_is_symmetric", 1, pbc_pairing_is_symmetric, 0},
    {"element_pp_init", 1, pbc_element_pp_init, 0},
    {"pairing_pp_init", 1, pbc_pairing_pp_init, 0},
    {"element_is0", 1, pbc_element_is0, 0},
    {"element_is1", 1, pbc_element_is1, 0},
    {"enable_pp_counts", 2, enable_pp_counts, 0}
    };

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;
    missed_pp_counts = false;
    missed_pp_threshold = 0;
    atom_undefined = enif_make_atom(env, "undefined");
    atom_group_mismatch    = enif_make_atom(env, "group_mismatch");
    atom_enotsup    = enif_make_atom(env, "enotsup");
    pbc_set_memory_functions(&enif_alloc, &enif_realloc, &enif_free);
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    PBC_GROUP_RESOURCE = enif_open_resource_type(env, NULL, "pbc_group", group_destructor, flags, NULL);
    PBC_ELEMENT_RESOURCE = enif_open_resource_type(env, NULL, "pbc_group", element_destructor, flags, NULL);
    return 0;
}

ERL_NIF_INIT(erlang_pbc, nif_funcs, load, NULL, NULL, NULL);
