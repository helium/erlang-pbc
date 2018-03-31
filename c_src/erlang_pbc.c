/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the OpenSSL license (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

//#define PBC_DEBUG 1
#include "erl_nif.h"
#include <pbc/pbc.h>
#include <string.h>
#include <stdbool.h>

static ErlNifResourceType *PBC_GROUP_RESOURCE;
static ErlNifResourceType *PBC_ELEMENT_RESOURCE;

static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_enomem;
static ERL_NIF_TERM atom_enotsup;

struct pbc_group {
    pairing_t pairing;
    bool initialized;
};

struct pbc_element {
    element_t element;
    element_pp_t pp;
    bool initialized;
    bool pp_initialized;
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

    struct pbc_group *group;
    if (!enif_get_resource(env, argv[1], PBC_GROUP_RESOURCE, (void**)&group)) {
        return enif_make_badarg(env);
    }

    // increment the reference count on the group
    enif_keep_resource(group);

    struct pbc_element* element = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));

    if (strncmp("G1", groupname, 2) == 0) {
        element_init_G1(element->element, group->pairing);
    } else if (strncmp("G2", groupname, 2) == 0) {
        element_init_G2(element->element, group->pairing);
    } else if (strncmp("GT", groupname, 2) == 0) {
        element_init_GT(element->element, group->pairing);
    } else if (strncmp("Zr", groupname, 2) == 0) {
        element_init_Zr(element->element, group->pairing);
    }

    element->initialized = true;
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

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_add(element_new->element, element_a->element, element_b->element);

    /*element_printf("Added %B + %B = %B\n", element_a->element, element_b->element, element_new->element);*/

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_sub(element_new->element, element_a->element, element_b->element);

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_mul(element_new->element, element_a->element, element_b->element);
    /*element_printf("Mul %B * %B = %B\n", element_a->element, element_b->element, element_new->element);*/

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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
    mpz_import(n, bin.size/4, 1, 4, 1, 0, bin.data);
    /*printf("SET ELEMENT TO");*/
    /*mpz_out_str(NULL, 10, n);*/
    /*printf("\n");*/

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_set_mpz(element_new->element, n);
    mpz_clear(n);
    
    /*element_printf("set %B to %B\n", element_a->element, element_new->element);*/

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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
    mpz_import(n, bin.size/4, 1, 4, 1, 0, bin.data);

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_mul_mpz(element_new->element, element_a->element, n);
    mpz_clear(n);

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_div(element_new->element, element_a->element, element_b->element);

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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
    mpz_import(n, bin.size/4, 1, 4, 1, 0, bin.data);

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_pow_mpz(element_new->element, element_a->element, n);
    mpz_clear(n);

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
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

    struct pbc_element* element_new = enif_alloc_resource(PBC_ELEMENT_RESOURCE, sizeof(struct pbc_element));
    element_init_same_as(element_new->element, element_a->element);
    element_pow_zn(element_new->element, element_a->element, element_b->element);

    // increment the reference count on the group
    enif_keep_resource(element_a->group);

    element_new->initialized = true;
    element_new->group = element_a->group;

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

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
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

    char buf[2048];
    element_snprint(buf, 2048, element->element);
    buf[2047] = '\0';
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

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
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
    if (!enif_alloc_binary(bytes, &bin)) {
        return enif_make_badarg(env);
    }

    element_to_bytes(bin.data, element->element);
    return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM
pbc_binary_to_element(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    mpz_t *r;
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
    element_from_bytes(element_new->element, bin.data);

    // increment the reference count on the group
    enif_keep_resource(element->group);

    element_new->initialized = true;
    element_new->group = element->group;

    ERL_NIF_TERM term = enif_make_resource(env, element_new);
    // always release the resource, BEAM will GC it when nobody is using it anymore
    enif_release_resource(element_new);
    return term;
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
        // decrement reference count
        enif_release_resource(element->group);
        element_clear(element->element);
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
    {"element_new", 2, pbc_element_new, 0},
    {"element_from_hash_nif", 2, pbc_element_from_hash, 0},
    {"element_random", 1, pbc_element_random, 0},
    {"element_to_string", 1, pbc_element_to_string, 0},
    {"element_to_binary", 1, pbc_element_to_binary, 0},
    {"binary_to_element", 2, pbc_binary_to_element, 0},
    {"element_cmp", 2, pbc_element_cmp, 0},
    };

static int
load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;
    atom_undefined = enif_make_atom(env, "undefined");
    atom_enomem    = enif_make_atom(env, "enomem");
    atom_enotsup    = enif_make_atom(env, "enotsup");
    pbc_set_memory_functions(&enif_alloc, &enif_realloc, &enif_free);
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    PBC_GROUP_RESOURCE = enif_open_resource_type(env, NULL, "pbc_group", group_destructor, flags, NULL);
    PBC_ELEMENT_RESOURCE = enif_open_resource_type(env, NULL, "pbc_group", element_destructor, flags, NULL);
    return 0;
}

ERL_NIF_INIT(erlang_pbc, nif_funcs, load, NULL, NULL, NULL);
