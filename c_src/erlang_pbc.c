/*
 * Copyright 2018 Helium Systems Inc. All Rights Reserved.
 *
 * Licensed under the OpenSSL license (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

#include "erl_nif.h"
#include <pbc/pbc.h>
#include <string.h>

static ErlNifResourceType *PBC_GROUP_RESOURCE;

static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_enomem;
static ERL_NIF_TERM atom_enotsup;

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
    pairing_t* group = enif_alloc_resource(PBC_GROUP_RESOURCE, sizeof(pairing_t));
    // initialize the group
    int res = pairing_init_set_str(*group, param_buf);
    // always release the resource, BEAM will GC it
    enif_release_resource(group);
    if (res != 0) {
        // params were bad, sorry
        return enif_raise_exception(env, atom_enotsup);
    }
    // return a resource handle to Erlang
    return enif_make_resource(env, group);
}

void group_destructor(ErlNifEnv *env, void *res) {
    (void)env;
    pairing_clear(*((pairing_t*) res));
}

static ErlNifFunc nif_funcs[] = {
    {"group_new", 1, group_new, 0},
    /*{"element_set", 1, element_new, 0},*/
    /*{"element_from_hash", 1, element_new, 0},*/
    /*{"element_random", 1, element_new, 0},*/
    /*{"element_mul", 1, element_new, 0},*/
    /*{"element_add", 1, element_new, 0},*/
    /*{"element_sub", 1, element_new, 0},*/
    /*{"element_new", 1, element_new, 0}};*/
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
    return 0;
}

ERL_NIF_INIT(erlang_pbc, nif_funcs, load, NULL, NULL, NULL);
