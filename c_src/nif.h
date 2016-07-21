#ifndef EZLIB_C_SRC_NIF_H_
#define EZLIB_C_SRC_NIF_H_

#include "erl_nif.h"

#if defined(_USE_PRAGMA_WEAK)
    #pragma weak enif_free
    #pragma weak enif_alloc
    #pragma weak enif_alloc_resource
    #pragma weak enif_get_int
    #pragma weak enif_get_list_cell
    #pragma weak enif_get_resource
    #pragma weak enif_get_tuple
    #pragma weak enif_inspect_binary
    #pragma weak enif_inspect_iolist_as_binary
    #pragma weak enif_is_binary
    #pragma weak enif_is_identical
    #pragma weak enif_is_list
    #pragma weak enif_make_atom
    #pragma weak enif_make_badarg
    #pragma weak enif_make_existing_atom
    #pragma weak enif_make_double
    #pragma weak enif_make_new_binary
    #pragma weak enif_make_resource
    #pragma weak enif_make_string_len
    #pragma weak enif_make_tuple
    #pragma weak enif_make_ulong
    #pragma weak enif_open_resource_type
    #pragma weak enif_release_resource
    #pragma weak enif_priv_data
    #pragma weak enif_self
#endif

#endif
