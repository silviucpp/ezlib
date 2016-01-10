#ifndef EZLIB_C_SRC_EZLIB_H_
#define EZLIB_C_SRC_EZLIB_H_

#include "erl_nif.h"

void nif_zlib_session_free(ErlNifEnv* env, void* obj);

ERL_NIF_TERM nif_zlib_new_session(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_zlib_process_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_zlib_read_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_get_stats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
