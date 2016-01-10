#include "ezlib_nif.h"
#include "nif_utils.h"
#include "ezlib.h"
#include "macros.h"

const char kAtomOk[] = "ok";
const char kAtomError[] = "error";
const char kAtomTrue[] = "true";
const char kAtomFalse[] = "false";

atoms ATOMS;

void open_resources(ErlNifEnv* env, ezlib_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->resZlibSession = enif_open_resource_type(env, NULL, "zlib_session", nif_zlib_session_free, flags, NULL);
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);
    
    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    
    ezlib_data* data = static_cast<ezlib_data*>(enif_alloc(sizeof(ezlib_data)));
    open_resources(env, data);
    
    *priv_data = data;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    
    ezlib_data* data = static_cast<ezlib_data*>(priv_data);
    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);
    
    ezlib_data* data = static_cast<ezlib_data*>(enif_alloc(sizeof(ezlib_data)));
    open_resources(env, data);
    
    *priv = data;
    return 0;
}

static ErlNifFunc nif_funcs[] =
{    
    {"new_session", 1, nif_zlib_new_session},
    {"process_buffer", 3, nif_zlib_process_buffer},
    {"read_data", 1, nif_zlib_read_data},
};

ERL_NIF_INIT(ezlib_nif, nif_funcs, on_nif_load, NULL, on_nif_upgrade, on_nif_unload)

