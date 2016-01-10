#ifndef EZLIB_C_SRC_EZLIB_NIF_H_
#define EZLIB_C_SRC_EZLIB_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    
    //settings
    
    ERL_NIF_TERM atomCompressionLevel;
    ERL_NIF_TERM atomWindowBits;
    ERL_NIF_TERM atomMemLevel;
    ERL_NIF_TERM atomCompStrategy;
};

struct ezlib_data
{
    ErlNifResourceType* resZlibSession;
};

extern atoms ATOMS;

#endif
