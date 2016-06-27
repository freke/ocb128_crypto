#ifndef OCB128_CRYPTO_PORT_NIF_H
#define OCB128_CRYPTO_PORT_NIF_H

#include "erl_nif.h"

ERL_NIF_TERM pack(cryptState_t *cs, ErlNifEnv* env);
int unpack(cryptState_t *cs, const ERL_NIF_TERM tuple[], ErlNifEnv* env);

#endif
