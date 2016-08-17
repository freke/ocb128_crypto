-module(ocb128_crypto).

-export([generate_key/0]).
-export([key/1, encrypt_iv/1, decrypt_iv/1, resync/2]).
-export([good/1, late/1, lost/1]).
-export([encrypt/2, decrypt/2]).

-type statistics() :: {number(), number(), number(), number()}.
-type key() :: {binary(), binary(), binary(), binary(), statistics()}.

-export_type([key/0]).

-record(key, {key, decrypt_iv, encrypt_iv, history, good, late, lost, resync}).

generate_key() ->
  #key{
    key=crypto:strong_rand_bytes(16),
    decrypt_iv=crypto:strong_rand_bytes(16),
    encrypt_iv=crypto:strong_rand_bytes(16),
    history=binary:copy(<<0>>, 255),
    good=0,
    late=0,
    lost=0,
    resync=0
  }.

key(Key) ->
  Key#key.key.

encrypt_iv(Key) ->
  Key#key.encrypt_iv.

decrypt_iv(Key) ->
  Key#key.decrypt_iv.

resync(Key, Div) ->
  Key#key{decrypt_iv=Div, resync=Key#key.resync+1}.

good(Key) ->
  Key#key.good.

late(Key) ->
  Key#key.late.

lost(Key) ->
  Key#key.lost.

decrypt(Key, Msg) ->
  {ok, {K, D, E, H, {G, La, Lo, R}}, Decrypted} = ocb128_crypto_serv:decrypt(
    {
      Key#key.key,
      Key#key.decrypt_iv,
      Key#key.encrypt_iv,
      Key#key.history,
      {
        Key#key.good,
        Key#key.late,
        Key#key.lost,
        Key#key.resync
      }
    },
    Msg
  ),
  NewKey = Key#key{
    key=K,
    decrypt_iv=D,
    encrypt_iv=E,
    history=H,
    good=G,
    late=La,
    lost=Lo,
    resync=R
  },
  {ok, NewKey, Decrypted}.

encrypt(Key, Msg) ->
  {ok, {K, D, E, H, {G, La, Lo, R}}, Encrypted} = ocb128_crypto_serv:encrypt(
    {
      Key#key.key,
      Key#key.decrypt_iv,
      Key#key.encrypt_iv,
      Key#key.history,
      {
        Key#key.good,
        Key#key.late,
        Key#key.lost,
        Key#key.resync
      }
    },
    Msg
  ),
  NewKey = Key#key{
    key=K,
    decrypt_iv=D,
    encrypt_iv=E,
    history=H,
    good=G,
    late=La,
    lost=Lo,
    resync=R
  },
  {ok, NewKey, Encrypted}.
