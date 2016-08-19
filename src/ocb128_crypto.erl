%%% @author David AAberg <davabe@hotmail.com>

-module(ocb128_crypto).

-export([generate_key/0]).
-export([key/1, encrypt_iv/1, decrypt_iv/1, resync/2]).
-export([good/1, late/1, lost/1]).
-export([encrypt/2, decrypt/2]).

-record(key,
  {
    key :: <<_:16>>,
    decrypt_iv :: <<_:16>>,
    encrypt_iv :: <<_:16>>,
    history :: binary(),
    good :: integer(),
    late :: integer(),
    lost :: integer(),
    resync :: integer()
  }).

-opaque key() :: #key{}.
-export_type([key/0]).

-spec generate_key() -> key().
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

-spec key(key()) -> <<_:16>>.
key(Key) ->
  Key#key.key.

-spec encrypt_iv(key()) -> <<_:16>>.
encrypt_iv(Key) ->
  Key#key.encrypt_iv.

-spec decrypt_iv(key()) -> <<_:16>>.
decrypt_iv(Key) ->
  Key#key.decrypt_iv.

-spec resync(key(), <<_:16>>) -> key().
resync(Key, Div) ->
  Key#key{decrypt_iv=Div, resync=Key#key.resync+1}.

-spec good(key()) -> integer().
good(Key) ->
  Key#key.good.

-spec late(key()) -> integer().
late(Key) ->
  Key#key.late.

-spec lost(key()) -> integer().
lost(Key) ->
  Key#key.lost.

-spec decrypt(key(), binary()) -> {ok, key(), binary()}.
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

-spec encrypt(key(), binary()) -> {ok, key(), binary()}.
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
