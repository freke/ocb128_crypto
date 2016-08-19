%%% @author David AAberg <davabe@hotmail.com>
%%% @private

-module(ocb128_crypto_port).

-export([init/0, decrypt/2, encrypt/2]).

-spec init() -> any().
init() ->
  SoName = case code:priv_dir(ocb128_crypto) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
          filename:join(["..", "priv", "ocb128_crypto"]);
        false ->
          filename:join(["priv", "ocb128_crypto"])
      end;
    Dir ->
      filename:join(Dir, "ocb128_crypto")
    end,
  (catch erlang:load_nif(SoName, 0)).

-spec encrypt(ocb128_crypto:key(), binary()) ->
  {
    binary(),
    ocb128_crypto:key()
  }.
encrypt(_Key, _Source) ->
  exit(ocb128_crypto_nif_not_loaded).

-spec decrypt(ocb128_crypto:key(), binary()) ->
                {binary() | error, ocb128_crypto:key()}.
decrypt(_Key, _Source) ->
  exit(ocb128_crypto_nif_not_loaded).
