%%% @author David AAberg <davabe@hotmail.com>
%%% @copyright (C) 2013,
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2013 by  <davabe@hotmail.com>

-module(ocb128_crypto_port).

-export([init/0, decrypt/2, encrypt/2]).

%-on_load({init, 0}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec init() -> any().
init() ->
  SoName = case code:priv_dir(ocb128_crypto) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
          filename:join(["..", "priv", "ocb128_crypto_port_nif"]);
        false ->
          filename:join(["priv", "ocb128_crypto_port_nif"])
      end;
    Dir ->
      filename:join(Dir, "ocb128_crypto_port_nif")
    end,
  (catch erlang:load_nif(SoName, 0)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec encrypt(ocb128_crypto_serv:key(), binary()) ->
  {
    binary(),
    ocb128_crypto:key()
  }.
encrypt(_Key, _Source) ->
  exit(ocb128_crypto_nif_not_loaded).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec decrypt(ocb128_crypto_serv:key(), binary()) ->
                {binary() | error, ocb128_crypto:key()}.
decrypt(_Key, _Source) ->
  exit(ocb128_crypto_nif_not_loaded).
