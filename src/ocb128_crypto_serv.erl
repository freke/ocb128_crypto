-module(ocb128_crypto_serv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, decrypt/2, encrypt/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec encrypt(ocb128_crypto:key(), iolist()) ->
  {ok, ocb128_crypto:key(), iolist()}.
encrypt(Key, Msg) ->
  gen_server:call(?SERVER, {encrypt, Key, Msg}).

-spec decrypt(ocb128_crypto:key(), iolist()) ->
  {ok, ocb128_crypto:key(), iolist()}.
decrypt(Key, Msg) ->
  gen_server:call(?SERVER, {decrypt, Key, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  ocb128_crypto_port:init(),
  {ok, Args}.

handle_call({encrypt, Key, Msg}, _From, State) ->
  {Encrypted, NewKey} = ocb128_crypto_port:encrypt(Key, iolist_to_binary(Msg)),
  {reply, {ok, NewKey, Encrypted}, State};
handle_call({decrypt, Key, Msg}, _From, State) ->
  {Decrypted, NewKey} = ocb128_crypto_port:decrypt(Key, iolist_to_binary(Msg)),
  {reply, {ok, NewKey, Decrypted}, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
