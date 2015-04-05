-module(ocb128_crypto_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

test_crypto_setup() ->
  application:start(ocb128_crypto).

test_crypto_teardown(_) ->
  application:stop(ocb128_crypto).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

crypto_OCB_AES_128_0_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    Plaintext = iolist_to_binary([]),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_0B_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    Plaintext = iolist_to_binary(""),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_3B_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    Plaintext = hexstr_to_bin("000102"),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_16B_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    Plaintext = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_20B_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    Plaintext = hexstr_to_bin("000102030405060708090a0b0c0d0e0f10111213"),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_string_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Key = hexstr_to_bin("000102030405060708090a0b0c0d0e0f"),
    Plaintext = iolist_to_binary("Some random text"),
    test_crypto(Key, Plaintext)
  end
}.

crypto_OCB_AES_128_missing_one_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    Plaintext = hexstr_to_bin("000102030405060708090a0b0c0d0e0f10111213"),
    K1 = ocb128_crypto:generate_key(),
    EN = ocb128_crypto:encrypt_iv(K1),
    DK1 = ocb128_crypto:resync(K1, EN),
    EK1 = K1,
    {ok,EK2,_E1} = ocb128_crypto:encrypt(EK1, Plaintext),
    {ok,_EK3,E2} = ocb128_crypto:encrypt(EK2, Plaintext),
    {ok,DK2,D1} = ocb128_crypto:decrypt(DK1, E2),
    Good = ocb128_crypto:good(DK2),
    Late = ocb128_crypto:late(DK2),
    Lost = ocb128_crypto:lost(DK2),
    [
      ?_assertEqual(Plaintext, D1),
      ?_assertEqual(1, Good),
      ?_assertEqual(0, Late),
      ?_assertEqual(1, Lost)
    ]
  end
}.

crypto_test_() ->
{
  setup,
  fun test_crypto_setup/0,
  fun test_crypto_teardown/1,
  fun(_) ->
    ?_assertEqual(true, proper:quickcheck(prop_crypto(), [{to_file, user}]))
  end
}.

test_crypto(Key, Plaintext) ->
  K1 = ocb128_crypto:generate_key(),
  K2 = setelement(2, K1, Key),
  N = ocb128_crypto:encrypt_iv(K2),
  K3 = ocb128_crypto:resync(K2, N),
  {ok, _ ,Encrypted} = ocb128_crypto:encrypt(K3, Plaintext),
  {ok, _, Decrypted} = ocb128_crypto:decrypt(K3, Encrypted),
  [?_assertEqual(Plaintext, Decrypted)].

prop_crypto() ->
  ?FORALL({Key, Plain}, {binary(16), iodata()},
  begin
    K1 = ocb128_crypto:generate_key(),
    K2 = setelement(2, K1, Key),
    N = ocb128_crypto:encrypt_iv(K2),
    K3 = ocb128_crypto:resync(K2, N),
    {ok, _, Encrypted} = ocb128_crypto:encrypt(K3, Plain),
    {ok, _, Decrypted} = ocb128_crypto:decrypt(K3, Encrypted),
    Decrypted == iolist_to_binary(Plain)
  end).
