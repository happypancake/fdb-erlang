-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/fdb.hrl").

-define(SOLIB,"../priv/fdb_nif").

hello_world_test() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  {ok, DB} = fdb:open(),
  AKey = <<"Hello">>,
  AValue = <<"World">>,
  ok = fdb:set(DB, AKey, AValue),
  AValue = fdb:get(DB, AKey),
  ok = fdb:clear(DB, AKey),
  ?assertEqual(not_found, fdb:get(DB, AKey)).

transaction_test() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  {ok, DB} = fdb:open(),
  AKey = <<"abc">>,
  AValue = <<"xyz">>,
  ok = fdb:clear(DB, AKey),
  not_found = fdb:get(DB, AKey),
  fdb:transact(DB, fun(Tx)->
        fdb:set(Tx, AKey, AValue)
    end),
  ?assertEqual(AValue, fdb:get(DB, AKey)),
  ok = fdb:clear(DB, AKey).

range_test() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  {ok, DB} = fdb:open(),
  [ok = fdb:set(DB, I, I) || I <- lists:seq(1, 4)],
  ?assertEqual([{2, 2}, {3, 3},{4, 4}], fdb:get(DB, #select{gte = 2, lte =4})),
  ?assertEqual([{2, 2}, {3, 3}], fdb:get(DB, #select{gte = 2, lte =3})),
  ?assertEqual([{2, 2}, {3, 3}, {4, 4}], fdb:get(DB, #select{gte = 2})),
  ?assertEqual([{3, 3}, {4, 4}], fdb:get(DB, #select{ gt = 2})),
  ?assertEqual([{1, 1}, {2, 2}], fdb:get(DB, #select{ lt = 3})),
  ?assertEqual([{1, 1}, {2, 2}, {3, 3}], fdb:get(DB, #select{ lte = 3})).
