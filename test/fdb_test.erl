-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(SOLIB,"../priv/fdb_nif").

hello_world_test() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  DBR = fdb:open(),
  io:format("DBR ~p~n",[DBR]),
  {ok, DB} = DBR, 
  AKey = <<"Hello">>,
  AValue = <<"World">>,
  ok = fdb:set(DB, AKey, AValue),
  AValue = fdb:get(DB, AKey),
  ok = fdb:clear(DB, AKey),
  not_found = fdb:get(DB, AKey).

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
  ?assertEqual(AValue, fdb:get(DB, AKey)).

