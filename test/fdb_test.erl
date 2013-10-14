-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define(SOLIB,"../priv/fdb_nif").

hello_world_test() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  DB = fdb:open(),
  AKey = <<"Hello">>,
  AValue = <<"World">>,
  ok = fdb:set(DB,AKey,AValue),
  AValue = fdb:get(DB,AKey),
  ok = fdb:clear(DB,AKey),
  not_found = fdb:get(DB,AKey).

atomic_test_todo() ->
  fdb:init(?SOLIB),
  fdb:api_version(100),
  DB = fdb:open(),
  AKey = <<"abc">>,
  AValue = <<"xyz">>,
  not_found = fdb:get(DB,AKey),
  fdb:atomic(DB,fun(Tx)->
        fdb:set(Tx,AKey,AValue)
    end),
  not_found = fdb:get(DB,AKey).

