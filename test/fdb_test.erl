-module(fdb_test).

-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
  DB = fdb:open(),
  AKey = {hello,"World",[15,4]},
  AValue = ['something',odd,<<"but">>,$i,"t works"],
  ok = fdb:set(DB,AKey,AValue),
  AValue = fdb:get(DB,AKey),
  ok = fdb:clear(DB,AKey),
  not_found = fdb:get(DB,AKey),
  ok = fdb:close(DB).

atomic_test() ->
  DB = fdb:open(),
  AKey = abc,
  AValue = xyz,
  not_found = fdb:get(DB,AKey),
  fdb:atomic(DB,fun(Tx)->
        fdb:set(Tx,AKey,AValue),
        _ = 1/0
    end),
  not_found = fdb:get(DB,AKey),
  fdb:close(DB).



