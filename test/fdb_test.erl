-module (fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 100).
-define (FDB_CLUSTER_FILE, "fixtures/").
-define (A_KEY,"Hello").
-define (A_VALUE, "World").

-define (SO_FOLDER, "../priv").
-define (SO_FILE, "test").

%too_large_api_version_test() ->
%	{error,_Reason} = fdb:api_version(9999999).

%% This can only be called once
%correct_api_version_test() ->
%	ok = fdb:api_version(?FDB_API_VERSION).
fdb_double_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  ?assertEqual({ok,2}, fdb:double(FDB, 1)),
  fdb:stop(FDB).

fdb_add_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  ?assertEqual({ok,3}, fdb:add(FDB, 1,2)),
  fdb:stop(FDB).
  


%%open_a_db_test() ->
%%	{ok, _Fdb} = fdb:open().

%%open_a_transaction_test() ->
%%	{ok, Fdb} = fdb:open(),
%%	{ok, _Transaction} = fdb:transaction(Fdb).
%%

