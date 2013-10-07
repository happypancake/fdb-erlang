-module (fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 21).
-define (FDB_CLUSTER_FILE, "fixtures/").
-define (A_KEY,"Hello").
-define (A_VALUE, "World").

-define (SO_FOLDER, "../priv").
-define (SO_FILE, "test").

api_version_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  ?assertEqual({error,"api_version_invalid"},fdb:api_version(FDB,999)),
  ?assertEqual(ok, fdb:api_version(FDB,?FDB_API_VERSION)),
  ?assertEqual({error,"api_version_already_set"}, fdb:api_version(FDB,?FDB_API_VERSION)).

setup_network_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  ?assertEqual(ok,fdb:setup_network(FDB)),
  ?assertEqual({error,"network_already_setup"},fdb:setup_network(FDB)).

run_network_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:setup_network(FDB),
  ?assertEqual(ok,fdb:run_network(FDB)),
  ?assertEqual({error,"network_already_running"},fdb:run_network(FDB)).

cluster_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:setup_network(FDB),
  fdb:run_network(FDB),
  _ClusterId = fdb:create_cluster(FDB,?FDB_CLUSTER_FILE).
  %% this crashes
  %fdb:destroy_cluster(FDB,ClusterId).

