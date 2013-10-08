-module (fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 21).

-define (A_KEY,<<"Hello">>).
-define (A_VALUE, <<"World">>).

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
  Cluster = {FDB,cl,_} = fdb:create_cluster(FDB),
  ?assertEqual(ok,fdb:cluster_destroy(Cluster)).

database_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:setup_network(FDB),
  fdb:run_network(FDB),
  Cluster = fdb:create_cluster(FDB),
  Database = {FDB,db,_} = fdb:cluster_create_database(Cluster), 
  ?assertEqual(ok,fdb:database_destroy(Database)),
  ?assertEqual(ok,fdb:cluster_destroy(Cluster)).

transaction_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:setup_network(FDB),
  fdb:run_network(FDB),
  Cluster = fdb:create_cluster(FDB),
  Database = {FDB,db,_} = fdb:cluster_create_database(Cluster), 
  Transaction = {FDB,tx,_} = fdb:database_create_transaction(Database),
  ?assertEqual(ok,fdb:transaction_destroy(Transaction)),
  ?assertEqual(ok,fdb:database_destroy(Database)),
  ?assertEqual(ok,fdb:cluster_destroy(Cluster)).

basic_storage_test() ->
  {ok,FDB} = fdb:start_link(?SO_FOLDER, ?SO_FILE),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:setup_network(FDB),
  fdb:run_network(FDB),
  Cluster = fdb:create_cluster(FDB),
  Database = fdb:cluster_create_database(Cluster), 
  Transaction = fdb:database_create_transaction(Database),
  ?assertEqual(not_found,fdb:transaction_get(Transaction,?A_KEY,not_found)),
  fdb:transaction_set(Transaction,?A_KEY,?A_VALUE),
  ?assertEqual(?A_VALUE,fdb:transaction_get(Transaction,?A_KEY,not_found)),
  ?assertEqual(ok,fdb:transaction_destroy(Transaction)),
  ?assertEqual(ok,fdb:database_destroy(Database)),
  ?assertEqual(ok,fdb:cluster_destroy(Cluster)).



