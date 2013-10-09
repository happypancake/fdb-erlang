-module (fdb_core_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 21).

-define (A_KEY,<<"Hello">>).
-define (ANOTHER_KEY,<<"Hello2">>).
-define (A_VALUE, <<"World">>).
-define (A_LARGE_VALUE,repeat_value(?A_VALUE,128)).

-define (SO_FOLDER, "../priv").
-define (SO_FILE, "test").

api_version_test_skip() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  ?assertEqual({error,"api_version_invalid"},fdb_core:api_version(FDB,999)),
  ?assertEqual(ok, fdb_core:api_version(FDB,?FDB_API_VERSION)),
  ?assertEqual({error,"api_version_already_set"},
               fdb_core:api_version(FDB,?FDB_API_VERSION)),
  ok=fdb_core:stop(FDB).

setup_network_test_skip() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  ?assertEqual(ok,fdb_core:setup_network(FDB)),
  ?assertEqual({error,"network_already_setup"},fdb_core:setup_network(FDB)),
  ok=fdb_core:stop(FDB).

run_network_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  ?assertEqual(ok,fdb_core:run_network(FDB)),
  ?assertEqual({error,"network_already_running"},fdb_core:run_network(FDB)),
  ok=fdb_core:stop(FDB).

cluster_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = {FDB,cl,_} = fdb_core:create_cluster(FDB),
  ?assertEqual(ok,fdb_core:cluster_destroy(Cluster)),
  ok=fdb_core:stop(FDB).

database_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  Database = {FDB,db,_} = fdb_core:cluster_create_database(Cluster), 
  ?assertEqual(ok,fdb_core:database_destroy(Database)),
  ?assertEqual(ok,fdb_core:cluster_destroy(Cluster)),
  ok=fdb_core:stop(FDB).

transaction_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  Database = fdb_core:cluster_create_database(Cluster), 
  Transaction = {FDB,tx,_} = fdb_core:database_create_transaction(Database),
  ?assertEqual(ok,fdb_core:transaction_destroy(Transaction)),
  ?assertEqual(ok,fdb_core:database_destroy(Database)),
  ?assertEqual(ok,fdb_core:cluster_destroy(Cluster)),
  ok=fdb_core:stop(FDB).

basic_storage_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  Database = fdb_core:cluster_create_database(Cluster), 
  Transaction = fdb_core:database_create_transaction(Database),
  fdb_core:transaction_clear(Transaction,?A_KEY),
  ?assertEqual(not_found,fdb_core:transaction_get(Transaction,?A_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_set(Transaction,?A_KEY,?A_VALUE)),
  ?assertEqual(?A_VALUE,fdb_core:transaction_get(Transaction,?A_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_clear(Transaction,?A_KEY)),
  ?assertEqual(not_found,fdb_core:transaction_get(Transaction,?A_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_destroy(Transaction)),
  ?assertEqual(ok,fdb_core:database_destroy(Database)),
  ?assertEqual(ok,fdb_core:cluster_destroy(Cluster)),
  ok=fdb_core:stop(FDB).

larger_value_storage_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  Database = fdb_core:cluster_create_database(Cluster), 
  Transaction = fdb_core:database_create_transaction(Database),
  fdb_core:transaction_clear(Transaction,?A_KEY),
  fdb_core:transaction_set(Transaction,?A_KEY,?A_LARGE_VALUE),
  ?assertEqual(?A_LARGE_VALUE,fdb_core:transaction_get(Transaction,?A_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_destroy(Transaction)),
  ?assertEqual(ok,fdb_core:database_destroy(Database)),
  ?assertEqual(ok,fdb_core:cluster_destroy(Cluster)),
  ok=fdb_core:stop(FDB).

commit_test() ->
  {ok,FDB} = fdb_core:start_link(?SO_FOLDER, ?SO_FILE),
  fdb_core:api_version(FDB,?FDB_API_VERSION),
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  Database = fdb_core:cluster_create_database(Cluster), 
  Transaction = fdb_core:database_create_transaction(Database),
  fdb_core:transaction_clear(Transaction,?ANOTHER_KEY),
  fdb_core:transaction_commit(Transaction), 
  ?assertEqual(ok,fdb_core:transaction_set(Transaction,?ANOTHER_KEY,?A_VALUE)),
  ?assertEqual(ok,fdb_core:transaction_commit(Transaction)),
  ?assertEqual(ok,fdb_core:transaction_destroy(Transaction)),
  AnotherTransaction = fdb_core:database_create_transaction(Database), 
  ?assertEqual(?A_VALUE, fdb_core:transaction_get(AnotherTransaction,?ANOTHER_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_clear(AnotherTransaction,?ANOTHER_KEY)),
  ?assertEqual(ok,fdb_core:transaction_commit(AnotherTransaction)),
  ?assertEqual(ok,fdb_core:transaction_destroy(AnotherTransaction)),
  Tx3 = fdb_core:database_create_transaction(Database),
  ?assertEqual(not_found,fdb_core:transaction_get(Tx3,?ANOTHER_KEY,not_found)),
  ?assertEqual(ok,fdb_core:transaction_destroy(Tx3)),
  ?assertEqual(ok,fdb_core:database_destroy(Database)),
  ok=fdb_core:stop(FDB).

repeat_value(Bin,N) ->
  repeat_value(Bin,N,<<>>).

repeat_value(_Bin,0,Acc) ->
  Acc;
repeat_value(Bin,N,Acc) ->
  repeat_value(Bin,N-1,<<Acc/binary,Bin/binary>>).


