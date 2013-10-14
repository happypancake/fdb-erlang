-module (fdb_nif_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 21).

-define (A_KEY,<<"Hello">>).
-define (ANOTHER_KEY,<<"Hello2">>).
-define (A_VALUE, <<"World">>).
-define (A_LARGE_VALUE,repeat_value(?A_VALUE,128)).

-define (SO_FILE, "../priv/fdb_nif").

api_version_test() ->
  ?assertEqual(ok,fdb_nif:init(?SO_FILE)),
  ?assertEqual(api_version_invalid,fdb_nif:fdb_get_error(fdb_nif:fdb_select_api_version(999))),
  ?assertEqual(0, fdb_nif:fdb_select_api_version(?FDB_API_VERSION)),
  ?assertEqual(api_version_already_set,fdb_nif:fdb_get_error(fdb_nif:fdb_select_api_version(?FDB_API_VERSION))).

store_and_retrieve_test() ->
  AKey = "abc",
  AValue = "xyz",
  
  ?assertEqual(ok,fdb_nif:init(?SO_FILE)),
  fdb_nif:fdb_setup_network(),
  fdb_nif:fdb_run_network(),

  F1 = fdb_nif:fdb_create_cluster(),
  0 = fdb_nif:fdb_future_block_until_ready(F1),
  {0, Cluster} = fdb_nif:fdb_future_get_cluster(F1),

  F2 = fdb_nif:fdb_cluster_create_database(Cluster),
  0 = fdb_nif:fdb_future_block_until_ready(F2),
  {0, Database} = fdb_nif:fdb_future_get_database(F2),

  {0, Transaction} = fdb_nif:fdb_database_create_transaction(Database),

  fdb_nif:fdb_transaction_set(Transaction,AKey,AValue),
  
  F3 = fdb_nif:fdb_transaction_get(Transaction,AKey),
  0 = fdb_nif:fdb_future_block_until_ready(F3),
  {0, AValue} = fdb_nif:fdb_future_get_value(F3).







