-module (fdb_nif_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 21).

-define (A_KEY,<<"Hello">>).
-define (ANOTHER_KEY,<<"Hello2">>).
-define (A_VALUE, <<"World">>).
-define (A_LARGE_VALUE,repeat_value(?A_VALUE,128)).

-define (SO_FILE, "../priv/fdb_nif").

store_and_retrieve_test() ->
  AKey = "abc",
  
  ?assertEqual(ok,fdb_nif:init(?SO_FILE)),
  fdb_nif:fdb_setup_network(),
  fdb_nif:fdb_run_network(),

  F1 = fdb_nif:fdb_create_cluster(),
  ok = fdb_nif:fdb_future_block_until_ready(F1),
  {0, Cluster} = fdb_nif:fdb_future_get_cluster(F1),

  F2 = fdb_nif:fdb_cluster_create_database(Cluster),
  ok = fdb_nif:fdb_future_block_until_ready(F2),
  {0, Database} = fdb_nif:fdb_future_get_database(F2),

  {0, Transaction} = fdb_nif:fdb_database_create_transaction(Database),

  F3 = fdb_nif:fdb_transaction_get(Transaction,AKey),
  ok = fdb_nif:fdb_future_block_until_ready(F3),
  {0, not_found} = fdb_nif:fdb_future_get_value(F3).
