-module(fdb).
-export([init/0,init/1]).
-export([api_version/1,open/0]).
-export([get/2,get/3,set/3]).
-export([clear/2]).
-export([transact/2]).

-define (FDB_API_VERSION, 21).

init(SoFile) -> fdb_nif:init(SoFile).

init() ->
  init("priv/fdb_nif").

api_version(Version) when Version>0 ->
  fdb_nif:fdb_select_api_version(Version).

open() ->
  fdb_nif:fdb_setup_network(),
  fdb_nif:fdb_run_network(),
  F1 = fdb_nif:fdb_create_cluster(),
  0 = fdb_nif:fdb_future_block_until_ready(F1),
  0 = fdb_nif:fdb_future_get_error(F1),
  {0, Cluster} = fdb_nif:fdb_future_get_cluster(F1),
  F2 = fdb_nif:fdb_cluster_create_database(Cluster),
  0 = fdb_nif:fdb_future_block_until_ready(F2),
  0 = fdb_nif:fdb_future_get_error(F2),
  {0, Database} = fdb_nif:fdb_future_get_database(F2),
  {db, Database}.

get(Context, Key) ->
   get(Context, Key, not_found).

get({db, Database}, Key, DefaultValue) ->
  {0, Tx} = fdb_nif:fdb_database_create_transaction(Database),  
  get({tx, Tx}, Key, DefaultValue);
get({tx, Tx}, Key, DefaultValue) ->
    F1 = fdb_nif:fdb_transaction_get(Tx, Key),
    0 = fdb_nif:fdb_future_block_until_ready(F1),
    0 = fdb_nif:fdb_future_get_error(F1),
    {0, Result} = fdb_nif:fdb_future_get_value(F1),
  case Result of
    not_found -> DefaultValue;
    _ -> Result
  end.

set({db, Database}, Key, Value) ->
  transact({db, Database}, fun (Tx)-> set(Tx, Key, Value) end);
set({tx, Tx}, Key, Value) ->
  fdb_nif:fdb_transaction_set(Tx, Key, Value).

clear({db, Database}, Key) ->
  transact({db, Database}, fun (Tx)-> clear(Tx, Key) end);
clear({tx, Tx}, Key) ->
  fdb_nif:fdb_transaction_clear(Tx, Key).

transact({db, Database}, DoStuff) ->
  Result = try_transact_operation(Database, DoStuff),
  process_commit_result(Result, Database, DoStuff).

try_transact_operation(Database, DoStuff) ->
  {0, Tx} = fdb_nif:fdb_database_create_transaction(Database),
  Result = DoStuff({tx, Tx}),
  F1 = fdb_nif:fdb_transaction_commit(Tx),
  0 = fdb_nif:fdb_future_block_until_ready(F1),
  Err = fdb_nif:fdb_future_get_error(F1),
  {Err, Result, Tx}.

process_commit_result({0, Result, _Tx}, _Database, _DoStuff) ->
  Result;
process_commit_result({Err, _Result, Tx}, Database, DoStuff) ->
  F1 = fdb_nif:fdb_transaction_on_error(Tx, Err),
  0 = fdb_nif:fdb_future_block_until_ready(F1),
  ErrResult = fdb_nif:fdb_future_get_error(F1),
  retry_transact_op(ErrResult, Database, DoStuff).

retry_transact_op(0, Database, DoStuff) ->
  transact({db, Database}, DoStuff);
retry_transact_op(Error, _Database, _DoStuff) ->
  Error.

