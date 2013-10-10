-module(fdb).
-export([start_link/2]).
-export([api_version/2,open/0,open/1]).
-export([get/2,get/3,set/3]).
-export([clear/2,commit/1,close/1]).
-export([atomic/2]).

-define (FDB_API_VERSION, 21).

start_link(SoFolder,SoPath) ->
  fdb_core:start_link(SoFolder,SoPath).

api_version(FDB,Version) when Version>0 ->
  fdb_core:api_version(FDB,Version).

open() ->
  {ok,FDB} = try_start_link([".","priv","../priv"]),
  fdb:api_version(FDB,?FDB_API_VERSION),
  fdb:open(FDB).

try_start_link([]) ->
  {error,"unable_to_load_so_file"};
try_start_link([Folder|T]) ->
    Result = fdb:start_link(Folder,"test"),
    case Result of
      {ok,_Pid} -> Result;
      {error,{open_error,-10}} -> try_start_link(T); 
      {error,bad_driver_name} -> try_start_link(T) 
    end.

open(FDB) ->
  fdb_core:setup_network(FDB),
  fdb_core:run_network(FDB),
  Cluster = fdb_core:create_cluster(FDB),
  DB = fdb_core:cluster_create_database(Cluster),
  Tx = fdb_core:database_create_transaction(DB),
  {Tx,{DB,{Cluster}}}.

get(Handle={{_Pid,tx,_TxHandle},_Parent},Key) ->
  get(Handle,Key,not_found).


get({Tx={_Pid,tx,_TxHandle},_Parent},Key,Default) ->
  Result = fdb_core:transaction_get(Tx,term_to_binary(Key),Default),
  case Result of
    Default -> Default;
    {error,_} -> Result;
    _ -> binary_to_term(Result)
  end.

set({Tx={_Pid,tx,_TxHandle},_Parent},Key,Value) ->
  fdb_core:transaction_set(Tx,term_to_binary(Key),term_to_binary(Value)).

clear({Tx={_Pid,tx,_TxHandle},_Parent},Key) ->
  fdb_core:transaction_clear(Tx,term_to_binary(Key)).

commit({Tx={_Pid,tx,_TxHandle},_Parent}) -> 
  commit(Tx);
commit(Tx={_Pid,tx,_TxHandle}) -> 
  fdb_core:transaction_commit(Tx).

close({Tx={_Pid,tx,_TxHandle},{Database,{Cluster}}}) ->
  fdb_core:transaction_commit(Tx),
  fdb_core:transaction_destroy(Tx),
  fdb_core:database_destroy(Database),
  fdb_core:cluster_destroy(Cluster).

atomic({{_Pid,tx,_TxHandle},WR = {Database,{_Cluster}}},SomeFunc) ->
  Tx = fdb_core:database_create_transaction(Database),
  try
    SomeFunc({Tx,WR}),
    fdb_core:transaction_commit(Tx)
  catch
    Exception:Reason -> {error,Exception,Reason}
  after
    fdb_core:transaction_destroy(Tx)
  end.
