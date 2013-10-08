-module(fdb).
-export([start_link/2, stop/1]).
-export([api_version/2,setup_network/1,run_network/1]).
-export([create_cluster/1,create_cluster/2,cluster_destroy/1]).
-export([cluster_create_database/1,database_destroy/1]).
-export([database_create_transaction/1,transaction_destroy/1]).
-export([transaction_get/3,transaction_set/3]).

-define(DEFAULT_HANDLE,<<0,0,0,0,0,0,0,0>>).

% Commands to be executed by the driver.
-define(CMD_API_VERSION, 16#03).
-define(CMD_SETUP_NETWORK, 16#04).
-define(CMD_RUN_NETWORK, 16#05).
-define(CMD_CREATE_CLUSTER, 16#06).
-define(CMD_CLUSTER_DESTROY, 16#07).
-define(CMD_CLUSTER_CREATE_DATABASE, 16#08).
-define(CMD_DATABASE_DESTROY, 16#09).
-define(CMD_DATABASE_CREATE_TRANSACTION,16#0A).
-define(CMD_TRANSACTION_DESTROY,16#0B).
-define(CMD_TRANSACTION_GET, 16#0C).
-define(CMD_TRANSACTION_SET, 16#0D).

%% ----------------------------------------------------------------------------
%% Public functions
%% ----------------------------------------------------------------------------

% Load the specified driver and start the associated generic server. The pid of
% the generic server managing the driver is returned.
start_link(Path, Name) ->
  gen_driver:start_link(Path, Name).

% As the generic server for drivers may not be part of a supervision tree, we
% provide a simple method to stop it.
-spec stop(Pid :: pid())
  -> ok.
stop(Pid) ->
  gen_server:cast(Pid, stop).

api_version(Pid, Version) ->
  gen_server:call(Pid, {port, ?CMD_API_VERSION, Version}).

setup_network(Pid) ->
  gen_server:call(Pid, {port, ?CMD_SETUP_NETWORK}).

run_network(Pid) ->
  gen_server:call(Pid, {port, ?CMD_RUN_NETWORK}).

create_cluster(Pid) ->
  Result = gen_server:call(Pid, {port, ?CMD_CREATE_CLUSTER }),
  case Result of
    {error,_} -> Result;
    _ -> {Pid,cl,Result}
  end.

create_cluster(Pid,ClusterFile) ->
  Result = gen_server:call(Pid, {port, ?CMD_CREATE_CLUSTER, ClusterFile}),
  case Result of
    {error,_} -> Result;
    _ -> {Pid,cl,Result}
  end.

cluster_destroy({Pid, cl,ClusterHandle}) ->
  gen_server:call(Pid, {port, ?CMD_CLUSTER_DESTROY, ClusterHandle}).

cluster_create_database({Pid,cl,ClHandle}) ->
  Result = gen_server:call(Pid, {port, ?CMD_CLUSTER_CREATE_DATABASE, ClHandle}),
  case Result of
    {error,_} -> Result;
    _ -> {Pid,db,Result}
  end.

database_destroy({Pid,db,DBHandle}) ->
  gen_server:call(Pid, {port, ?CMD_DATABASE_DESTROY,DBHandle}).

database_create_transaction({Pid,db,DBHandle}) ->
  Result = gen_server:call(Pid, {port, ?CMD_DATABASE_CREATE_TRANSACTION, DBHandle}),
  case Result of
    {error,_} -> Result;
    _ -> {Pid,tx,Result}
  end.

transaction_destroy({Pid,tx,TxHandle}) ->
  gen_server:call(Pid,{port, ?CMD_TRANSACTION_DESTROY, TxHandle}).

transaction_get({Pid,tx,TxHandle}, Key, Default) ->
  Result = gen_server:call(Pid, {port, ?CMD_TRANSACTION_GET,{TxHandle,Key}}),
  case Result of
    {ok,not_found} -> Default;
    _ -> Result
  end.

transaction_set({Pid, tx,TxHandle}, Key, Value) ->
  gen_server:call(Pid, {port, ?CMD_TRANSACTION_SET,{TxHandle,Key,Value}}).
