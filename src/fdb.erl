-module(fdb).
-export([start_link/2, stop/1]).
-export([api_version/2,setup_network/1,run_network/1]).
-export([create_cluster/2,destroy_cluster/2]).


% Commands to be executed by the driver.
-define(CMD_API_VERSION, 16#03).
-define(CMD_SETUP_NETWORK, 16#04).
-define(CMD_RUN_NETWORK, 16#05).
-define(CMD_CREATE_CLUSTER, 16#06).
-define(CMD_DESTROY_CLUSTER, 16#07).

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

create_cluster(Pid,ClusterFile) ->
  gen_server:call(Pid, {port, ?CMD_CREATE_CLUSTER, ClusterFile}).

destroy_cluster(Pid, ClusterId) ->
  gen_server:call(Pid, {port, ?CMD_DESTROY_CLUSTER, ClusterId}).

