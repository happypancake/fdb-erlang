-module(fdb).
-export([start_link/2, stop/1]).
-export([add/3, double/2]).

% Commands to be executed by the driver.
-define(CMD_ADD, 16#01).
-define(CMD_DOUBLE, 16#02).

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

add(Pid, Val1, Val2) ->
  gen_server:call(Pid, { port, ?CMD_ADD, Val1, Val2 }).

double(Pid, Val) ->
  gen_server:call(Pid, { port, ?CMD_DOUBLE, Val }).

