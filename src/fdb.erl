-module(fdb).
-export([start_link/0, stop/1]).
-export([add/3, double/2]).

% Path and name of the driver.
-define(DRV_PATH, "../priv").
-define(DRV_NAME, "fdbdrv.so").

% Commands to be executed by the driver.
-define(CMD_ADD, 16#01).
-define(CMD_DOUBLE, 16#02).

%% ----------------------------------------------------------------------------
%% Public functions
%% ----------------------------------------------------------------------------

% Load the specified driver and start the associated generic server. The pid of
% the generic server managing the driver is returned.
-spec start_link()
  -> { ok, pid() } | { error, any() }.
start_link() ->
  gen_driver:start_link(?DRV_PATH, ?DRV_NAME).

% As the generic server for drivers may not be part of a supervision tree, we
% provide a simple method to stop it.
-spec stop(Pid :: pid())
  -> ok.
stop(Pid) ->
  gen_server:cast(Pid, stop).

add(Pid, val1, val2) ->
  gen_server:call(Pid, { port, ?CMD_ADD, val1, val2 }).

double(Pid,val) ->
  gen_server:call(Pid, { port, ?CMD_DOUBLE, val }).

