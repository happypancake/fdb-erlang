-module (fdb_test).

-include_lib("eunit/include/eunit.hrl").

-define (FDB_API_VERSION, 100).
-define (FDB_CLUSTER_FILE, "fixtures/").
-define (A_KEY,"Hello").
-define (A_VALUE, "World").

%too_large_api_version_test() ->
%	{error,_Reason} = fdb:api_version(9999999).

%% This can only be called once
%correct_api_version_test() ->
%	ok = fdb:api_version(?FDB_API_VERSION).

open_a_db_test() ->
	{ok, _Fdb} = fdb:open().

open_a_transaction_test() ->
	{ok, Fdb} = fdb:open(),
	{ok, _Transaction} = fdb:transaction(Fdb).
