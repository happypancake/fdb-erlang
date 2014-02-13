-record(select, {
   lt = nil, 
   lte = nil, 
   offset_begin = 0, 
   gte = nil, 
   gt = nil, 
   offset_end = 0,
   limit = 0,
   target_bytes = 0,
   streaming_mode = iterator,
   iteration = 1,
   is_snapshot = false,
   is_reverse = false}).

-record(iterator, {
   tx,
   iteration,
   data = [],
   select,
   out_more = true}).

-type fdb_version() :: pos_integer().
-type fdb_errorcode() :: pos_integer().
-type fdb_cmd_result() :: ok | {error, fdb_errorcode()}|{error,nif_not_loaded}.
%-type fdb_qry_result() :: {ok, term()} | {error, fdb_errorcode()}.
-type fdb_database() :: {db, term()}.
-type fdb_transaction() :: {tx, term()}.
-type fdb_handle() :: fdb_database() | fdb_transaction().
-type fdb_key() :: term().


