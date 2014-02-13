-module(gen_fdb).

-include("../include/fdb.hrl").

-callback get(fdb_handle(), fdb_key()|#select{}) -> (term() | not_found | [term()]).
-callback get_range(fdb_handle(), fdb_key(),fdb_key()) -> ([term()]|{error,nif_not_loaded}).
-callback get(fdb_handle(), fdb_key(), term()) -> term().
-callback set(fdb_handle(), fdb_key(), term()) -> fdb_cmd_result().
-callback clear(fdb_handle(), fdb_key()) -> fdb_cmd_result().
-callback clear_range(fdb_handle(), fdb_key(), fdb_key()) -> fdb_cmd_result().
-callback transact(fdb_database(), fun((fdb_transaction())->term())) -> term().

