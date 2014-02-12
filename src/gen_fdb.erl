-module(gen_fdb).

-include("../include/fdb.hrl").

-callback init(SoFile::list())-> ok | {error, term()}.
-callback init()-> ok | {error, term()}.
-callback api_version(fdb_version()) -> fdb_cmd_result().
-callback open() -> fdb_database().
-callback init_and_open() -> fdb_database().
-callback init_and_open(SoFile::list()) -> fdb_database().
-callback get(fdb_handle(), fdb_key()|#select{}) -> (term() | not_found | [term()]).
-callback get_range(fdb_handle(), fdb_key(),fdb_key()) -> ([term()]).
-callback get(fdb_handle(), fdb_key(), term()) -> term().
-callback bind(fdb_handle(), #select{}) -> #iterator{}.
-callback next(#iterator{}) -> (#iterator{} | done).
-callback set(fdb_handle(), fdb_key(), term()) -> fdb_cmd_result().
-callback clear(fdb_handle(), fdb_key()) -> fdb_cmd_result().
-callback clear_range(fdb_handle(), fdb_key(), fdb_key()) -> fdb_cmd_result().
-callback transact(fdb_database(), fun((fdb_transaction())->term())) -> term().

