-module(fdb_nif).
-compile([export_all]).

init() ->
  try_load(["./fdb_drv","./priv/fdb_drv","../priv/fdb_drv"],nok).
  
init(NifFile) ->
  erlang:load_nif(NifFile,0).
  
try_load([],LastErr) ->
  LastErr;
try_load([_],ok) -> 
  ok;
try_load([H|T],_) ->
  try_load(T,init(H)).

fdb_get_max_api_version(_)  ->
  nif_not_loaded.

fdb_run_network(_)  ->
  nif_not_loaded.

fdb_setup_network(_)  ->
  nif_not_loaded.

fdb_stop_network(_)  ->
  nif_not_loaded.
  
fdb_transaction_add_conflict_range(_,_,_,_,_,_) ->
  nif_not_loaded.
  
fdb_transaction_cancel(_) ->
  nif_not_loaded.
  
fdb_transaction_destroy(_) ->
  nif_not_loaded.
  
fdb_cluster_configure_database() -> 
  nif_not_loaded.

fdb_cluster_create_database(_c, _db_name, _db_name_length) ->
  nif_not_loaded.

fdb_cluster_destroy(_c) ->
  nif_not_loaded.

fdb_cluster_set_option(_c, _option, _value, _value_length) ->
  nif_not_loaded.
  
fdb_transaction_get_addresses_for_key(_,_,_) ->  
  nif_not_loaded.

fdb_transaction_watch(_,_,_) ->  
  nif_not_loaded.

fdb_create_cluster(_cluster_file_path) ->
  nif_not_loaded.

fdb_database_create_transaction(_d, _out_transaction) ->
  nif_not_loaded.

fdb_database_destroy(_d) ->
  nif_not_loaded.

fdb_database_set_option(_d, _option, _value, _value_length) ->
  nif_not_loaded.

fdb_future_block_until_ready(_f) ->
  nif_not_loaded.

fdb_future_cancel(_f) ->
  nif_not_loaded.

fdb_future_destroy(_f) ->
  nif_not_loaded.

fdb_future_get_cluster(_f, _out_cluster) ->
  nif_not_loaded.

fdb_future_get_database(_f, _out_database) ->
  nif_not_loaded.

fdb_future_get_error(_f) ->
  nif_not_loaded.

fdb_future_get_error_impl() -> 

  nif_not_loaded.

fdb_future_get_error_v22() -> 

  nif_not_loaded.

fdb_future_get_key(_f, _out_key, _out_key_length) ->
  nif_not_loaded.

fdb_future_get_keyvalue_array(_f, _out_kv, _out_count, _out_more) ->
  nif_not_loaded.

fdb_future_get_string_array(_f, _out_strings, _out_count) ->
  nif_not_loaded.

fdb_future_get_value(_f, _out_present, _out_value, _out_value_length) ->
  nif_not_loaded.

fdb_future_get_version(_f, _out_version) ->
  nif_not_loaded.

fdb_future_is_error(_f) ->
  nif_not_loaded.

fdb_future_is_error_v22() -> 

  nif_not_loaded.

fdb_future_is_ready(_f) ->
  nif_not_loaded.

fdb_future_release_memory(_f) ->
  nif_not_loaded.

fdb_future_set_callback(_f, _callback, _callback_parameter) ->
  nif_not_loaded.

fdb_get_error(_code) ->
  nif_not_loaded.


fdb_network_set_option(_option, _value, _value_length) ->
  nif_not_loaded.

fdb_select_api_version(_runtime_version) ->
  nif_not_loaded.

fdb_transaction_add_conflict_range(_tr, _begin_key_name, _begin_key_name_length, _end_key_name, _end_key_name_length) ->
  nif_not_loaded.

fdb_transaction_atomic_op(_tr, _key_name, _key_name_length, _param, _param_length, _operation_type) ->
  nif_not_loaded.

fdb_transaction_clear(_tr, _key_name, _key_name_length) ->
  nif_not_loaded.

fdb_transaction_clear_range(_tr, _begin_key_name, _begin_key_name_length, _end_key_name, _end_key_name_length) ->
  nif_not_loaded.

fdb_transaction_commit(_tr) ->
  nif_not_loaded.

fdb_transaction_get(_tr) ->
  nif_not_loaded.

fdb_transaction_get_addresses_for_key(_tr, _key_name) ->
  nif_not_loaded.

fdb_transaction_get_committed_version(_tr, _out_version) ->
  nif_not_loaded.

fdb_transaction_get_key(_tr, _key_name, _key_name_length, _or_equal, _offset, _snapshot) ->
  nif_not_loaded.

fdb_transaction_get_range(_tr, _begin_key_name, _begin_key_name_length, _begin_or_equal, _begin_offset, _end_key_name, _end_key_name_length, _end_or_equal, _end_offset, _limit, _target_bytes, _mode, _iteration, _snapshot, _reverse) ->
  nif_not_loaded.

fdb_transaction_get_range_selector(_tr, _begin_key_name, _begin_key_name_length, _begin_or_equal, _begin_offset, _end_key_name, _end_key_name_length, _end_or_equal, _end_offset, _limit) ->
  nif_not_loaded.

fdb_transaction_get_read_version(_tr) ->
  nif_not_loaded.

fdb_transaction_on_error(_tr, _error) ->
  nif_not_loaded.

fdb_transaction_reset(_tr) ->
  nif_not_loaded.

fdb_transaction_set(_tr, _option, _value, _value_length) ->
  nif_not_loaded.

fdb_transaction_set_option(_tr, _option, _value, _value_length) ->
  nif_not_loaded.

fdb_transaction_set_read_version(_tr, _version) ->
  nif_not_loaded.

fdb_transaction_watch(_tr, _key_name) ->
  nif_not_loaded.

