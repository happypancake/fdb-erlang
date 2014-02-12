-module(fdb_nif).
-compile(export_all).

init(NifFile) ->
  eat_reload(erlang:load_nif(NifFile,0)).

eat_reload({error,{reload,_}}) ->
  ok;
eat_reload(Other) -> 
  Other.
  
fdb_run_network()  ->
  {error, nif_not_loaded}.

fdb_setup_network()  ->
  {error, nif_not_loaded}.

fdb_stop_network()  ->
  {error, nif_not_loaded}.
  
fdb_transaction_add_conflict_range(_,_,_,_,_,_) ->
  {error, nif_not_loaded}.
  
fdb_transaction_cancel(_) ->
  {error, nif_not_loaded}.
  
fdb_transaction_destroy(_) ->
  {error, nif_not_loaded}.
  
fdb_cluster_create_database(_c) ->
  {error, nif_not_loaded}.

fdb_cluster_create_database(_c, _db_name) ->
  {error, nif_not_loaded}.

fdb_cluster_destroy(_c) ->
  {error, nif_not_loaded}.

fdb_cluster_set_option(_c, _option, _value) ->
  {error, nif_not_loaded}.
  
fdb_transaction_get_addresses_for_key(_,_,_) ->  
  {error, nif_not_loaded}.

fdb_create_cluster() ->
  {error, nif_not_loaded}.

fdb_create_cluster(_cluster_file_path) ->
  {error, nif_not_loaded}.

fdb_database_create_transaction(_d) ->
  {error, nif_not_loaded}.

fdb_database_destroy(_d) ->
  {error, nif_not_loaded}.

fdb_database_set_option(_d, _option, _value) ->
  {error, nif_not_loaded}.

fdb_future_block_until_ready(_f) ->
  {error, nif_not_loaded}.

fdb_future_cancel(_f) ->
  {error, nif_not_loaded}.

fdb_future_destroy(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_cluster(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_database(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_error(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_key(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_keyvalue_array(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_string_array(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_value(_f) ->
  {error, nif_not_loaded}.

fdb_future_get_version(_f) ->
  {error, nif_not_loaded}.

fdb_future_is_ready(_f) ->
  {error, nif_not_loaded}.

fdb_future_release_memory(_f) ->
  {error, nif_not_loaded}.

fdb_get_error(_code) ->
  {error, nif_not_loaded}.

fdb_network_set_option(_option, _value) ->
  {error, nif_not_loaded}.

fdb_select_api_version(_runtime_version) ->
  {error, nif_not_loaded}.

fdb_transaction_add_conflict_range(_tr, _begin_key, _end_key, _type ) ->
  {error, nif_not_loaded}.

fdb_transaction_atomic_op(_tr, _key_name, _param, _operation_type) ->
  {error, nif_not_loaded}.

fdb_transaction_clear(_tr, _key) ->
  {error, nif_not_loaded}.

fdb_transaction_clear_range(_tr, _begin_key, _end_key) ->
  {error, nif_not_loaded}.

fdb_transaction_commit(_tr) ->
  {error, nif_not_loaded}.

fdb_transaction_get(_tr,_key) ->
  {error, nif_not_loaded}.

fdb_transaction_get_addresses_for_key(_tr, _key_name) ->
  {error, nif_not_loaded}.

fdb_transaction_get_committed_version(_tr, _out_version) ->
  {error, nif_not_loaded}.

fdb_transaction_get_key(_tr, _key, _or_equal, _offset, _snapshot) ->
  {error, nif_not_loaded}.

fdb_transaction_get_range(_tr, _begin_key_name, _begin_or_equal, _begin_offset, _end_key_name, _end_or_equal, _end_offset, _limit, _target_bytes, _mode, _iteration, _snapshot, _reverse) ->
  {error, nif_not_loaded}.

fdb_transaction_get_read_version(_tr) ->
  {error, nif_not_loaded}.

fdb_transaction_on_error(_tr, _error) ->
  {error, nif_not_loaded}.

fdb_transaction_reset(_tr) ->
  {error, nif_not_loaded}.

fdb_transaction_set(_tr, _key, _value) 
    when is_binary(_key) 
    andalso is_binary(_value)->
  {error, nif_not_loaded}.

fdb_transaction_set_option(_tr, _option, _value) ->
  {error, nif_not_loaded}.

fdb_transaction_set_read_version(_tr, _version) ->
  {error, nif_not_loaded}.

fdb_transaction_watch(_tr, _key_name) ->
  {error, nif_not_loaded}.

new_cluster()->
  {error, nif_not_loaded}.

new_database() ->
  {error, nif_not_loaded}.

new_transaction() ->
  {error, nif_not_loaded}.

send_on_complete(_Future,_Pid,_Message) ->
  {error, nif_not_loaded}.
