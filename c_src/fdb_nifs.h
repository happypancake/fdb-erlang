#ifndef __FDB_FUNCS__ 
#define __FDB_FUNCS__

#include "erl_nif.h"

static ERL_NIF_TERM nif_fdb_cluster_configure_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_cluster_create_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_cluster_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_cluster_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_create_cluster(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_database_create_transaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_database_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_database_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_block_until_ready(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_cluster(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_error_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_error_v22(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_keyvalue_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_string_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_get_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_is_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_is_error_v22(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_is_ready(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_release_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_future_set_callback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_get_max_api_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_network_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_run_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_select_api_version_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_setup_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_stop_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_add_conflict_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_atomic_op(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_clear_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_addresses_for_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_committed_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_range_selector(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_get_read_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_on_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_set_read_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_fdb_transaction_watch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nifs[] =
{
{"fdb_cluster_configure_database", 0, nif_fdb_cluster_configure_database},
{"fdb_cluster_create_database", 3, nif_fdb_cluster_create_database},
{"fdb_cluster_destroy", 1, nif_fdb_cluster_destroy},
{"fdb_cluster_set_option", 4, nif_fdb_cluster_set_option},
{"fdb_create_cluster", 1, nif_fdb_create_cluster},
{"fdb_database_create_transaction", 2, nif_fdb_database_create_transaction},
{"fdb_database_destroy", 1, nif_fdb_database_destroy},
{"fdb_database_set_option", 4, nif_fdb_database_set_option},
{"fdb_future_block_until_ready", 1, nif_fdb_future_block_until_ready},
{"fdb_future_cancel", 1, nif_fdb_future_cancel},
{"fdb_future_destroy", 1, nif_fdb_future_destroy},
{"fdb_future_get_cluster", 2, nif_fdb_future_get_cluster},
{"fdb_future_get_database", 2, nif_fdb_future_get_database},
{"fdb_future_get_error", 1, nif_fdb_future_get_error},
{"fdb_future_get_error_impl", 0, nif_fdb_future_get_error_impl},
{"fdb_future_get_error_v22", 0, nif_fdb_future_get_error_v22},
{"fdb_future_get_key", 3, nif_fdb_future_get_key},
{"fdb_future_get_keyvalue_array", 4, nif_fdb_future_get_keyvalue_array},
{"fdb_future_get_string_array", 3, nif_fdb_future_get_string_array},
{"fdb_future_get_value", 4, nif_fdb_future_get_value},
{"fdb_future_get_version", 2, nif_fdb_future_get_version},
{"fdb_future_is_error", 1, nif_fdb_future_is_error},
{"fdb_future_is_error_v22", 0, nif_fdb_future_is_error_v22},
{"fdb_future_is_ready", 1, nif_fdb_future_is_ready},
{"fdb_future_release_memory", 1, nif_fdb_future_release_memory},
{"fdb_future_set_callback", 3, nif_fdb_future_set_callback},
{"fdb_get_error", 1, nif_fdb_get_error},
{"fdb_get_max_api_version", 1, nif_fdb_get_max_api_version},
{"fdb_network_set_option", 3, nif_fdb_network_set_option},
{"fdb_run_network", 1, nif_fdb_run_network},
{"fdb_select_api_version_impl", 4, nif_fdb_select_api_version_impl},
{"fdb_setup_network", 1, nif_fdb_setup_network},
{"fdb_stop_network", 1, nif_fdb_stop_network},
{"fdb_transaction_add_conflict_range", 6, nif_fdb_transaction_add_conflict_range},
{"fdb_transaction_atomic_op", 6, nif_fdb_transaction_atomic_op},
{"fdb_transaction_cancel", 1, nif_fdb_transaction_cancel},
{"fdb_transaction_clear", 3, nif_fdb_transaction_clear},
{"fdb_transaction_clear_range", 5, nif_fdb_transaction_clear_range},
{"fdb_transaction_commit", 1, nif_fdb_transaction_commit},
{"fdb_transaction_destroy", 1, nif_fdb_transaction_destroy},
{"fdb_transaction_get", 1, nif_fdb_transaction_get},
{"fdb_transaction_get_addresses_for_key", 3, nif_fdb_transaction_get_addresses_for_key},
{"fdb_transaction_get_committed_version", 2, nif_fdb_transaction_get_committed_version},
{"fdb_transaction_get_key", 6, nif_fdb_transaction_get_key},
{"fdb_transaction_get_range", 15, nif_fdb_transaction_get_range},
{"fdb_transaction_get_range_selector", 10, nif_fdb_transaction_get_range_selector},
{"fdb_transaction_get_read_version", 1, nif_fdb_transaction_get_read_version},
{"fdb_transaction_on_error", 2, nif_fdb_transaction_on_error},
{"fdb_transaction_reset", 1, nif_fdb_transaction_reset},
{"fdb_transaction_set", 4, nif_fdb_transaction_set},
{"fdb_transaction_set_option", 4, nif_fdb_transaction_set_option},
{"fdb_transaction_set_read_version", 2, nif_fdb_transaction_set_read_version},
{"fdb_transaction_watch", 3, nif_fdb_transaction_watch},
};
#endif
