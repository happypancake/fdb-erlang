#include "erl_nif.h"
#include "fdb_nifs.h"

#define FDB_API_VERSION 100

#include "fdb_c.h"


static ERL_NIF_TERM nif_error(ErlNifEnv* env,const char* error) 
{
  return enif_make_tuple2(env,enif_make_atom(env,"error"),enif_make_atom(env,error));
}


static ERL_NIF_TERM nif_fdb_cluster_configure_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=0) 
  {
    return nif_error(env,"expected_0_parameters");
  }


  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_cluster_create_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBCluster* c;
//  uint8_t const* db_name;
//  int db_name_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_cluster_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBCluster* c;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_cluster_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

//  FDBCluster* c;
//  FDBClusterOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_create_cluster(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  const char* cluster_file_path;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_database_create_transaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBDatabase* d;
//  FDBTransaction** out_transaction;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_database_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBDatabase* d;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_database_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

//  FDBDatabase* d;
//  FDBDatabaseOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_block_until_ready(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture *f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_cluster(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBFuture* f;
//  FDBCluster** out_cluster;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_database(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBFuture* f;
//  FDBDatabase** out_database;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_error_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=0) 
  {
    return nif_error(env,"expected_0_parameters");
  }


  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_error_v22(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=0) 
  {
    return nif_error(env,"expected_0_parameters");
  }


  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBFuture* f;
//  uint8_t const** out_key;
//  int* out_key_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_keyvalue_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

//  FDBFuture* f;
//  FDBKeyValue const** out_kv;
//  int* out_count;
//  fdb_bool_t* out_more;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_string_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

// FDBFuture* f;
//  const char*** out_strings;
//  int* out_count)

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

//  FDBFuture* f;
//  fdb_bool_t *out_present;
//  uint8_t const** out_value;
//  int* out_value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_get_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBFuture* f;
//  int64_t* out_version;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_is_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_is_error_v22(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=0) 
  {
    return nif_error(env,"expected_0_parameters");
  }


  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_is_ready(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_release_memory(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBFuture* f;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_future_set_callback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBFuture* f;
//  FDBCallback callback;
//  void* callback_parameter;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  fdb_error_t code;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_get_max_api_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

// )

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_network_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBNetworkOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_run_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

// )

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_select_api_version_impl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

// v;
//  FDB_API_VERSION) DLLEXPORT WARN_UNUSED_RESULT fdb_error_t fdb_network_set_option( FDBNetworkOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_setup_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

// )

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_stop_network(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

// )

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_add_conflict_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=6) 
  {
    return nif_error(env,"expected_6_parameters");
  }

// FDBTransaction *tr;
//  uint8_t const* begin_key_name;
//  int begin_key_name_length;
//  uint8_t const* end_key_name;
//  int end_key_name_length;
//  FDBConflictRangeType type)

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_atomic_op(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=6) 
  {
    return nif_error(env,"expected_6_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* key_name;
//  int key_name_length;
//  uint8_t const* param;
//  int param_length;
//  FDBMutationType operation_type;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_cancel(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBTransaction* tr)

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* key_name;
//  int key_name_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_clear_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=5) 
  {
    return nif_error(env,"expected_5_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* begin_key_name;
//  int begin_key_name_length;
//  uint8_t const* end_key_name;
//  int end_key_name_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBTransaction* tr;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_destroy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBTransaction* tr)

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

// fdb_transaction_get_read_version( FDBTransaction* tr;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_addresses_for_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

// FDBTransaction* tr;
//  uint8_t const* key_name;
//  int key_name_length)

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_committed_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBTransaction* tr;
//  int64_t* out_version;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=6) 
  {
    return nif_error(env,"expected_6_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* key_name;
//  int key_name_length;
//  fdb_bool_t or_equal;
//  int offset;
//  fdb_bool_t snapshot;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_range(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=15) 
  {
    return nif_error(env,"expected_15_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* begin_key_name;
//  int begin_key_name_length;
//  fdb_bool_t begin_or_equal;
//  int begin_offset;
//  uint8_t const* end_key_name;
//  int end_key_name_length;
//  fdb_bool_t end_or_equal;
//  int end_offset;
//  int limit;
//  int target_bytes;
//  FDBStreamingMode mode;
//  int iteration;
//  fdb_bool_t snapshot;
//  fdb_bool_t reverse;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_range_selector(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=10) 
  {
    return nif_error(env,"expected_10_parameters");
  }

//  FDBTransaction* tr;
//  uint8_t const* begin_key_name;
//  int begin_key_name_length;
//  fdb_bool_t begin_or_equal;
//  int begin_offset;
//  uint8_t const* end_key_name;
//  int end_key_name_length;
//  fdb_bool_t end_or_equal;
//  int end_offset;
//  int limit;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_get_read_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBTransaction* tr;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_on_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBTransaction* tr;
//  fdb_error_t error;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=1) 
  {
    return nif_error(env,"expected_1_parameters");
  }

//  FDBTransaction* tr;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

// fdb_transaction_set_option( FDBTransaction* tr;
//  FDBTransactionOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_set_option(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=4) 
  {
    return nif_error(env,"expected_4_parameters");
  }

//  FDBTransaction* tr;
//  FDBTransactionOption option;
//  uint8_t const* value;
//  int value_length;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_set_read_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=2) 
  {
    return nif_error(env,"expected_2_parameters");
  }

//  FDBTransaction* tr;
//  int64_t version;

  return nif_error(env,"not_implemented");
}

static ERL_NIF_TERM nif_fdb_transaction_watch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc!=3) 
  {
    return nif_error(env,"expected_3_parameters");
  }

//  FDBTransaction *tr;
//  uint8_t const* key_name;
//  int key_name_length)

  return nif_error(env,"not_implemented");
}

ERL_NIF_INIT(fdb_nif,nifs,NULL,NULL,NULL,NULL)

