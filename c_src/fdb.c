/*#include <stdio.h>
#include <pthread.h>
#include "erl_nif.h"

#ifndef FDB_API_VERSION
#define FDB_API_VERSION 100
#endif

#include "/usr/include/foundationdb/fdb_c.h"

static ErlNifResourceType* fdb_ctx_resource = NULL;
static pthread_t run_thread;
typedef struct {
  FDBDatabase* db;
} Context;


// Prototypes
static ERL_NIF_TERM fdbdriver_api_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fdbdriver_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM fdbdriver_transaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enif_make_error(ErlNifEnv* env, const int errorcode);
static ERL_NIF_TERM enif_make_ok(ErlNifEnv* env, ERL_NIF_TERM details);

static ErlNifFunc nif_funcs[] =
{
  {"api_version", 1, fdbdriver_api_version},
  {"open"       , 0, fdbdriver_open},
  {"transaction", 1, fdbdriver_transaction}
};

static ERL_NIF_TERM fbdriver_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  FDBFuture* f = fdb_create_cluster(NULL);
  fdb_error_t errorcode = fdb_future_block_until_ready(f);
  if ( errorcode != 0 ) 
  {
     return enif_make_error(env, errorcode);
  }
  FDBCluster* cluster;
  errorcode = fdb_future_get_cluster(f, &cluster);
  if (errorcode != 0 ) 
  {
     return enif_make_error(env, errorcode);
  }
  fdb_future_destroy(f);

  static const uint8_t dbname[] = {'D','B'};

  f = fdb_cluster_create_database(cluster,dbname, 2);

  errorcode = fdb_future_block_until_ready(f);
  if ( errorcode != 0 ) 
  {
    return enif_make_error(env, errorcode);
  }

  Context_t *ctx = enif_alloc_resource(fdb_ctx_resource,sizeof(Context));
  
  errorcode = fdb_future_get_database(f, &((*ctx).db));
  if (errorcode != 0 ) 
  {
     enif_release_resource(ctx);
     return enif_make_error(env, errorcode);
  }
  fdb_future_destroy(f);

  ERL_NIF_TERM result = enif_make_resource(env,ctx);
  enif_release_resource(ctx);

  return enif_make_ok(env, result);
}

static ERL_NIF_TERM fdbdriver_transaction(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  FDBDatabase*** db;
  if (argc!=1 || !enif_get_resource(env, argv[0], fdb_db_resource, db)) {
    return enif_make_badarg(env);
  }
  FDBTransaction* transaction;
  fdb_error_t errorcode = fdb_database_create_transaction(*db, &transaction);
  if (errorcode != 0 ) 
  {
     return enif_make_error(env, errorcode);
  }

  ERL_NIF_TERM result = enif_make_resource(env, transaction);
  enif_release_resource(transaction);

  return enif_make_ok(env,result);
}

static void fdb_ctx_resource_cleanup(ErlNifEnv* env, void* arg)
{
    Context* ctx = (Context*)arg;
    if (ctx.db != NULL) 
    {
       fdb_database_destroy(ctx.db);
       free(ctx.db);
    }
}


static ERL_NIF_TERM fdbdriver_api_version(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    int version;
    if (!enif_get_int(env, argv[0], &version)) {
        return enif_make_badarg(env);
    }
    fdb_error_t errorcode = fdb_select_api_version(version); 
    if (errorcode != 0)
    {
        return enif_make_error(env, errorcode);
    }
    return enif_make_atom(env, "ok");
}

static void *main_thread(void* _)
{
  fdb_run_network();
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  fdb_select_api_version(FDB_API_VERSION);
  fdb_error_t errorcode = fdb_setup_network();
  if (errorcode != 0 ) 
  {   
    return errorcode;
  }

  pthread_create(&run_thread, NULL, main_thread, (void *)NULL);

  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                   "fdb_ctx_resource",
                                                   &fdb_ctx_resource_cleanup,
                                                   flags, NULL);
  if (rt == NULL)
      return -1;
  fdb_db_resource = rt;

  return 0;
}

static ERL_NIF_TERM enif_make_error(ErlNifEnv* env, const int errorcode)
{
  const char* error_msg = fdb_get_error(errorcode);
  ERL_NIF_TERM term_error_msg = enif_make_string(env, error_msg, ERL_NIF_LATIN1);
  return enif_make_tuple2(env, enif_make_atom(env, "error"), term_error_msg);
}

static ERL_NIF_TERM enif_make_ok(ErlNifEnv* env, ERL_NIF_TERM details)
{
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), details);
}


ERL_NIF_INIT(fdb, nif_funcs, on_load, NULL, NULL, NULL);

*/
