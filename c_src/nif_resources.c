#include "nif_resources.h"

static ErlNifResourceType *NETWORK_RESOURCE;
static ErlNifResourceType *CLUSTER_RESOURCE;
static ErlNifResourceType *FUTURE_RESOURCE;
static ErlNifResourceType *DATABASE_RESOURCE;
static ErlNifResourceType *TRANSACTION_RESOURCE;

enif_network_t* wrap_network()
{
    enif_network_t *ctx;
    ctx = (enif_network_t*)
          enif_alloc_resource(NETWORK_RESOURCE,sizeof(enif_network_t));
    ctx->is_running = 0;
    ctx->lock = enif_mutex_create("network");
    return ctx;
}

enif_cluster_t* wrap_cluster(FDBCluster *c)
{
    enif_cluster_t *ctx;
    ctx =(enif_cluster_t*)
         enif_alloc_resource(CLUSTER_RESOURCE,sizeof(enif_cluster_t));
    ctx->handle = c;
    ctx->lock = enif_mutex_create("cluster");
    return ctx;
}

enif_database_t* wrap_database(FDBDatabase* d)
{
    enif_database_t *ctx;
    ctx =(enif_database_t*)
         enif_alloc_resource(DATABASE_RESOURCE,sizeof(enif_database_t));
    ctx->handle = d;
    ctx->lock = enif_mutex_create("database");
    return ctx;
}

enif_transaction_t* wrap_transaction(FDBTransaction *t)
{
    enif_transaction_t *ctx;
    ctx =(enif_transaction_t*)
         enif_alloc_resource(TRANSACTION_RESOURCE,sizeof(enif_transaction_t));
    ctx->handle = t;
    ctx->lock = enif_mutex_create("transaction");
    return ctx;
}

enif_future_t* wrap_future(FDBFuture *f)
{
    enif_future_t *ctx;
    ctx =(enif_future_t*)
         enif_alloc_resource(FUTURE_RESOURCE,sizeof(enif_future_t));
    ctx->handle = f;
    ctx->callback_env = NULL;
    ctx->lock = enif_mutex_create("future");
    return ctx;
}

void cleanup_network(ErlNifEnv* env, void* obj)
{
    enif_network_t *ctx = (enif_network_t*) obj;
    if (ctx == NULL) return;
    ErlNifMutex *lock = ctx->lock;
    enif_mutex_lock(lock);
    if (ctx->is_running) {
        fdb_stop_network();
        //pthread_join( ctx->thread, NULL );
        enif_thread_join(ctx->tid, NULL);
        ctx->is_running = 0;
    }
    enif_mutex_unlock(lock);
    enif_mutex_destroy(lock);
}

void cleanup_cluster(ErlNifEnv* env, void* obj)
{
    enif_cluster_t *ctx=(enif_cluster_t*)obj;
    if (ctx == NULL) return;
    ErlNifMutex *lock = ctx->lock;
    enif_mutex_lock(lock);
    if (ctx->handle!=NULL)
    {
        fdb_cluster_destroy(ctx->handle);
        ctx->handle = NULL;
    }
    enif_mutex_unlock(lock);
    enif_mutex_destroy(lock);
}

void cleanup_database(ErlNifEnv* env, void* obj)
{
    enif_database_t *ctx=(enif_database_t*)obj;
    if (ctx == NULL) return;
    ErlNifMutex *lock = ctx->lock;
    enif_mutex_lock(lock);
    if (ctx->handle!=NULL)
    {
        fdb_database_destroy(ctx->handle);
        ctx->handle = NULL;
    }
    enif_mutex_unlock(lock);
    enif_mutex_destroy(lock);
}

void cleanup_transaction(ErlNifEnv* env, void* obj)
{
    enif_transaction_t *ctx=(enif_transaction_t*)obj;
    if (ctx == NULL) return;
    ErlNifMutex *lock = ctx->lock;
    enif_mutex_lock(lock);
    if (ctx->handle!=NULL)
    {
        fdb_transaction_destroy(ctx->handle);
        ctx->handle = NULL;
    }
    enif_mutex_unlock(lock);
    enif_mutex_destroy(lock);
}

void cleanup_future(ErlNifEnv* env, void* obj)
{
    enif_future_t *ctx=(enif_future_t*)obj;
    if (ctx == NULL) return;
    ErlNifMutex *lock = ctx->lock;
    enif_mutex_lock(lock);
    if (ctx->handle!=NULL)
    {
        fdb_future_destroy(ctx->handle);
        ctx->handle = NULL;
    }
    if (ctx->callback_env != NULL)
    {
        enif_free_env(ctx->callback_env);
        ctx->handle = NULL;
    }
    enif_mutex_unlock(lock);
    enif_mutex_destroy(lock);
}

int get_cluster(ErlNifEnv* env, ERL_NIF_TERM term,enif_cluster_t **cluster)
{
    return enif_get_resource(env,term,CLUSTER_RESOURCE,(void**)cluster);
}

int get_database(ErlNifEnv* env, ERL_NIF_TERM term,enif_database_t **database)
{
    return enif_get_resource(env,term,DATABASE_RESOURCE,(void**)database);
}

int get_transaction(ErlNifEnv* env, ERL_NIF_TERM term, enif_transaction_t **transaction)
{
    return enif_get_resource(env,term,TRANSACTION_RESOURCE,(void**)transaction);
}

int get_future(ErlNifEnv* env, ERL_NIF_TERM term,enif_future_t **future)
{
    return enif_get_resource(env,term,FUTURE_RESOURCE,(void**)future);
}




#define REGISTER_RESOURCE(handle,name,dtor) \
  if ( (handle = enif_open_resource_type(env, NULL,name, dtor, \
            ERL_NIF_RT_CREATE, NULL)) == NULL) \
  return -1; \
 

int register_fdb_resources(ErlNifEnv *env)
{
    REGISTER_RESOURCE(NETWORK_RESOURCE,"fdb_network_loop",cleanup_network);
    REGISTER_RESOURCE(CLUSTER_RESOURCE,"fdb_cluster",cleanup_cluster);
    REGISTER_RESOURCE(FUTURE_RESOURCE,"fdb_future",cleanup_future);
    REGISTER_RESOURCE(DATABASE_RESOURCE,"fdb_database",cleanup_database);
    REGISTER_RESOURCE(TRANSACTION_RESOURCE,"fdb_transaction",cleanup_transaction);
    return 0;
}
