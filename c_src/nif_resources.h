#ifndef __NIF_RESOURCES__
#define __NIF_RESOURCES__

#define FDB_API_VERSION 100

#include <pthread.h>
#include "fdb_c.h"
#include "erl_nif.h"

typedef struct {
    ErlNifMutex *lock; 
    pthread_t thread;
    ErlNifTid tid;
    int is_running;
} enif_network_t;

typedef struct {
    ErlNifMutex *lock;
    FDBCluster* handle;
    void *parent;
} enif_cluster_t;

typedef struct {
    ErlNifMutex *lock;
    FDBDatabase* handle;
    void *parent;
} enif_database_t;

typedef struct {
    ErlNifMutex *lock;
    FDBTransaction* handle;
    void *parent;
} enif_transaction_t;

typedef struct {
    ErlNifMutex *lock;
    FDBFuture* handle;
    ErlNifEnv* callback_env;
    ErlNifPid  callback_pid;
    ERL_NIF_TERM callback_msg;
    void* parent;
} enif_future_t;

enif_network_t* wrap_network();
enif_cluster_t* wrap_cluster(FDBCluster *c);
enif_database_t* wrap_database(FDBDatabase *d);
enif_transaction_t* wrap_transaction(FDBTransaction *t);
enif_future_t* wrap_future(FDBFuture *f);

int get_cluster(ErlNifEnv* env, ERL_NIF_TERM term,enif_cluster_t **cluster);
int get_database(ErlNifEnv* env, ERL_NIF_TERM term,enif_database_t **database);
int get_transaction(ErlNifEnv* env, ERL_NIF_TERM term, enif_transaction_t **transaction);
int get_future(ErlNifEnv* env, ERL_NIF_TERM term,enif_future_t **future);

int register_fdb_resources(ErlNifEnv *env);


#endif
