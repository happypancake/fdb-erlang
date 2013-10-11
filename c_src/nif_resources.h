#ifndef __NIF_RESOURCES__
#define __NIF_RESOURCES__

#define FDB_API_VERSION 100

#include <pthread.h>
#include "fdb_c.h"
#include "erl_nif.h"

typedef struct {
    pthread_t thread;
    int is_running;
} enif_network_t;

typedef struct {
    FDBCluster* handle;
} enif_cluster_t;

typedef struct {
    FDBDatabase* handle;
} enif_database_t;

typedef struct {
    FDBTransaction* handle;
} enif_transaction_t;

typedef struct {
    FDBFuture* handle;
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
