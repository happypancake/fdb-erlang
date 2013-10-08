#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <string.h>

#include <erl_driver.h>
#include <ei.h>

#include "fdb_drv.h"

#define FDB_API_VERSION 21
#include "fdb_c.h"

void* network_loop(void *arg)
{
    fdb_run_network();
    return NULL;
}

void ei_ok(gd_res_t *res)
{
    ei_encode_atom(res->buf, &res->index, "ok");
}

void ei_error(gd_res_t *res, const char* msg)
{
    ei_encode_tuple_header(res->buf, &res->index,2);
    ei_encode_atom(res->buf, &res->index,"error");
    ei_encode_string(res->buf, &res->index,msg);
}

int ei_get_string(gd_req_t *req,char** result)
{
    int size, type;
    *result = NULL;
    if( ei_get_type(req->buf,&req->index,&type, &size)!=0) {
        return -1;
    }
    *result = (char*)malloc(size+1);
    if (ei_decode_string(req->buf,&req->index,*result)!=0)
    {
        free(*result);
        return -1;
    }
    return 0;
}

int ei_get_bytes(gd_req_t *req, char** result, int *len)
{
  int type,length;
  if (ei_get_type(req->buf, &req->index,&type,&length)!=0 || 
      type!=ERL_BINARY_EXT)
  {
    return  -1;
  }

  long size;

  *result = (char*)malloc(length);
  if(ei_decode_binary(req->buf,&req->index,*result,&size)!=0 || size!=length);
  {
    return -1;
  }
  *len = length;
  return 0;
}

int ei_decode_ptr(gd_req_t *req,gd_res_t *res,void** dest)
{
  ptr_data data;
  long len = 0;
  int errcode = ei_decode_binary(
      req->buf, 
      &req->index,
      (void*)&(data.ptr_data),
      &len);

  if (errcode!=0 || len!=sizeof(ptr_data))
  {
    ei_error(res,"invalid handle");
    *dest = NULL;
    return -1;
  }
  else
  {
    *dest = data.ptr;
    return 0;
  }
}

void ei_encode_ptr(gd_res_t *res, void* ptr)
{
  ptr_data data;
  data.ptr = ptr;
  ei_encode_binary(res->buf,&res->index,data.ptr_data,sizeof(data.ptr_data));
}

fdb_error_t ei_fdb_error(gd_res_t *res, fdb_error_t errcode)
{
    if (errcode != 0)
        ei_error(res,fdb_get_error(errcode));
    return errcode;
}

fdb_error_t wait_until_ready(gd_res_t *res,FDBFuture *future)
{
    fdb_future_block_until_ready(future);
    if (fdb_future_is_error(future)) {
        return ei_fdb_error(res, fdb_future_get_error(future,NULL));
    }
    return 0;
}

void cmd_api_version(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
    long version;
    if (ei_decode_long(req->buf, &req->index, &version)) return ei_error(res, "invalid_version");

    if (ei_fdb_error(res, fdb_select_api_version(version))==0)
        return ei_ok(res);
}

void cmd_setup_network(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
    if (ei_fdb_error(res, fdb_setup_network())==0)
        return ei_ok(res);
}

void cmd_run_network(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
    if (drv->network_thread_started!=0)
        return ei_error(res,"network_already_running");

    if (pthread_create(&drv->network_thread,NULL,network_loop,NULL)!= 0)
        return ei_error(res,"unable_to_create_network_thread");

    drv->network_thread_started = 1;

    return ei_ok(res);
}

void cmd_create_cluster(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
    char *cluster_file_path=NULL;
    // it might not exist, so no need to check here
    ei_get_string(req, &cluster_file_path); 

    FDBFuture* future = fdb_create_cluster(cluster_file_path);

    int errorcode = wait_until_ready(res, future);
    
    if (cluster_file_path!=NULL) free(cluster_file_path);

    if (errorcode != 0) return;

    FDBCluster *cluster=NULL;

    errorcode = ei_fdb_error(res, fdb_future_get_cluster(future, &cluster));
   
    fdb_future_destroy(future);

    if (errorcode!=0)
        return;

    ei_encode_ptr(res,(void*)cluster);
}

void cmd_cluster_destroy(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBCluster *cluster;
  if (ei_decode_ptr(req,res,(void**)&cluster)!=0)
  {
    return ei_error(res,"invalid_cluster_handle");
  }
  fdb_cluster_destroy(cluster);
  ei_ok(res);
}

void cmd_cluster_create_database(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBCluster *cluster;
  if (ei_decode_ptr(req,res,(void**)&cluster)!=0)
  {
    return ei_error(res,"invalid_cluster_handle");
  }

  const char *dbname="DB";

  FDBFuture* future = fdb_cluster_create_database(cluster,
      (const uint8_t*)dbname,
      strlen(dbname));

  if (wait_until_ready(res, future)!=0) return;

  FDBDatabase* DB;

  fdb_error_t errcode =ei_fdb_error(res,fdb_future_get_database(future, &DB));
  fdb_future_destroy(future);
  if (errcode!=0) return;
  
  ei_encode_ptr(res,(void*)DB);
}

void cmd_database_destroy(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBDatabase *DB;
  if (ei_decode_ptr(req,res,(void**)&DB)!=0)
  {
    return ei_error(res,"invalid_database_handle");
  }
  fdb_database_destroy(DB);
  ei_ok(res);
}

void cmd_database_create_transaction(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBDatabase *DB;
  if (ei_decode_ptr(req,res,(void**)&DB)!=0)
  {
    return ei_error(res,"invalid_database_handle");
  }
  FDBTransaction * transaction=NULL;
  if(ei_fdb_error(res,fdb_database_create_transaction(DB,&transaction))!=0)
    return;

  ei_encode_ptr(res,(void*)transaction);
}

void cmd_transaction_destroy(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBTransaction *Tx;
  if (ei_decode_ptr(req,res,(void**)&Tx)!=0)
  {
    return ei_error(res,"invalid_transaction_handle");
  }
  fdb_transaction_destroy(Tx);
  ei_ok(res);
}

void cmd_transaction_get(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  int arity;
  if (ei_decode_tuple_header(req->buf, &req->index, &arity)!=0 || arity!=2)
  {
    return ei_error(res,"invalid_tuple");
  }

  FDBTransaction *Tx;
  if (ei_decode_ptr(req,res,(void**)&Tx)!=0)
  {
    return ei_error(res,"invalid_transaction_handle");
  }
  char* key;
  int keysize=0;

  if (ei_get_bytes(req,&key,&keysize)!= 0)
  {
    return ei_error(res,"invalid_key");
  }

  FDBFuture* future = fdb_transaction_get(Tx,(const uint8_t*)key,keysize,0);
  
  fdb_error_t errcode = wait_until_ready(res, future);
  
  free(key);
  
  if (errcode!=0)
  {
    fdb_future_destroy(future);
    ei_fdb_error(res,errcode);
    return;
  }

  char* value = NULL;

  fdb_bool_t is_present = 0;
  int length=0;

  errcode = fdb_future_get_value(future,&is_present,(const uint8_t**)&value,&length);
  if (errcode!=0)
  {
    fdb_future_destroy(future);
    ei_fdb_error(res,errcode);
    return;
  }

  if (is_present == 0)
  {
    ei_encode_tuple_header(res->buf, &res->index, 2);
    ei_encode_atom(res->buf, &res->index, "ok");
    ei_encode_atom(res->buf, &res->index, "not_found");
  }
  else
  {
    ei_encode_binary(res->buf, &res->index, (void*)value,length);
  }
  fdb_future_destroy(future);
}

void cmd_transaction_set(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  int arity;
  if (ei_decode_tuple_header(req->buf, &req->index, &arity)!=0 || arity!=3)
  {
    return ei_error(res,"invalid_tuple");
  }
  FDBTransaction *Tx;
  if (ei_decode_ptr(req,res,(void**)&Tx)!=0)
  {
    return ei_error(res,"invalid_transaction_handle");
  }
  char *key,*val;
  int keysize,valsize;

  if (ei_get_bytes(req,&key,&keysize)!= 0)
  {
    ei_error(res,"invalid_key");
    return;
  }
  if (ei_get_bytes(req, &val, &valsize)!= 0)
  {
    free(key);
    ei_error(res,"invalid_value");
    return;
  }

  fdb_transaction_set(Tx,(const uint8_t *)key,keysize,(const uint8_t*)val,valsize);
}


static api_func_t API[]= {
    {CMD_API_VERSION, cmd_api_version},
    {CMD_SETUP_NETWORK, cmd_setup_network},
    {CMD_RUN_NETWORK, cmd_run_network},
    {CMD_CREATE_CLUSTER, cmd_create_cluster},
    {CMD_CLUSTER_DESTROY, cmd_cluster_destroy},
    {CMD_CLUSTER_CREATE_DATABASE, cmd_cluster_create_database},
    {CMD_DATABASE_DESTROY, cmd_database_destroy},
    {CMD_DATABASE_CREATE_TRANSACTION, cmd_database_create_transaction},
    {CMD_TRANSACTION_DESTROY, cmd_transaction_destroy},
    {CMD_TRANSACTION_GET, cmd_transaction_get},
    {CMD_TRANSACTION_SET, cmd_transaction_set}
};

/* ----------------------------------------------------------------------------
 * Driver callbacks
 * ------------------------------------------------------------------------- */

/**
 * Callback to initialize the application-relevant state data when opening the
 * port driver and to return a pointer to the newly created driver state.
 */
void * init()
{
  gdt_drv_t *drv;
  if ((drv = driver_alloc(sizeof(gdt_drv_t))) == NULL) /* destroy */
    return NULL;
  drv->network_thread_started = 0;

  return (void *)drv;
}


/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to the driver state.
 */
void destroy(void *drv_state)
{
  driver_free(drv_state); /* init */
}

/**
 * Initialize any thread-specific data. This is called, when first dispatching
 * a request to a thread.
 */
void * thread_init()
{
    gdt_trd_t *trd;
    if ((trd = driver_alloc(sizeof(gdt_trd_t))) == NULL) /* thread_destroy */
        return NULL;
    trd->count = 0;
    return (void *)trd;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to thread-specific data.
 */
void thread_destroy(void *trd_state)
{
    driver_free(trd_state); /* thread_init */
}

/**
 * Load balancing among threads. Balancing is implemented as a modulo
 * operation: % THREADS. Return null for round-robin strategy.
 */
unsigned int * balance(int cmd, unsigned char syn, unsigned int *key)
{
    return NULL;
}

/**
 * Dispatch an asynchronous request by invoking the respective callback. If no
 * matching command is found, return an error.
 */
void dispatch(gd_req_t *req, gd_res_t *res, void *drv_state, void *trd_state)
{
    int  found = 0;
    int api_count = sizeof(API)/sizeof(API[0]);
    int i;
    for (i = 0; i < api_count; i++) {
        if (API[i].cmd_id != req->cmd) continue;
        found = !found;
        API[i].get_result(req, res, (gdt_drv_t *)drv_state, (gdt_trd_t*)trd_state);
        break;
    }
    if (!found) {
        ei_error(res, "invalid_command");
    }
}

