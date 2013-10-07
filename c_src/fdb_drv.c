#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#include <erl_driver.h>
#include <ei.h>

#include "fdb_drv.h"

#define FDB_API_VERSION 21
#include "fdb_c.h"

void ei_ok(gd_res_t *res) 
{
  ei_encode_atom(res->buf, &res->index, "ok");
}

int ei_get_string(gd_req_t *req,char** result)
{
  int size, type;
  *result = NULL;
  if( ei_get_type(req->buf,&req->index,&type, &size)!=0) 
  {
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

fdb_error_t ei_fdb_error(gd_res_t *res, fdb_error_t errcode)
{
  if (errcode != 0) {
    ei_encode_tuple_header(res->buf, &res->index, 2);
    ei_encode_atom(res->buf, &res->index, "error");
    const char* err = fdb_get_error(errcode);
    ei_encode_string(res->buf, &res->index, err);
  } 
  return errcode;
}

fdb_error_t wait_until_ready(gd_res_t *res,FDBFuture *future)
{
  fdb_future_block_until_ready(future);
  if (fdb_future_is_error(future))
  {
     return ei_fdb_error(res, fdb_future_get_error(future,NULL));
  }
  return 0;
}

void cmd_api_version(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  long version;
  if (ei_decode_long(req->buf, &req->index, &version)) return error(res, GDE_ERR_DEC);

  if (ei_fdb_error(res, fdb_select_api_version(version))==0) 
    return ei_ok(res);
}

void cmd_setup_network(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  if (ei_fdb_error(res, fdb_setup_network())==0)
    return ei_ok(res);
}

void* network_loop(void *arg)
{
  fdb_run_network();
}

void ei_error(gd_res_t *res, const char* msg) 
{
  ei_encode_tuple_header(res->buf, &res->index,2);
  ei_encode_atom(res->buf, &res->index,"error");
  ei_encode_string(res->buf, &res->index,msg); 
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
  if (ei_get_string(req, &cluster_file_path)!=0) return error(res, GDE_ERR_DEC);

  FDBFuture* future = fdb_create_cluster(cluster_file_path);

  if (wait_until_ready(res, future)!=0) {
    free(cluster_file_path);
    return;
  }
  free(cluster_file_path);

  FDBCluster *cluster;

  if (ei_fdb_error(res, fdb_future_get_cluster(future, &cluster))!=0)
    return;

  long cluster_tmp = (long)cluster;
  ei_encode_long(res->buf,&res->index, cluster_tmp);
}

void cmd_destroy_cluster(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  FDBCluster* cluster;
  long cluster_tmp = 0;
  if (ei_decode_long(req->buf, &req->index, &cluster_tmp)!=0) 
    return error(res, GDE_ERR_DEC);

  cluster = (FDBCluster*)cluster_tmp;

  fdb_cluster_destroy(cluster);
  ei_ok(res);
}

static APIFunc API[]= { 
  {CMD_API_VERSION, cmd_api_version},
  {CMD_SETUP_NETWORK, cmd_setup_network},
  {CMD_RUN_NETWORK, cmd_run_network},
  {CMD_CREATE_CLUSTER, cmd_create_cluster},
  {CMD_DESTROY_CLUSTER, cmd_destroy_cluster}
};    

/* ----------------------------------------------------------------------------
 * Driver callbacks
 * ------------------------------------------------------------------------- */

/**
 * Callback to initialize the application-relevant state data when opening the
 * port driver and to return a pointer to the newly created driver state.
 */
void *
init() {
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
void
destroy(void *drv_state) {
  gdt_drv_t *drv = (gdt_drv_t*)drv_state;
  driver_free(drv_state); /* init */
}

/**
 * Initialize any thread-specific data. This is called, when first dispatching
 * a request to a thread.
 */
void *
thread_init() {
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
void
thread_destroy(void *trd_state) {
  driver_free(trd_state); /* thread_init */
}

/**
 * Load balancing among threads. Balancing is implemented as a modulo
 * operation: % THREADS. Return null for round-robin strategy.
 */
unsigned int *
balance(int cmd, unsigned char syn, unsigned int *key) {
  return NULL;
}

/**
 * Dispatch an asynchronous request by invoking the respective callback. If no
 * matching command is found, return an error.
 */
void
dispatch(gd_req_t *req, gd_res_t *res, void *drv_state, void *trd_state) {
  int  found = 0;
  int api_count = sizeof(API)/sizeof(API[0]);
  int i;
  for (i = 0; i < api_count; i++)
  {
    if (API[i].cmd_id != req->cmd) continue; 
    found = !found;
    API[i].get_result(req, res, (gdt_drv_t *)drv_state, (gdt_trd_t*)trd_state);
    break;
  }
  if (!found) {
    error(res, GDE_ERR_CMD);
  }
}

