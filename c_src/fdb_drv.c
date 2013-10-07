#include <stdlib.h>
#include <stdio.h>

#include <erl_driver.h>
#include <ei.h>

#include "fdb_drv.h"

#define FDB_API_VERSION 21
#include "fdb_c.h"

void ei_ok(gd_res_t *res) 
{
  ei_encode_atom(res->buf, &res->index, "ok");
}

fdb_error_t ei_fdb_err(gd_res_t *res, fdb_error_t errcode)
{
  if (errcode != 0) {
    ei_encode_tuple_header(res->buf, &res->index, 2);
    ei_encode_atom(res->buf, &res->index, "error");
    const char* err = fdb_get_error(errcode);
    ei_encode_string(res->buf, &res->index, err);
  } 
  return errcode;
}

void api_add(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  long val1,val2;
  int arity;

  if (ei_decode_tuple_header(req->buf,&req->index,&arity) || arity!=2 ||
      ei_decode_long(req->buf, &req->index, &val1) ||
      ei_decode_long(req->buf, &req->index, &val2))
    return error(res, GDE_ERR_DEC); 

  long result = val1+val2;

  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "ok");
  ei_encode_long(res->buf, &res->index, result);
}

void api_double(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  long val;
  if (ei_decode_long(req->buf, &req->index, &val)) return error(res, GDE_ERR_DEC);

  long result = val*2;
  ei_encode_tuple_header(res->buf, &res->index, 2);
  ei_encode_atom(res->buf, &res->index, "ok");
  ei_encode_long(res->buf, &res->index, result);
}

void cmd_api_version(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  long version;
  if (ei_decode_long(req->buf, &req->index, &version)) return error(res, GDE_ERR_DEC);

  if (ei_fdb_err(res, fdb_select_api_version(version))==0) 
    return ei_ok(res);
}

void cmd_setup_network(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
  if (ei_fdb_err(res, fdb_setup_network())==0)
    return ei_ok(res);
}

static APIFunc API[]= { 
  {CMD_ADD, api_add},
  {CMD_DOUBLE, api_double},
  {CMD_API_VERSION, cmd_api_version},
  {CMD_SETUP_NETWORK, cmd_setup_network}
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
  drv->count = 0;
  return (void *)drv;
}

/**
 * Upon closing the port, this callback is invoked in order to free all memory
 * allocated to the driver state.
 */
void
destroy(void *drv_state) {
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

