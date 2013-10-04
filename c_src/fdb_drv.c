#include <stdlib.h>
#include <stdio.h>

#include <erl_driver.h>
#include <ei.h>

#include "fdb_drv.h"

typedef struct {
  int cmd_id;
  int arity;
  void (*get_result)(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd);
} APIFunc;

void api_add(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd);
void api_double(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd);

int ei_get_long(gd_req_t *req, gd_res_t *res, long *value);

static APIFunc API[]= { 
  {CMD_ADD, 2, api_add},
  {CMD_DOUBLE, 1, api_double}
};    

void api_add(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
   long val1,val2;

   if (!ei_get_long(req, res, &val1) || !ei_get_long(req, res, &val2)) return;
     
   long result = val1+val2;

   ei_encode_long(res->buf, &res->index, result);
}

void api_double(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd)
{
   long val;
   if (!ei_get_long(req, res, &val)) return;

   long result = val*2;
   ei_encode_long(res->buf, &res->index, result);
}

int ei_get_long(gd_req_t *req,gd_res_t *res,long *value)
{
  int type,size;

  if (ei_get_type(req->buf, &req->index, &type, &size)!=0 || type != ERL_SMALL_INTEGER_EXT) 
  {
     error(res,GDE_ERR_TPE);
     return 0;
  }
  else if (ei_decode_long(req->buf, &req-> index, value)!=0)
  {
     error(res, GDE_ERR_DEC);
     return 0;
  }
  else 
     return 1;
}

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
  int size,type;
  for (i = 0; i < api_count; i++)
  {
    if (API[i].cmd_id != req->cmd) continue; 
    found = !found;
    if (ei_get_type(req->buf, &req->index, &type, &size) || size != API[i].arity)
      return error(res, GDE_ERR_ARI);
    API[i].get_result(req, res, (gdt_drv_t *)drv_state, (gdt_trd_t*)trd_state);
    break;
  }
  if (!found) {
      error(res, GDE_ERR_CMD);
  }
}

