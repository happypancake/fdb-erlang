#ifndef __GEN_DRIVER_TEST__
#define __GEN_DRIVER_TEST__

#include "gen_driver.h"
#include <pthread.h>
#define FDB_API_VERSION 21
#include "fdb_c.h"

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

/**
 * This structure is an example for driver-specific state data and holds the
 * amount of calls made to the driver.
 */
typedef struct gdt_drv_t {
    pthread_t network_thread;
    int network_thread_started;
} gdt_drv_t;

/**
 * This structure is an example for thread-specific state data and holds the
 * amount of calls made to the respective thread.
 */
typedef struct gdt_trd_ {
    int count;
} gdt_trd_t;

/**
 * This structure is used to wrap API calls
 */
typedef struct {
    int cmd_id;
    void (*get_result)(gd_req_t *req, gd_res_t *res, gdt_drv_t *drv, gdt_trd_t *trd);
} api_func_t;

typedef union {
    void* ptr;
    uint8_t ptr_data[8];
} ptr_data;

/**
 * Error atoms to be returned by the generic driver.
#define GDE_ERR_MEM "memory"
#define GDE_ERR_ARI "arity"
#define GDE_ERR_DEC "decode"
#define GDE_ERR_CMD "command"
#define GDE_ERR_TPE "type"
 */

#endif
