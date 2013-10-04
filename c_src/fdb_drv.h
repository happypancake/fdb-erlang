#ifndef __GEN_DRIVER_TEST__
#define __GEN_DRIVER_TEST__

#include "gen_driver.h"

/* ----------------------------------------------------------------------------
 * Type definitions
 * ------------------------------------------------------------------------- */

/**
 * This structure is an example for driver-specific state data and holds the
 * amount of calls made to the driver.
 */
typedef struct gdt_drv_t {
  int count;
} gdt_drv_t;

/**
 * This structure is an example for thread-specific state data and holds the
 * amount of calls made to the respective thread.
 */
typedef struct gdt_trd_ {
  int count;
} gdt_trd_t;

/* ----------------------------------------------------------------------------
 * Macros
 * ------------------------------------------------------------------------- */

/**
 * Actions to be executed by the driver.
 */
#define CMD_ADD 0x01
#define CMD_DOUBLE 0x02

/**
 * Error atoms to be returned by the generic driver.
 */
#define GDE_ERR_MEM "memory"
#define GDE_ERR_ARI "arity"
#define GDE_ERR_DEC "decode"
#define GDE_ERR_CMD "command"
#define GDE_ERR_TPE "type"

#endif
