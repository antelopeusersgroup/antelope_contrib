#pragma ident "$Id$"
/* Steim compression structures and constants */

/* Modified for RefTek PASSCAL packet storage */

/* 01-Nov-1993, RLB */

#include "platform.h"

#ifndef _STEIM_TYPES_
#define _STEIM_TYPES_

/* DFA state constants ------------------------------------------------ */
#define _START_STATE   0   /* Ground state                       */
#define _D1            1   /* 1 INT8 difference                  */
#define _D2            2   /* 1 INT16 difference                 */
#define _D1_D1         3   /* 2 INT8 differences                 */
#define _D1_D1_D1      4   /* 3 INT8 differences                 */
#define _D4_f          5   /* 1 INT32 difference  (final state)  */
#define _D2_D2_f       6   /* 2 INT16 differences (final state)  */
#define _D1_D1_D1_D1_f 7   /* 4 INT8 differences (final state)   */

/* Chunk types */
#define CHUNK_NULL     0L  /* No data        */
#define CHUNK_BYTES    1L  /* 4 INT8 values  */
#define CHUNK_WORDS    2L  /* 2 INT16 values */
#define CHUNK_LONG     3L  /* 1 INT32 value  */

/* Steim structures and unions ---------------------------------------- */

/* Chunk union, accessible as bytes, words or long */
typedef union _CHUNK {
   INT8  b[4];              /* 4 INT8  values */
   INT16 w[2];              /* 2 INT16 values */
   INT32 l;                 /* 1 INT32 value  */
} CHUNK;

/* Frame structure */
typedef struct _FRAME {
   UINT32 flags;            /* Frame control header */
   CHUNK chunk[15];        /* Array of chunks composing 1 frame */
} FRAME;

/* Steim compressed data record */
typedef struct _STEIM {
   FRAME frame[15];        /* Array of frames composing 1 record */
} STEIM;

/* DFA transition table entry */
typedef struct _TRANSITION {
   INT8 new_state ;        /* New state after transition     */
   INT8 unget ;            /* Number of difs to unget        */
   INT8 d_ndx ;            /* Index into buffer to place dif */
} TRANSITION ;

/* Data state */
typedef struct _DATA_STATE {
   INT32 x0;               /* Forward integrating constant (x-0) */
   INT32 xn;               /* Reverse integrating constant (x-n) */
   UINT32 flags;            /* Current frame flags storage        */
   INT16 f_ndx;            /* Current frame number within record */
   INT16 c_ndx;            /* Current chunk number within frame  */
} DATA_STATE;

#endif

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/08/25 22:47:59  vernon
 * Rebuilt all the software and was able to get it to complie now.
 *
 * Revision 1.1.1.1  2004/03/09 18:28:23  vernon
 * adding rtp2orb first attempt
 *
 *
 * Revision 1.2  2002/01/18 17:55:58  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
