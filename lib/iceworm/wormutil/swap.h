#ifndef SWAP_H
#define SWAP_H

#include <trace_buf.h>
/* include file for swap.c: handy routines for swapping earthwormy things */

void SwapShort( short * );                  /* swap.c       sys-independent  */
void SwapInt( int * );                      /* swap.c       sys-independent  */
void SwapLong ( long * );                   /* swap.c       sys-independent  */
void SwapDouble( double * );                /* swap.c       sys-independent  */

/* fixes wave message into local byte order, based on globals _SPARC and _INTEL */
int WaveMsgMakeLocal( TRACE_HEADER* );

#endif
