
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2000/03/09 21:59:17  davidk
 *     added a prototype for SwapFloat(), it had not been inluded in the
 *     list of swap function prototypes.
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */

#ifndef SWAP_H
#define SWAP_H

#include <trace_buf.h>
/* include file for swap.c: handy routines for swapping earthwormy things */

void SwapShort( short * );                  /* swap.c       sys-independent  */
void SwapInt( int * );                      /* swap.c       sys-independent  */
void SwapLong ( long * );                   /* swap.c       sys-independent  */
void SwapDouble( double * );                /* swap.c       sys-independent  */
void SwapFloat( float * );                  /* swap.c       sys-independent  */

/* fixes wave message into local byte order, based on globals _SPARC and _INTEL */
int WaveMsgMakeLocal( TRACE_HEADER* );

#endif
