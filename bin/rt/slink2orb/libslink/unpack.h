/***************************************************************************
 * unpack.h:
 * 
 * Interface declarations for the Mini-SEED unpacking routines in unpack.c
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *
 * modified: 2003.177
 ***************************************************************************/


#ifndef	UNPACK_H
#define	UNPACK_H 1

#include "libslink.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int msr_unpack (SLlog * log, MSrecord * msr, int swapflag);

#ifdef __cplusplus
}
#endif

#endif
