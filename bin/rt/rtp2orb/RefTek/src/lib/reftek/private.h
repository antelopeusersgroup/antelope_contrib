#pragma ident "$Id$"
/*======================================================================
 *
 *  Prototypes for revision specific functions.
 *
 *====================================================================*/
#ifndef reftek_private_included
#define reftek_private_included

#include "reftek.h"
#include "steim.h"         /* Steim structures and constants */

#ifndef BIG_ENDIAN_HOST
#   define REVERSE_BYTE_ORDER
#   define LSWAP(ptr, count) util_lswap((UINT32 *) ptr, count)
#   define SSWAP(ptr, count) util_sswap((UINT16 *) ptr, count)
#else
#   undef  REVERSE_BYTE_ORDER
#   define LSWAP(ptr, count) 
#   define SSWAP(ptr, count) 
#endif /* BIG_ENDIAN_HOST */

VOID reftek_com(UINT8 *src, UINT16 *exp, UINT16 *unit, UINT16 *seqno, REAL64 *tstamp);
VOID reftek_dcomp(struct reftek_dt *dt);

INT16 encode_steim( INT32 *samples, INT16 n_rawsamp, VOID *ptr );
BOOL decode_steim( VOID *ptr, INT16 *n, INT32 *samples );

#endif /* reftek_private_included */

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.2  2002/01/18 17:55:57  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
