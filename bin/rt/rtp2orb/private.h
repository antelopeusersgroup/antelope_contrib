#pragma ident "$Id$"
/*======================================================================
 *
 *  Prototypes for revision specific functions.
 *
 *====================================================================*/
#ifndef reftek_private_included
#define reftek_private_included

#include "reftek.h"
#include "rtp_steim.h"         /* Steim structures and constants */

#define LSWAP(ptr,count) H2N4((ptr),(ptr),(count))
#define SSWAP(ptr,count) H2N2((ptr),(ptr),(count))

VOID reftek_com(UINT8 *src, UINT16 *exp, UINT16 *unit, UINT16 *seqno, REAL64 *tstamp);
VOID reftek_dcomp(struct reftek_dt *dt);

INT16 encode_steim( INT32 *samples, INT16 n_rawsamp, VOID *ptr );
BOOL decode_steim( VOID *ptr, INT16 *n, INT32 *samples );

#endif /* reftek_private_included */

/* Revision History
 *
 * $Log$
 * Revision 1.3  2004/08/27 16:50:44  danq
 * Hi Frank,
 *
 * I've made some modifications to hopefully allow this to compile and run
 * under Linux.  But I can't test it, and there's a reasonable chance I screwed
 * up even the Solaris version.
 *
 * -- danq
 *
 * Revision 1.2  2004/08/26 15:17:23  danq
 * clean up the Makefile, rename steim.h to rtp_steim.h to avoid name conflict
 * with tr steim.h, fix minor problem in man page
 *
 * Revision 1.1  2004/08/25 22:47:59  vernon
 * Rebuilt all the software and was able to get it to complie now.
 *
 * Revision 1.1.1.1  2004/03/09 18:28:23  vernon
 * adding rtp2orb first attempt
 *
 *
 * Revision 1.2  2002/01/18 17:55:57  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
