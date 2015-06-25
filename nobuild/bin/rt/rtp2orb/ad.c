#pragma ident "$Id$"
/*======================================================================
 *
 *  Decode an AD packet
 *
 *====================================================================*/
#include "private.h"

BOOL reftek_ad(struct reftek_ad *dest, UINT8 *src)
{
    reftek_com(src, &dest->exp, &dest->unit, &dest->seqno, &dest->tstamp);
    return TRUE;
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/08/25 22:47:58  vernon
 * Rebuilt all the software and was able to get it to complie now.
 *
 * Revision 1.1.1.1  2004/03/09 18:28:23  vernon
 * adding rtp2orb first attempt
 *
 *
 * Revision 1.2  2002/01/18 17:55:55  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
