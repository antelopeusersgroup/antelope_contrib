#pragma ident "$Id$"
/*======================================================================
 *
 *  Determine record type
 *
 *====================================================================*/
#include "reftek.h"

#define SOH   0x01
#define ATTEN 0x80
#define RT130_CMND 0x84
#define RT130_RESP 0x85

UINT16 reftek_type(UINT8 *raw)
{
    if (memcmp(raw, "AD", 2) == 0) return REFTEK_AD;
    if (memcmp(raw, "CD", 2) == 0) return REFTEK_CD;
    if (memcmp(raw, "DS", 2) == 0) return REFTEK_DS;
    if (memcmp(raw, "DT", 2) == 0) return REFTEK_DT;
    if (memcmp(raw, "EH", 2) == 0) return REFTEK_EH;
    if (memcmp(raw, "ET", 2) == 0) return REFTEK_ET;
    if (memcmp(raw, "OM", 2) == 0) return REFTEK_OM;
    if (memcmp(raw, "SC", 2) == 0) return REFTEK_SC;
    if (memcmp(raw, "SH", 2) == 0) return REFTEK_SH;

    if (raw[0] == SOH   || raw[0] == RT130_RESP) return REFTEK_SPEC;
    if (raw[0] == ATTEN || raw[0] == RT130_CMND) return REFTEK_CMND;

    return 0;
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.3  2002/02/05 22:32:19  nobody
 * added support for RT130 command and response frames
 *
 * Revision 1.2  2002/01/18 17:55:59  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
