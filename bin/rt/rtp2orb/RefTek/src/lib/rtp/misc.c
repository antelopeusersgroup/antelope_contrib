#pragma ident "$Id$"
/*======================================================================
 * 
 * Miscellaneous utilities
 *
 *====================================================================*/
#include "rtp.h"
#include "reftek.h"
#include "util.h"

/* Get the current attribute time out value */

INT32 rtp_timeout(RTP *rtp)
{
INT32 retval;

    MUTEX_LOCK(&rtp->attr.at_mutex);
        retval = rtp->attr.at_timeo;
    MUTEX_UNLOCK(&rtp->attr.at_mutex);
    return retval;
}

/* Get the current read error flag */

UINT16 rtp_errno(RTP *rtp)
{
UINT16 retval;

    MUTEX_LOCK(&rtp->rcv.mutex);
        retval = rtp->rcv.error;
    MUTEX_UNLOCK(&rtp->rcv.mutex);
    return retval;
}

/* Accept/reject a packet */

#define STRM_OFF  18 

BOOL rtp_want(RTP *rtp, UINT8 *pkt)
{
UINT16 exp, type, unit, seqno, stream;
REAL64 tstamp;

/* If we've requested everything, then we don't even check.
 * This means garbage can go through, but one man's garbage
 * is another man's data.
 */

    if (rtp->attr.at_pmask == RTP_PMASK_ALL) return TRUE;

/* Now, see if it is a desired type */

    type = reftek_type(pkt);
    if ((rtp->attr.at_pmask & type) == 0) return FALSE;

/* We have no knowledge of the contents of special packet, so if this
 * is one and we want them then we get them all.  Note that from our
 * point of view, "special" and "command" packets are both "special".
 */

    if (type == RTP_PMASK_SPEC) return TRUE;

/* If we don't have UNIT id filtering in place then we are done */

    if (rtp->attr.at_dasid == 0) return TRUE;

/* Decode the common header to see if this is from the desired unit */

    reftek_com(pkt, &exp, &unit, &seqno, &tstamp);
    if (unit != rtp->attr.at_dasid) return FALSE;

/* Only stream filtering remains, so if this isn't a DT packet we're done */

    if (type != RTP_PMASK_DT) return TRUE;

/* We are going to "cheat" and decode the stream id here, instead of
 * via the reftek_dt library routine.  This is for efficiency, since
 * the library routine also deals with the data part, which we are not
 * interested in here.
 */

    stream = (UINT16) utilBcdToUint32(pkt + STRM_OFF, 2, 0);
    return ((rtp->attr.at_smask & (1 << stream)) == 0) ? FALSE : TRUE;
}

/* Decode a packet mask */

#define RTP_PMASKBUFLEN 64
#define RTP_SMASKBUFLEN 64

CHAR *rtp_decode_pmask(UINT32 pmask, CHAR *buf)
{

    buf[0] = 0;
    if (pmask & RTP_PMASK_AD) sprintf(buf + strlen(buf), "AD ");
    if (pmask & RTP_PMASK_CD) sprintf(buf + strlen(buf), "CD ");
    if (pmask & RTP_PMASK_DS) sprintf(buf + strlen(buf), "DS ");
    if (pmask & RTP_PMASK_DT) sprintf(buf + strlen(buf), "DT ");
    if (pmask & RTP_PMASK_EH) sprintf(buf + strlen(buf), "EH ");
    if (pmask & RTP_PMASK_ET) sprintf(buf + strlen(buf), "ET ");
    if (pmask & RTP_PMASK_OM) sprintf(buf + strlen(buf), "OM ");
    if (pmask & RTP_PMASK_SC) sprintf(buf + strlen(buf), "SC ");
    if (pmask & RTP_PMASK_SH) sprintf(buf + strlen(buf), "SH ");
    if (pmask & RTP_PMASK_SPEC) sprintf(buf + strlen(buf), "special ");

    if (buf[0] == 0) sprintf(buf, "0x%0x defines no known packets", pmask);

    return buf;
}

/* Encode a packet mask */

BOOL rtp_encode_pmask(CHAR **token, UINT16 ntok, UINT32 *pmask)
{
UINT16 i;

    *pmask = RTP_PMASK_NONE;

    for (i = 0; i < ntok; i++) {
        if (strcasecmp(token[i], "ALL") == 0) {
            *pmask |= RTP_PMASK_ALL;
        } else if (strcasecmp(token[i],  "AD") == 0) {
            *pmask |= RTP_PMASK_AD;
        } else if (strcasecmp(token[i],  "CD") == 0) {
            *pmask |= RTP_PMASK_CD;
        } else if (strcasecmp(token[i],  "DS") == 0) {
            *pmask |= RTP_PMASK_DS;
        } else if (strcasecmp(token[i],  "DT") == 0) {
            *pmask |= RTP_PMASK_DT;
        } else if (strcasecmp(token[i],  "EH") == 0) {
            *pmask |= RTP_PMASK_EH;
        } else if (strcasecmp(token[i],  "ET") == 0) {
            *pmask |= RTP_PMASK_ET;
        } else if (strcasecmp(token[i],  "OM") == 0) {
            *pmask |= RTP_PMASK_OM;
        } else if (strcasecmp(token[i],  "SC") == 0) {
            *pmask |= RTP_PMASK_SC;
        } else if (strcasecmp(token[i],  "SH") == 0) {
            *pmask |= RTP_PMASK_SH;
        } else if (strcasecmp(token[i],  "special") == 0) {
            *pmask |= RTP_PMASK_SPEC;
        } else {
            return FALSE;
        }
    }

    return (*pmask != RTP_PMASK_NONE);
}

/* Decode a stream mask */

CHAR *rtp_decode_smask(UINT32 smask, CHAR *buf)
{
int i;

    buf[0] = 0;
    for (i = 0; i < sizeof(UINT32)*8; i++) {
        if (smask & (1 << i)) sprintf(buf + strlen(buf), "%d ", i+1);
    }
    if (buf[0] == 0) sprintf(buf, "0x%0x defines no known streams", smask);

    return buf;
}

/* Encode a stream mask */

BOOL rtp_encode_smask(CHAR **token, UINT16 ntok, UINT32 *smask)
{
UINT16 i, value;

    *smask = 0;

    for (i = 0; i < ntok; i++) {
        if (strcasecmp(token[i], "ALL") == 0) {
            *smask = 0xffffffff;
        } else if ((value = (UINT16) atoi(token[i])) > 0) {
            *smask |= (1 << (value - 1));
        } else {
            return FALSE;
        }
    }
        
    return (*smask != 0);
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.2  2002/01/18 17:57:48  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
