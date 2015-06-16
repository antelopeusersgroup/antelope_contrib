#pragma ident "$Id$"
/*======================================================================
 *
 *  Create packet header summary strings
 *
 *====================================================================*/
#include "reftek.h"

static CHAR *format_string(UINT16 format)
{
static CHAR *F16 = "16";
static CHAR *F32 = "32";
static CHAR *FC0 = "C0";
static CHAR *BAD = "??";

    switch (format) {
      case REFTEK_FC0: return FC0;
      case REFTEK_F32: return F32;
      case REFTEK_F16: return F16;
      default: return BAD;
    }
}

CHAR *reftek_adstr(struct reftek_ad *ad, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "AD ");
    sprintf(buf + strlen(buf), "%03d ", ad->exp);
    sprintf(buf + strlen(buf), "%04X ", ad->unit);
    sprintf(buf + strlen(buf), "%05d ", ad->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(ad->tstamp, 0, sbuf));
    return buf;
}

CHAR *reftek_cdstr(struct reftek_cd *cd, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "CD ");
    sprintf(buf + strlen(buf), "%03d ", cd->exp);
    sprintf(buf + strlen(buf), "%04X ", cd->unit);
    sprintf(buf + strlen(buf), "%05d ", cd->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(cd->tstamp, 0, sbuf));
    return buf;
}

CHAR *reftek_dsstr(struct reftek_ds *ds, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "DS ");
    sprintf(buf + strlen(buf), "%03d ", ds->exp);
    sprintf(buf + strlen(buf), "%04X ", ds->unit);
    sprintf(buf + strlen(buf), "%05d ", ds->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(ds->tstamp, 0, sbuf));
    return buf;
}

CHAR *reftek_dtstr(struct reftek_dt *dt, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "DT ");
    sprintf(buf + strlen(buf), "%03d ", dt->exp);
    sprintf(buf + strlen(buf), "%04X ", dt->unit);
    sprintf(buf + strlen(buf), "%05d ", dt->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(dt->tstamp, 0, sbuf));

    sprintf(buf + strlen(buf), "%05d ", dt->evtno);
    sprintf(buf + strlen(buf), "%02d ", dt->stream);
    sprintf(buf + strlen(buf), "%02d ", dt->chan);
    sprintf(buf + strlen(buf), "%05d ", dt->nsamp);
    sprintf(buf + strlen(buf), "%s ",   format_string(dt->format));
    if (dt->data != (INT32 *) NULL) {
        sprintf(buf + strlen(buf), "%6ld", dt->data[0]);
    }

    return buf;
}

CHAR *reftek_ehstr(struct reftek_eh *eh, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "EH ");
    sprintf(buf + strlen(buf), "%03d ", eh->exp);
    sprintf(buf + strlen(buf), "%04X ", eh->unit);
    sprintf(buf + strlen(buf), "%05d ", eh->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(eh->tstamp, 0, sbuf));

    sprintf(buf + strlen(buf), "%05d ", eh->evtno);
    sprintf(buf + strlen(buf), "%02d ", eh->stream);
    sprintf(buf + strlen(buf), "%s ",   format_string(eh->format));
    sprintf(buf + strlen(buf), "%6.3f ",eh->sint);
    sprintf(buf + strlen(buf), "%d ",   eh->trgtype);
    sprintf(buf + strlen(buf), "%s",    util_dttostr(eh->on, 0, sbuf));

    return buf;
}

CHAR *reftek_etstr(struct reftek_et *et, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "ET ");
    sprintf(buf + strlen(buf), "%03d ", et->exp);
    sprintf(buf + strlen(buf), "%04X ", et->unit);
    sprintf(buf + strlen(buf), "%05d ", et->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(et->tstamp, 0, sbuf));

    sprintf(buf + strlen(buf), "%05d ", et->evtno);
    sprintf(buf + strlen(buf), "%02d ", et->stream);
    sprintf(buf + strlen(buf), "%s ",   format_string(et->format));
    sprintf(buf + strlen(buf), "%6.3f ",et->sint);
    sprintf(buf + strlen(buf), "%d ",   et->trgtype);
    sprintf(buf + strlen(buf), "%s",    util_dttostr(et->off, 0, sbuf));

    return buf;
}

CHAR *reftek_omstr(struct reftek_om *om, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "OM ");
    sprintf(buf + strlen(buf), "%03d ", om->exp);
    sprintf(buf + strlen(buf), "%04X ", om->unit);
    sprintf(buf + strlen(buf), "%05d ", om->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(om->tstamp, 0, sbuf));
    return buf;
}

CHAR *reftek_scstr(struct reftek_sc *sc, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "SC ");
    sprintf(buf + strlen(buf), "%03d ", sc->exp);
    sprintf(buf + strlen(buf), "%04X ", sc->unit);
    sprintf(buf + strlen(buf), "%05d ", sc->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(sc->tstamp, 0, sbuf));
    sprintf(buf + strlen(buf), "%2d  ", sc->nchan);
    return buf;
}

CHAR *reftek_shstr(struct reftek_sh *sh, CHAR *buf)
{
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    sprintf(buf, "SH ");
    sprintf(buf + strlen(buf), "%03d ", sh->exp);
    sprintf(buf + strlen(buf), "%04X ", sh->unit);
    sprintf(buf + strlen(buf), "%05d ", sh->seqno);
    sprintf(buf + strlen(buf), "%s ",   util_dttostr(sh->tstamp, 0, sbuf));
    return buf;
}

static CHAR *decode_err(CHAR *prefix, CHAR *buf)
{
    sprintf(buf, "unable to decode `%s' packet!", prefix);
    return buf;
}

CHAR *reftek_comstr(UINT8 *pkt, CHAR *buf)
{
UINT16 unit, exp, seqno, i;
REAL64 tstamp;
CHAR sbuf[UTIL_MAXTIMESTRLEN];

    switch (reftek_type(pkt)) {
      case REFTEK_SPEC:
        sprintf(buf, "SPK:");
        for (i = 0; i < 12; i++) sprintf(buf + strlen(buf), " %02x", pkt[i]);
        return buf;
      case REFTEK_CMND:
        sprintf(buf, "SPK:");
        for (i = 0; i < 12; i++) sprintf(buf + strlen(buf), " %02x", pkt[i]);
        return buf;
      case 0:
        sprintf(buf, "<** UNRECOGNIZED PACKET: (0x%0x 0x%0x) **>", pkt[0], pkt[1]);
        return buf;
      default:
        reftek_com(pkt, &exp, &unit, &seqno, &tstamp);
        sprintf(buf,               "%c",    pkt[0]);
        sprintf(buf + strlen(buf), "%c ",   pkt[1]);
        sprintf(buf + strlen(buf), "%03d ", exp);
        sprintf(buf + strlen(buf), "%04X ", unit);
        sprintf(buf + strlen(buf), "%05d ", seqno);
        sprintf(buf + strlen(buf), "%s ",   util_dttostr(tstamp, 0, sbuf));
        return buf;
    }
}

CHAR *reftek_str(UINT8 *pkt, CHAR *buf)
{
union {
    struct reftek_ad ad;
    struct reftek_cd cd;
    struct reftek_ds ds;
    struct reftek_dt dt;
    struct reftek_eh eh;
    struct reftek_et et;
    struct reftek_om om;
    struct reftek_sh sh;
    struct reftek_sc sc;
} decoded;
int i;

    switch (reftek_type(pkt)) {

      case REFTEK_AD:
        if (!reftek_ad(&decoded.ad, pkt)) {
            return decode_err("AD", buf);
        } else {
            return reftek_adstr(&decoded.ad, buf);
        }

      case REFTEK_CD:
        if (!reftek_cd(&decoded.cd, pkt)) {
            return decode_err("CD", buf);
        } else {
            return reftek_cdstr(&decoded.cd, buf);
        }

      case REFTEK_DS:
        if (!reftek_ds(&decoded.ds, pkt)) {
            return decode_err("DS", buf);
        } else {
            return reftek_dsstr(&decoded.ds, buf);
        }

      case REFTEK_DT:
        if (!reftek_dt(&decoded.dt, pkt, FALSE)) {
            return decode_err("DT", buf);
        } else {
            return reftek_dtstr(&decoded.dt, buf);
        }

      case REFTEK_EH:
        if (!reftek_eh(&decoded.eh, pkt)) {
            return decode_err("EH", buf);
        } else {
            return reftek_ehstr(&decoded.eh, buf);
        }

      case REFTEK_ET:
        if (!reftek_et(&decoded.et, pkt)) {
            return decode_err("ET", buf);
        } else {
            return reftek_etstr(&decoded.et, buf);
        }

      case REFTEK_OM:
        if (!reftek_om(&decoded.om, pkt)) {
            return decode_err("OM", buf);
        } else {
            return reftek_omstr(&decoded.om, buf);
        }

      case REFTEK_SC:
        if (!reftek_sc(&decoded.sc, pkt)) {
            return decode_err("SC", buf);
        } else {
            return reftek_scstr(&decoded.sc, buf);
        }

      case REFTEK_SH:
        if (!reftek_sh(&decoded.sh, pkt)) {
            return decode_err("SH", buf);
        } else {
            return reftek_shstr(&decoded.sh, buf);
        }

      default:
        buf[0] = 0;
        for (i = 0; i < 16; i++) sprintf(buf+strlen(buf), "%02x ", pkt[i]);
        return buf;
    }
}

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
