/* @(#)log.c	1.8 03/18/97 */
/*======================================================================
 *
 * Log various xfer structures
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include "xfer.h"
#include "util.h"

#define SB (Xfer_Buffer + strlen(Xfer_Buffer))
#define CB (Xfer_Buffer[0] = 0)

void Xfer_LogReq(level, req)
int level;
struct xfer_req *req;
{
int i, j;
struct xfer01_wavreq *wav01;

    CB;
    util_log(level, "Protocol: 0x%02x", req->protocol);
    sprintf(SB, "Type: 0x%02x ", req->type);
    switch (req->type) {
      case XFER_CNFREQ: sprintf(SB, "(configuration request)"); break;
      case XFER_WAVREQ: sprintf(SB, "(waveform request)"); break;
      default:          sprintf(SB, "(UNKNOWN request)");
    }
    util_log(level, Xfer_Buffer); CB;
    util_log(level, "Timeout: %ld seconds", req->timeout);
    switch (req->protocol) {
      case 0x01:
        util_log(level, "Client Id: %ld", req->preamble.ver01.client_id);
        sprintf(SB, "Config format: 0x%02x ", req->preamble.ver01.format);
        switch (req->preamble.ver01.format) {
          case XFER_CNFGEN1: sprintf(SB, "(generic)"); break;
          case XFER_CNFNRTS: sprintf(SB, "(NRTS)"); break;
          default:           sprintf(SB, "(UNKNOWN format)");
        }
        util_log(level, Xfer_Buffer); CB;
        break;
      default:
        util_log(level, "Unsupported protocol version!");
        return;
    }
    if (req->type == XFER_CNFREQ) return;
    if (req->type != XFER_WAVREQ) {
        util_log(level, "Unsupported request type!");
        return;
    }
    util_log(level, "\tWaveform request parameters:");
    switch (req->protocol) {
      case 0x01:
        wav01 = (struct xfer01_wavreq *) &req->request.wav.ver01;
        util_log(level, "\t\tkeep up = %d", wav01->keepup);
        sprintf(SB,   "\t\tcomp    = %d ", wav01->comp);
        switch (wav01->comp) {
          case XFER_CMPNONE: sprintf(SB, "(no compression)"); break;
          case XFER_CMPIGPP: sprintf(SB, "(IGPP first difference)"); break;
          case XFER_CMPSTM1: sprintf(SB, "(Steim 1)"); break;
          default:           sprintf(SB, "(UNKNOWN compression)");
        }
        util_log(level, Xfer_Buffer); CB;
        sprintf(SB, "\t\tformat  = %d ", wav01->format);
        switch (wav01->format) {
          case XFER_WAVGEN1:    sprintf(SB, "(generic)"); break;
          case XFER_WAVRAW:     sprintf(SB, "(raw)"); break;
          case XFER_WAVIDA:     sprintf(SB, "(IDA)"); break;
          case XFER_WAVSEED:    sprintf(SB, "(SEED)"); break;
          case XFER_WAVPASSCAL: sprintf(SB, "(PASSCAL)"); break;
          default:              sprintf(SB, "(UNKNOWN format)");
        }
        util_log(level, Xfer_Buffer); CB;
        util_log(level, "\t\t%d stations:", wav01->nsta);
        for (i = 0; i < wav01->nsta; i++) {
            util_log(level, "\t\t\t%d channels of station %s",
                wav01->sta[i].nchn, wav01->sta[i].name
            );
            for (j = 0; j < wav01->sta[i].nchn; j++) {
                sprintf(SB, "\t\t\t\t%s ", wav01->sta[i].chn[j].name);
                if (wav01->sta[i].chn[j].beg == XFER_OLDEST) {
                    sprintf(SB, "begining of disk loop ");
                } else if (wav01->sta[i].chn[j].beg == XFER_YNGEST) {
                    sprintf(SB, "     end of disk loop ");
                } else {
                    sprintf(SB, "%s ",
                        util_dttostr(wav01->sta[i].chn[j].beg,0)
                    );
                }
                if (wav01->sta[i].chn[j].end == XFER_OLDEST) {
                    sprintf(SB, "begining of disk loop");
                } else if (wav01->sta[i].chn[j].end == XFER_YNGEST) {
                    sprintf(SB, "     end of disk loop");
                } else {
                    sprintf(SB, "%s",
                        util_dttostr(wav01->sta[i].chn[j].end,0)
                    );
                }
                util_log(level, Xfer_Buffer); CB;
            }
        }
        break;

      default:
        util_log(level, "Unsupported protocol version!");
        return;
    }
}

void xfer_LogCnfGen1(level, cnf)
int level;
struct xfer_cnfgen1 *cnf;
{
int i, j;

    CB;
    sprintf(SB, "remote byte order: 0x%08x ", cnf->order);
    if (cnf->order == BIG_ENDIAN_ORDER) {
        sprintf(SB, "(big endian)");
    } else if (cnf->order == LTL_ENDIAN_ORDER) {
        sprintf(SB, "(little endian)");
    } else {
        sprintf(SB, "(UNKNOWN order)");
    }
    util_log(level, Xfer_Buffer); CB;

    util_log(level, "%d stations:", cnf->nsta);
    for (i = 0; i < cnf->nsta; i++) {
        util_log(level, "\tname  = %s",   cnf->sta[i].name);
        util_log(level, "\tlat   = %.4f", cnf->sta[i].lat);
        util_log(level, "\tlon   = %.4f", cnf->sta[i].lon);
        util_log(level, "\telev  = %.4f", cnf->sta[i].elev);
        util_log(level, "\tdepth = %.4f", cnf->sta[i].depth);
        util_log(level, "\t%d channels:", cnf->sta[i].nchn);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            util_log(level, "\t\tname    = %s",cnf->sta[i].chn[j].name);
            util_log(level, "\t\tinstype = %s",cnf->sta[i].chn[j].instype);
            util_log(level, "\t\twrdsiz  = %d",cnf->sta[i].chn[j].wrdsiz );
            sprintf(SB, "\t\torder   = 0x%08x ", cnf->sta[i].chn[j].order);
            if (cnf->sta[i].chn[j].order == BIG_ENDIAN_ORDER) {
                sprintf(SB, "(big endian)");
            } else if (cnf->sta[i].chn[j].order == LTL_ENDIAN_ORDER) {
                sprintf(SB, "(little endian)");
            } else {
                sprintf(SB, "(UNKNOWN order)");
            }
            util_log(level, Xfer_Buffer); CB;
            util_log(level, "\t\tsint    = %8.3f",cnf->sta[i].chn[j].sint);
            util_log(level, "\t\tcalib   = %f",cnf->sta[i].chn[j].calib);
            util_log(level, "\t\tcalper  = %f",cnf->sta[i].chn[j].calper);
            util_log(level, "\t\tvang    = %.3f",cnf->sta[i].chn[j].vang);
            util_log(level, "\t\thang    = %.3f",cnf->sta[i].chn[j].hang);
            util_log(level, "\t\tbeg     = %s",
                util_dttostr(cnf->sta[i].chn[j].beg, 0)
            );
            util_log(level, "\t\tend     = %s",   
                util_dttostr(cnf->sta[i].chn[j].end, 0)
            );
        }
    }
}

#ifdef NRTS_SUPPORT
void xfer_LogCnfNrts(level, cnf)
int level;
struct xfer_cnfnrts *cnf;
{
int i, j;

    CB;
    sprintf(SB, "remote byte order: 0x%08x ", cnf->order);
    if (cnf->order == BIG_ENDIAN_ORDER) {
        sprintf(SB, "(big endian)");
    } else if (cnf->order == LTL_ENDIAN_ORDER) {
        sprintf(SB, "(little endian)");
    } else {
        sprintf(SB, "(UNKNOWN order)");
    }
    util_log(level, Xfer_Buffer); CB;

    util_log(level, "%d stations:", cnf->nsta);
    for (i = 0; i < cnf->nsta; i++) {
        util_log(level, "\tname  = %s",   cnf->sta[i].name);
        util_log(level, "\tlat   = %.4f", cnf->sta[i].lat);
        util_log(level, "\tlon   = %.4f", cnf->sta[i].lon);
        util_log(level, "\telev  = %.4f", cnf->sta[i].elev);
        util_log(level, "\tdepth = %.4f", cnf->sta[i].depth);
        util_log(level, "\t%d channels:", cnf->sta[i].nchn);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            util_log(level, "\t\tname    = %s", cnf->sta[i].chn[j].name);
            util_log(level, "\t\tinstype = %s", cnf->sta[i].chn[j].instype);
            util_log(level, "\t\twrdsiz  = %d", cnf->sta[i].chn[j].wrdsiz );
            sprintf(SB, "\t\torder   = 0x%08x ", cnf->sta[i].chn[j].order);
            if (cnf->sta[i].chn[j].order == BIG_ENDIAN_ORDER) {
                sprintf(SB, "(big endian)");
            } else if (cnf->sta[i].chn[j].order == LTL_ENDIAN_ORDER) {
                sprintf(SB, "(little endian)");
            } else {
                sprintf(SB, "(UNKNOWN order)");
            }
            util_log(level, Xfer_Buffer); CB;
            util_log(level, "\t\tsint    = %8.3f",cnf->sta[i].chn[j].sint);
            util_log(level, "\t\tcalib   = %f",cnf->sta[i].chn[j].calib);
            util_log(level, "\t\tcalper  = %f",cnf->sta[i].chn[j].calper);
            util_log(level, "\t\tvang    = %.3f",cnf->sta[i].chn[j].vang);
            util_log(level, "\t\thang    = %.3f",cnf->sta[i].chn[j].hang);
            util_log(level, "\t\tbeg     = %s",
                util_dttostr(cnf->sta[i].chn[j].beg, 0)
            );
            util_log(level, "\t\tend     = %s",   
                util_dttostr(cnf->sta[i].chn[j].end, 0)
            );
            sprintf(SB, "\t\ttype    = 0x%02x ", cnf->sta[i].chn[j].type);
            if (cnf->sta[i].chn[j].type == NRTS_IDA) {
                sprintf(SB, "(IDA)");
            } else {
                sprintf(SB, "(UNKNOWN type)");
            }
            util_log(level, Xfer_Buffer); CB;
            util_log(level, "\t\thlen    = %d", cnf->sta[i].chn[j].hlen);
            util_log(level, "\t\tdlen    = %d", cnf->sta[i].chn[j].dlen);
            util_log(level, "\t\tnrec    = %d", cnf->sta[i].chn[j].nrec);
            util_log(level, "\t\tnhide   = %d", cnf->sta[i].chn[j].nhide);
            util_log(level, "\t\tlatency = %s",
                nrts_latency(cnf->sta[i].chn[j].latency)
            );
        }
    }
}
#endif /* NRTS_SUPPORT */

void xfer_LogWavGen1(level, wav)
int level;
struct xfer_wavgen1 *wav;
{
    CB;
    sprintf(SB, "%4d ", wav->standx);
    sprintf(SB, "%4d ", wav->chnndx);
    sprintf(SB, "%s ",  util_dttostr(wav->tofs, 0));
    sprintf(SB, "%5d ", wav->nsamp);
    sprintf(SB, "%5d ", wav->nbyte);
    sprintf(SB, "%d ",  wav->tear);
    sprintf(SB, "%d ",  wav->comp);
    switch (wav->comp) {
      case XFER_CMPNONE: sprintf(SB, "(no compression)"); break;
      case XFER_CMPIGPP: sprintf(SB, "(IGPP first difference)"); break;
      case XFER_CMPSTM1: sprintf(SB, "(Steim 1)"); break;
      default:           sprintf(SB, "(UNKNOWN compression)");
    }
    util_log(level, Xfer_Buffer); CB;
}

#ifdef IDA_SUPPORT
void xfer_LogWavIda(level, wav)
int level;
struct xfer_wavida *wav;
{
int status;
struct data_head dhead;

    if ((status = iris_dhead(&dhead, wav->data, wav->rev)) != 0) {
        util_log(level, "can't decode header: status `%d'", status);
        return;
    }

    util_log(level, "%s", nrts_idahstr(&dhead));
}
#endif /* IDA_SUPPORT */

void Xfer_LogCnf(level, cnf)
int level;
struct xfer_cnf *cnf;
{

    CB;
    sprintf(SB, "Format: 0x%02x ", cnf->format);
    switch (cnf->format) {
      case XFER_CNFGEN1:
        sprintf(SB, "(generic)");
        util_log(level, Xfer_Buffer); CB;
        xfer_LogCnfGen1(level, &cnf->type.gen1);
        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:
        sprintf(SB, "(NRTS)");
        util_log(level, Xfer_Buffer); CB;
        xfer_LogCnfNrts(level, &cnf->type.nrts);
        break;
#endif /* NRTS_SUPPORT */

      default:          
        sprintf(SB, "(UNKNOWN format)");
        util_log(level, Xfer_Buffer); CB;
        return;
    }
}

void Xfer_LogWav(level, wav)
int level;
struct xfer_wav *wav;
{

    switch (wav->format) {
      case XFER_WAVGEN1:
        xfer_LogWavGen1(level, &wav->type.gen1);
        break;

      case XFER_WAVRAW:
        util_log(level, "raw waveform packet");
        break;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:
        xfer_LogWavIda(level, &wav->type.ida);
        break;
#endif /* IDA_SUPPORT */

      case XFER_WAVSEED:
      case XFER_WAVPASSCAL:
      default:          
        util_log(level, "Unsupported waveform packet");
        return;
    }
}
