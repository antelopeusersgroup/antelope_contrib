/* @(#)print.c	1.9 03/18/97 */
/*======================================================================
 *
 * Print various xfer structures
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include "xfer.h"
#include "util.h"

void Xfer_PrintReq(fp, req)
FILE *fp;
struct xfer_req *req;
{
int i, j;
struct xfer01_wavreq *wav01;

    fprintf(fp, "Protocol: 0x%02x\n", req->protocol);
    fprintf(fp, "Type: 0x%02x ", req->type);
    switch (req->type) {
      case XFER_CNFREQ: fprintf(fp, "(configuration request)"); break;
      case XFER_WAVREQ: fprintf(fp, "(waveform request)"); break;
      default:          fprintf(fp, "(UNKNOWN request)");
    }
    fprintf(fp, "\n");
    fprintf(fp, "Timeout: %ld seconds\n", req->timeout);
    switch (req->protocol) {
      case 0x01:
        fprintf(fp, "Client Id: %ld\n", req->preamble.ver01.client_id);
        fprintf(fp, "Config format: 0x%02x ", req->preamble.ver01.format);
        switch (req->preamble.ver01.format) {
          case XFER_CNFGEN1: fprintf(fp, "(generic)"); break;
          case XFER_CNFNRTS: fprintf(fp, "(NRTS)"); break;
          default:           fprintf(fp, "(UNKNOWN format)");
        }
        fprintf(fp, "\n");
        break;
      default:
        fprintf(fp, "Unsupported protocol version!\n");
        return;
    }
    if (req->type == XFER_CNFREQ) return;
    if (req->type != XFER_WAVREQ) {
        fprintf(fp, "Unsupported request type!\n");
        return;
    }
    fprintf(fp, "\tWaveform request parameters:\n");
    switch (req->protocol) {
      case 0x01:
        wav01 = (struct xfer01_wavreq *) &req->request.wav.ver01;
        fprintf(fp, "\t\tkeep up = %d\n", wav01->keepup);
        fprintf(fp, "\t\tcomp    = %d ", wav01->comp);
        switch (wav01->comp) {
          case XFER_CMPNONE: fprintf(fp, "(no compression)"); break;
          case XFER_CMPIGPP: fprintf(fp, "(IGPP first difference)"); break;
          case XFER_CMPSTM1: fprintf(fp, "(Steim 1)"); break;
          default:           fprintf(fp, "(UNKNOWN compression)");
        }
        fprintf(fp, "\n");
        fprintf(fp, "\t\tformat  = %d ", wav01->format);
        switch (wav01->format) {
          case XFER_WAVRAW:     fprintf(fp, "(raw)"); break;
          case XFER_WAVIDA:     fprintf(fp, "(IDA)"); break;
          case XFER_WAVSEED:    fprintf(fp, "(SEED)"); break;
          case XFER_WAVPASSCAL: fprintf(fp, "(PASSCAL)"); break;
          default:              fprintf(fp, "(UNKNOWN format)");
        }
        fprintf(fp, "\n");
        fprintf(fp, "\t\t%d stations:\n", wav01->nsta);
        for (i = 0; i < wav01->nsta; i++) {
            fprintf(fp, "\t\t\t%d channels of station %s\n",
                wav01->sta[i].nchn, wav01->sta[i].name
            );
            for (j = 0; j < wav01->sta[i].nchn; j++) {
                fprintf(fp, "\t\t\t\t%s ", wav01->sta[i].chn[j].name);
                if (wav01->sta[i].chn[j].beg == XFER_OLDEST) {
                    fprintf(fp, "begining of disk loop ");
                } else if (wav01->sta[i].chn[j].beg == XFER_YNGEST) {
                    fprintf(fp, "     end of disk loop ");
                } else {
                    fprintf(fp, "%s ",
                        util_dttostr(wav01->sta[i].chn[j].beg,0)
                    );
                }
                if (wav01->sta[i].chn[j].end == XFER_OLDEST) {
                    fprintf(fp, "begining of disk loop");
                } else if (wav01->sta[i].chn[j].end == XFER_YNGEST) {
                    fprintf(fp, "     end of disk loop");
                } else {
                    fprintf(fp, "%s",
                        util_dttostr(wav01->sta[i].chn[j].end,0)
                    );
                }
                fprintf(fp, "\n");
            }
        }
        break;

      default:
        fprintf(fp, "Unsupported protocol version!\n");
        return;
    }
}

void xfer_PrintCnfGen1(fp, cnf)
FILE *fp;
struct xfer_cnfgen1 *cnf;
{
int i, j;

    fprintf(fp, "remote byte order: 0x%08x ", cnf->order);
    if (cnf->order == BIG_ENDIAN_ORDER) {
        fprintf(fp, "(big endian)");
    } else if (cnf->order == LTL_ENDIAN_ORDER) {
        fprintf(fp, "(little endian)");
    } else {
        fprintf(fp, "(UNKNOWN order)");
    }
    fprintf(fp, "\n");

    fprintf(fp, "%d stations:\n", cnf->nsta);
    for (i = 0; i < cnf->nsta; i++) {
        fprintf(fp, "\n");
        fprintf(fp, "\tname  = %s\n",   cnf->sta[i].name);
        fprintf(fp, "\tlat   = %.4f\n", cnf->sta[i].lat);
        fprintf(fp, "\tlon   = %.4f\n", cnf->sta[i].lon);
        fprintf(fp, "\telev  = %.4f\n", cnf->sta[i].elev);
        fprintf(fp, "\tdepth = %.4f\n", cnf->sta[i].depth);
        fprintf(fp, "\t%d channels:\n", cnf->sta[i].nchn);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fprintf(fp, "\n");
            fprintf(fp, "\t\tname    = %s\n",   cnf->sta[i].chn[j].name);
            fprintf(fp, "\t\tinstype = %s\n",   cnf->sta[i].chn[j].instype);
            fprintf(fp, "\t\twrdsiz  = %d\n",   cnf->sta[i].chn[j].wrdsiz );
            fprintf(fp, "\t\torder   = 0x%08x ", cnf->sta[i].chn[j].order);
            if (cnf->sta[i].chn[j].order == BIG_ENDIAN_ORDER) {
                fprintf(fp, "(big endian)");
            } else if (cnf->sta[i].chn[j].order == LTL_ENDIAN_ORDER) {
                fprintf(fp, "(little endian)");
            } else {
                fprintf(fp, "(UNKNOWN order)");
            }
            fprintf(fp, "\n");
            fprintf(fp, "\t\tsint    = %.3f\n", cnf->sta[i].chn[j].sint);
            fprintf(fp, "\t\tcalib   = %f\n",   cnf->sta[i].chn[j].calib);
            fprintf(fp, "\t\tcalper  = %f\n",   cnf->sta[i].chn[j].calper);
            fprintf(fp, "\t\tvang    = %.3f\n", cnf->sta[i].chn[j].vang);
            fprintf(fp, "\t\thang    = %.3f\n", cnf->sta[i].chn[j].hang);
            fprintf(fp, "\t\tbeg     = %s\n",
                util_dttostr(cnf->sta[i].chn[j].beg, 0)
            );
            fprintf(fp, "\t\tend     = %s\n",   
                util_dttostr(cnf->sta[i].chn[j].end, 0)
            );
        }
    }
}

#ifdef NRTS_SUPPORT
void xfer_PrintCnfNrts(fp, cnf)
FILE *fp;
struct xfer_cnfnrts *cnf;
{
int i, j;

    fprintf(fp, "remote byte order: 0x%08x ", cnf->order);
    if (cnf->order == BIG_ENDIAN_ORDER) {
        fprintf(fp, "(big endian)");
    } else if (cnf->order == LTL_ENDIAN_ORDER) {
        fprintf(fp, "(little endian)");
    } else {
        fprintf(fp, "(UNKNOWN order)");
    }
    fprintf(fp, "\n");

    fprintf(fp, "%d stations:\n", cnf->nsta);
    for (i = 0; i < cnf->nsta; i++) {
        fprintf(fp, "\n");
        fprintf(fp, "\tname  = %s\n",   cnf->sta[i].name);
        fprintf(fp, "\tlat   = %.4f\n", cnf->sta[i].lat);
        fprintf(fp, "\tlon   = %.4f\n", cnf->sta[i].lon);
        fprintf(fp, "\telev  = %.4f\n", cnf->sta[i].elev);
        fprintf(fp, "\tdepth = %.4f\n", cnf->sta[i].depth);
        fprintf(fp, "\t%d channels:\n", cnf->sta[i].nchn);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fprintf(fp, "\n");
            fprintf(fp, "\t\tname    = %s\n",   cnf->sta[i].chn[j].name);
            fprintf(fp, "\t\tinstype = %s\n",   cnf->sta[i].chn[j].instype);
            fprintf(fp, "\t\twrdsiz  = %d\n",   cnf->sta[i].chn[j].wrdsiz );
            fprintf(fp, "\t\torder   = 0x%08x ", cnf->sta[i].chn[j].order);
            if (cnf->sta[i].chn[j].order == BIG_ENDIAN_ORDER) {
                fprintf(fp, "(big endian)");
            } else if (cnf->sta[i].chn[j].order == LTL_ENDIAN_ORDER) {
                fprintf(fp, "(little endian)");
            } else {
                fprintf(fp, "(UNKNOWN order)");
            }
            fprintf(fp, "\n");
            fprintf(fp, "\t\tsint    = %.3f\n", cnf->sta[i].chn[j].sint);
            fprintf(fp, "\t\tcalib   = %f\n",   cnf->sta[i].chn[j].calib);
            fprintf(fp, "\t\tcalper  = %f\n",   cnf->sta[i].chn[j].calper);
            fprintf(fp, "\t\tvang    = %.3f\n", cnf->sta[i].chn[j].vang);
            fprintf(fp, "\t\thang    = %.3f\n", cnf->sta[i].chn[j].hang);
            fprintf(fp, "\t\tbeg     = %s\n",
                util_dttostr(cnf->sta[i].chn[j].beg, 0)
            );
            fprintf(fp, "\t\tend     = %s\n",   
                util_dttostr(cnf->sta[i].chn[j].end, 0)
            );
            fprintf(fp, "\t\ttype    = 0x%02x ", cnf->sta[i].chn[j].type);
            if (cnf->sta[i].chn[j].type == NRTS_IDA) {
                fprintf(fp, "(IDA)\n");
            } else {
                fprintf(fp, "(UNKNOWN type)\n");
            }
            fprintf(fp, "\t\thlen    = %d\n", cnf->sta[i].chn[j].hlen);
            fprintf(fp, "\t\tdlen    = %d\n", cnf->sta[i].chn[j].dlen);
            fprintf(fp, "\t\tnrec    = %d\n", cnf->sta[i].chn[j].nrec);
            fprintf(fp, "\t\tnhide   = %d\n", cnf->sta[i].chn[j].nhide);
            fprintf(fp, "\t\tlatency = %s\n",
                nrts_latency(cnf->sta[i].chn[j].latency)
            );
        }
    }
}
#endif /* NRTS_SUPPORT */

void xfer_PrintWavGen1(fp, wav)
FILE *fp;
struct xfer_wavgen1 *wav;
{
    fprintf(fp, "\n");
    fprintf(fp, "%4d ", wav->standx);
    fprintf(fp, "%4d ", wav->chnndx);
    fprintf(fp, "%s ",  util_dttostr(wav->tofs, 0));
    fprintf(fp, "%5d ", wav->nsamp);
    fprintf(fp, "%5d ", wav->nbyte);
    fprintf(fp, "%d ",  wav->tear);
    fprintf(fp, "%d ",  wav->comp);
    switch (wav->comp) {
      case XFER_CMPNONE: fprintf(fp, "(no compression)"); break;
      case XFER_CMPIGPP: fprintf(fp, "(IGPP first difference)"); break;
      case XFER_CMPSTM1: fprintf(fp, "(Steim 1)"); break;
      default:           fprintf(fp, "(UNKNOWN compression)");
    }
    fprintf(fp, "\n");
}

#ifdef IDA_SUPPORT
void xfer_PrintWavIda(fp, wav)
FILE *fp;
struct xfer_wavida *wav;
{
int status;
struct data_head dhead;

    fprintf(fp, "%d ", wav->rev);
    fprintf(fp, "%5d ", wav->nbyte);
    fprintf(fp, "%d ",  wav->comp);
    switch (wav->comp) {
      case XFER_CMPNONE: fprintf(fp, "(no compression)"); break;
      case XFER_CMPIGPP: fprintf(fp, "(IGPP first difference)"); break;
      case XFER_CMPSTM1: fprintf(fp, "(Steim 1)"); break;
      default:           fprintf(fp, "(UNKNOWN compression)");
    }
    fprintf(fp, "\n");

    if ((status = iris_dhead(&dhead, wav->data, wav->rev)) != 0) {
        fprintf(fp, "can't decode header: status `%d'\n", status);
        return;
    }

    fprintf(fp, "%s\n", nrts_idahstr(&dhead));
}
#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT
void xfer_PrintWavSeed(fp, wav)
FILE *fp;
struct xfer_wavseed *wav;
{
struct seed_fsdh fsdh;

    fprintf(fp, "%5d ", wav->nbyte);
    seed_fsdh(&fsdh, wav->data);
    fprintf(fp, "%5s %2s %3s %2s ",
        fsdh.staid, fsdh.locid, fsdh.chnid, fsdh.netid
    );
    fprintf(fp, "%s ", util_dttostr(fsdh.start, 0));
    fprintf(fp, "%5d ", fsdh.nsamp);
    fprintf(fp, "%.3lf", seed_makesint(fsdh.srfact, fsdh.srmult));
    fprintf(fp, "0x%02x ", fsdh.active);
    fprintf(fp, "0x%02x ", fsdh.ioclck);
    fprintf(fp, "0x%02x ", fsdh.qual);
    fprintf(fp, "\n");
}
#endif

void Xfer_PrintCnf(fp, cnf)
FILE *fp;
struct xfer_cnf *cnf;
{

    fprintf(fp, "Format: 0x%02x ", cnf->format);
    switch (cnf->format) {
      case XFER_CNFGEN1:
        fprintf(fp, "(generic)\n");
        xfer_PrintCnfGen1(fp, &cnf->type.gen1);
        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:
        fprintf(fp, "(NRTS)\n");
        xfer_PrintCnfNrts(fp, &cnf->type.nrts);
        break;
#endif /* NRTS_SUPPORT */

      default:          
        fprintf(fp, "(UNKNOWN format)\n");
        return;
    }
}

void Xfer_PrintWav(fp, wav)
FILE *fp;
struct xfer_wav *wav;
{

    fprintf(fp, "Format: 0x%02x ", wav->format);
    switch (wav->format) {
      case XFER_WAVGEN1:
        fprintf(fp, "(generic) ");
        xfer_PrintWavGen1(fp, &wav->type.gen1);
        break;

      case XFER_WAVRAW:
        fprintf(fp, "(raw)\n");
        break;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:
        fprintf(fp, "(IDA) ");
        xfer_PrintWavIda(fp, &wav->type.ida);
        break;
#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT
      case XFER_WAVSEED:
        fprintf(fp, "(SEED) ");
        xfer_PrintWavSeed(fp, &wav->type.seed);
        break;
#endif /* SEED_SUPPORT */

      case XFER_WAVPASSCAL:
      default:          
        fprintf(fp, "(UNKNOWN format)\n");
        return;
    }
}
