/* @(#)convert.c	1.3 3/18/97 */
/*======================================================================
 *
 * Convert from generic xfer format to other types
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include "xfer.h"

#ifdef NRTS_SUPPORT
int Xfer_CvtNRTS(cnf, wav, packet)
struct xfer_cnf *cnf;
struct xfer_wav *wav;
struct nrts_packet *packet;
{
static unsigned long host_order = 0;
struct xfer_wavgen1 *wavgen;
struct xfer_cnfgen1 *cnfptr;
int standx, chnndx;

    if (wav->format != XFER_WAVGEN1) return -1;

    if (host_order == 0) host_order = util_order();

    cnfptr = (struct xfer_cnfgen1 *) &cnf->type.gen1;
    wavgen = (struct xfer_wavgen1 *) &wav->type.gen1;

    standx = wavgen->standx;
    chnndx = wavgen->chnndx;
    strcpy(packet->sname, cnfptr->sta[standx].name);
    strcpy(packet->cname, cnfptr->sta[standx].chn[chnndx].name);
    strcpy(packet->instype, cnfptr->sta[standx].chn[chnndx].instype);
    packet->beg.time = wavgen->tofs;
    packet->beg.code = ' ';
    packet->beg.qual = 0;
    packet->tear     = (float) wavgen->tear;
    packet->sint     = cnfptr->sta[standx].chn[chnndx].sint;
    packet->calib    = cnfptr->sta[standx].chn[chnndx].calib;
    packet->calper   = cnfptr->sta[standx].chn[chnndx].calper;
    packet->vang     = cnfptr->sta[standx].chn[chnndx].vang;
    packet->vang     = cnfptr->sta[standx].chn[chnndx].vang;
    packet->hang     = cnfptr->sta[standx].chn[chnndx].hang;
    packet->wrdsiz   = cnfptr->sta[standx].chn[chnndx].wrdsiz;
    packet->nsamp    = wavgen->nsamp;
    packet->order    = host_order;   /* protocol guarantees this */
    packet->type     = 0;
    packet->hlen     = 0;
    packet->dlen     = wavgen->nbyte;
    packet->header   = wavgen->data;
    packet->data     = wavgen->data;
    packet->end.time = packet->beg.time +
                     ((float) (packet->nsamp - 1) * packet->sint);
    packet->end.code = ' ';
    packet->end.qual = 0;

    return 0;
}
#endif /* NRTS_SUPPORT */

int xfer_Convert(cnf, wav, packet)
struct xfer_cnf *cnf;
struct xfer_wav *wav;
struct xfer_packet *packet;
{
long *ldata;
short *sdata;
struct xfer_wavgen1 *wavgen;
struct xfer_cnfgen1 *cnfptr;
int standx, chnndx;
long i, nbytes;

    if (wav->format != XFER_WAVGEN1) return -1;

    cnfptr = (struct xfer_cnfgen1 *) &cnf->type.gen1;
    wavgen = (struct xfer_wavgen1 *) &wav->type.gen1;

    standx = wavgen->standx;
    chnndx = wavgen->chnndx;

    strcpy(packet->sname, cnfptr->sta[standx].name);
    packet->lat   = cnfptr->sta[standx].lat;
    packet->lon   = cnfptr->sta[standx].lon;
    packet->elev  = cnfptr->sta[standx].elev;
    packet->depth = cnfptr->sta[standx].depth;

    strcpy(packet->cname,   cnfptr->sta[standx].chn[chnndx].name);
    strcpy(packet->instype, cnfptr->sta[standx].chn[chnndx].instype);
    packet->sint   = cnfptr->sta[standx].chn[chnndx].sint;
    packet->calib  = cnfptr->sta[standx].chn[chnndx].calib;
    packet->calper = cnfptr->sta[standx].chn[chnndx].calper;
    packet->vang   = cnfptr->sta[standx].chn[chnndx].vang;
    packet->hang   = cnfptr->sta[standx].chn[chnndx].hang;
    packet->beg    = wavgen->tofs;
    packet->tear   = wavgen->tear;
    packet->nsamp  = wavgen->nsamp;

    packet->end = packet->beg + ((packet->nsamp - 1) * packet->sint);

    if (cnfptr->sta[standx].chn[chnndx].wrdsiz == sizeof(long)) {
        packet->data = (long *) wavgen->data;
    } else if (cnfptr->sta[standx].chn[chnndx].wrdsiz == sizeof(short)) {
        nbytes = packet->nsamp * cnfptr->sta[standx].chn[chnndx].wrdsiz;
        sdata = (short *) wavgen->data;
        ldata = (long *)  packet->dbuf;
        for (i = 0; i < packet->nsamp; i++) {
            ldata[i] = (long) sdata[i];
        }
        packet->data = ldata;
    } else {
        return -1;
    }

    return 0;
}
