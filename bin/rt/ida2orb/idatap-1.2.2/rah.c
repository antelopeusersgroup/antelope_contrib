/* @(#)rah.c	1.2 11/22/93 */
/*======================================================================
 *
 *  sacio/ra_sachead.c
 *
 *  Read an ascii SAC header
 *
 *====================================================================*/
#include <stdio.h>
#include "sacio.h"

int sacio_rah(fp, hdr)
FILE *fp;
struct sac_header *hdr;
{
    clearerr(fp);

    fscanf (fp, SACRFCS,
        &(hdr->delta),
        &(hdr->depmin),
        &(hdr->depmax),
        &(hdr->scale), 
        &(hdr->odelta));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->b), 
        &(hdr->e), 
        &(hdr->o), 
        &(hdr->a), 
        &(hdr->internal1));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->t0), 
        &(hdr->t1), 
        &(hdr->t2), 
        &(hdr->t3), 
        &(hdr->t4));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->t5), 
        &(hdr->t6), 
        &(hdr->t7), 
        &(hdr->t8), 
        &(hdr->t9));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->f), 
        &(hdr->resp0), 
        &(hdr->resp1), 
        &(hdr->resp2), 
        &(hdr->resp3));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->resp4), 
        &(hdr->resp5), 
        &(hdr->resp6), 
        &(hdr->resp7), 
        &(hdr->resp8));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->resp9), 
        &(hdr->stla), 
        &(hdr->stlo), 
        &(hdr->stel), 
        &(hdr->stdp));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->evla),
        &(hdr->evlo),
        &(hdr->evel),
        &(hdr->evdp),
        &(hdr->unused1));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->user0),
        &(hdr->user1),
        &(hdr->user2),
        &(hdr->user3),
        &(hdr->user4));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->user5),
        &(hdr->user6),
        &(hdr->user7),
        &(hdr->user8),
        &(hdr->user9));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->dist),
        &(hdr->az),
        &(hdr->baz),
        &(hdr->gcarc),
        &(hdr->internal2));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->internal3),
        &(hdr->depmen),
        &(hdr->cmpaz),
        &(hdr->cmpinc),
        &(hdr->unused2));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->unused3),
        &(hdr->unused4),
        &(hdr->unused5),
        &(hdr->unused6),
        &(hdr->unused7));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRFCS, 
        &(hdr->unused8),
        &(hdr->unused9),
        &(hdr->unused10),
        &(hdr->unused11),
        &(hdr->unused12));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->nzyear),
        &(hdr->nzjday),
        &(hdr->nzhour),
        &(hdr->nzmin),
        &(hdr->nzsec));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->nzmsec),
        &(hdr->internal4),
        &(hdr->internal5),
        &(hdr->internal6),
        &(hdr->npts));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->internal7),
        &(hdr->internal8),
        &(hdr->unused13),
        &(hdr->unused14),
        &(hdr->unused15));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->iftype),
        &(hdr->idep),
        &(hdr->iztype),
        &(hdr->unused16),
        &(hdr->iinst));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->istreg),
        &(hdr->ievreg),
        &(hdr->ievtyp),
        &(hdr->iqual),
        &(hdr->isynth));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->unused17),
        &(hdr->unused18),
        &(hdr->unused19),
        &(hdr->unused20),
        &(hdr->unused21));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->unused22),
        &(hdr->unused23),
        &(hdr->unused24),
        &(hdr->unused25),
        &(hdr->unused26));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRICS, 
        &(hdr->leven),
        &(hdr->lpspol),
        &(hdr->lovrok),
        &(hdr->lcalda),
        &(hdr->unused27));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS2, 
        (hdr->kstnm),
        (hdr->kevnm));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->khole),
        (hdr->ko),
        (hdr->ka));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->kt0),
        (hdr->kt1),
        (hdr->kt2));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->kt3),
        (hdr->kt4),
        (hdr->kt5));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->kt6),
        (hdr->kt7),
        (hdr->kt8));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->kt9),
        (hdr->kf),
        (hdr->kuser0));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->kuser1),
        (hdr->kuser2),
        (hdr->kcmpnm));
    if (ferror(fp)) return -1;

    fscanf (fp, SACRCCS1, 
        (hdr->knetwk),
        (hdr->kdatrd),
        (hdr->kinst));
    if (ferror(fp)) return -1;

    return 0;
}
