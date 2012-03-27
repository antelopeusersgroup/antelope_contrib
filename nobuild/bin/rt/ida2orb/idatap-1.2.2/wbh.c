/* @(#)wbh.c	1.2 11/22/93 */
/*======================================================================
 *
 *  sacio/wb_sachead.c
 *
 *  Write a binary SAC header
 *
 *====================================================================*/
#include <stdio.h>
#include "sacio.h"

int sacio_wbh(fp, hdr)
FILE *fp;
struct sac_header *hdr;
{
int i;

/*  Force the header to contain those "mysterious LLNL set values"  */

    if (hdr->internal1 == -12345.0) hdr->internal1 = 2.0;
    if (hdr->internal4 == -12345)   hdr->internal4 = 6;
    if (hdr->internal5 == -12345)   hdr->internal5 = 0;
    if (hdr->internal6 == -12345)   hdr->internal6 = 0;
    if (hdr->unused27  == -12345)   hdr->unused27  = 0;

/*  Write the header  */

    clearerr(fp);

    fwrite(&hdr->delta,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fwrite(&hdr->depmin,    sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->depmax,    sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->scale,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->odelta,    sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->b,         sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->e,         sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->o,         sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->a,         sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal1, sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t0,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t1,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t2,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t3,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t4,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t5,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t6,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t7,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t8,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->t9,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->f,         sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp0,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp1,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp2,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp3,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp4,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp5,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp6,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp7,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp8,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->resp9,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->stla,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->stlo,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->stel,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->stdp,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->evla,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->evlo,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->evel,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->evdp,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused1,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user0,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user1,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user2,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user3,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user4,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user5,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user6,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user7,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user8,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->user9,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->dist,      sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->az,        sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->baz,       sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->gcarc,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal2, sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal3, sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->depmen,    sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->cmpaz,     sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->cmpinc,    sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused2,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused3,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused4,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused5,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused6,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused7,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused8,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused9,   sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused10,  sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused11,  sizeof(float), 1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused12,  sizeof(float), 1, fp);  if (ferror(fp)) return -1;

    fwrite(&hdr->nzyear,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->nzjday,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->nzhour,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->nzmin,     sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->nzsec,     sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->nzmsec,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal4, sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal5, sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal6, sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->npts,      sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal7, sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->internal8, sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused13,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused14,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused15,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->iftype,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->idep,      sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->iztype,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused16,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->iinst,     sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->istreg,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->ievreg,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->ievtyp,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->iqual,     sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->isynth,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused17,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused18,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused19,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused20,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused21,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused22,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused23,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused24,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused25,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused26,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->leven,     sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->lpspol,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->lovrok,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->lcalda,    sizeof(long),  1, fp);  if (ferror(fp)) return -1;
    fwrite(&hdr->unused27,  sizeof(long),  1, fp);  if (ferror(fp)) return -1;

    for (i = strlen(hdr->kstnm); i < 8; i++) hdr->kstnm[i] = ' ';
    fwrite( hdr->kstnm,     sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kevnm); i < 16; i++) hdr->kevnm[i] = ' ';
    fwrite( hdr->kevnm,     sizeof(char), 16, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->khole); i < 8; i++) hdr->khole[i] = ' ';
    fwrite( hdr->khole,     sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->ko); i < 8; i++) hdr->ko[i] = ' ';
    fwrite( hdr->ko,        sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->ka); i < 8; i++) hdr->ka[i] = ' ';
    fwrite( hdr->ka,        sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt0); i < 8; i++) hdr->kt0[i] = ' ';
    fwrite( hdr->kt0,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt1); i < 8; i++) hdr->kt1[i] = ' ';
    fwrite( hdr->kt1,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt2); i < 8; i++) hdr->kt2[i] = ' ';
    fwrite( hdr->kt2,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt3); i < 8; i++) hdr->kt3[i] = ' ';
    fwrite( hdr->kt3,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt4); i < 8; i++) hdr->kt4[i] = ' ';
    fwrite( hdr->kt4,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt5); i < 8; i++) hdr->kt5[i] = ' ';
    fwrite( hdr->kt5,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt6); i < 8; i++) hdr->kt6[i] = ' ';
    fwrite( hdr->kt6,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt7); i < 8; i++) hdr->kt7[i] = ' ';
    fwrite( hdr->kt7,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt8); i < 8; i++) hdr->kt8[i] = ' ';
    fwrite( hdr->kt8,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kt9); i < 8; i++) hdr->kt9[i] = ' ';
    fwrite( hdr->kt9,       sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kf); i < 8; i++) hdr->kf[i] = ' ';
    fwrite( hdr->kf,        sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kuser0); i < 8; i++) hdr->kuser0[i] = ' ';
    fwrite( hdr->kuser0,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kuser1); i < 8; i++) hdr->kuser1[i] = ' ';
    fwrite( hdr->kuser1,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kuser2); i < 8; i++) hdr->kuser2[i] = ' ';
    fwrite( hdr->kuser2,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kcmpnm); i < 8; i++) hdr->kcmpnm[i] = ' ';
    fwrite( hdr->kcmpnm,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->knetwk); i < 8; i++) hdr->knetwk[i] = ' ';
    fwrite( hdr->knetwk,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kdatrd); i < 8; i++) hdr->kdatrd[i] = ' ';
    fwrite( hdr->kdatrd,    sizeof(char),  8, fp); if (ferror(fp)) return -1;

    for (i = strlen(hdr->kinst); i < 8; i++) hdr->kinst[i] = ' ';
    fwrite( hdr->kinst,     sizeof(char),  8, fp); if (ferror(fp)) return -1;

    return 0;
}
