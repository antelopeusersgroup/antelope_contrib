/* @(#)rbh.c	1.3 09/04/97 */
/*======================================================================
 *
 *  Read a binary SAC header
 *
 *====================================================================*/
#include <stdio.h>
#include <string.h>
#include "sacio.h"

int sacio_rbh(fp, hdr)
FILE *fp;
struct sac_header *hdr;
{
char *space;

    clearerr(fp);

    fread(&hdr->delta,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->depmin,    sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->depmax,    sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->scale,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->odelta,    sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->b,         sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->e,         sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->o,         sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->a,         sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal1, sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t0,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t1,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t2,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t3,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t4,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t5,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t6,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t7,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t8,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->t9,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->f,         sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp0,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp1,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp2,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp3,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp4,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp5,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp6,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp7,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp8,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->resp9,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->stla,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->stlo,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->stel,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->stdp,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->evla,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->evlo,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->evel,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->evdp,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused1,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user0,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user1,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user2,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user3,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user4,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user5,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user6,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user7,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user8,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->user9,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->dist,      sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->az,        sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->baz,       sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->gcarc,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal2, sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal3, sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->depmen,    sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->cmpaz,     sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->cmpinc,    sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused2,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused3,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused4,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused5,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused6,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused7,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused8,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused9,   sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused10,  sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused11,  sizeof(float), 1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused12,  sizeof(float), 1, fp); if (ferror(fp)) return -1;
 if (ferror(fp)) return -1;
    fread(&hdr->nzyear,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->nzjday,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->nzhour,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->nzmin,     sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->nzsec,     sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->nzmsec,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal4, sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal5, sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal6, sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->npts,      sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal7, sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->internal8, sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused13,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused14,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused15,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->iftype,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->idep,      sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->iztype,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused16,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->iinst,     sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->istreg,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->ievreg,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->ievtyp,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->iqual,     sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->isynth,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused17,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused18,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused19,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused20,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused21,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused22,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused23,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused24,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused25,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused26,  sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->leven,     sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->lpspol,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->lovrok,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->lcalda,    sizeof(long),  1, fp); if (ferror(fp)) return -1;
    fread(&hdr->unused27,  sizeof(long),  1, fp); if (ferror(fp)) return -1;

     if (ferror(fp)) return -1;

    fread( hdr->kstnm,     sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kstnm[8] = 0;
    if ((space = strchr(hdr->kstnm, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kevnm,     sizeof(char), 16, fp); if (ferror(fp)) return -1;
    hdr->kevnm[16] = 0;
    if ((space = strchr(hdr->kevnm, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->khole,     sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->khole[8] = 0;
    if ((space = strchr(hdr->khole, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->ko,        sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->ko[8] = 0;
    if ((space = strchr(hdr->ko, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->ka,        sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->ka[8] = 0;
    if ((space = strchr(hdr->ka, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt0,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt0[8] = 0;
    if ((space = strchr(hdr->kt0, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt1,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt1[8] = 0;
    if ((space = strchr(hdr->kt1, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt2,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt2[8] = 0;
    if ((space = strchr(hdr->kt2, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt3,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt3[8] = 0;
    if ((space = strchr(hdr->kt3, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt4,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt4[8] = 0;
    if ((space = strchr(hdr->kt4, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt5,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt5[8] = 0;
    if ((space = strchr(hdr->kt5, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt6,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt6[8] = 0;
    if ((space = strchr(hdr->kt6, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt7,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt7[8] = 0;
    if ((space = strchr(hdr->kt7, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt8,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt8[8] = 0;
    if ((space = strchr(hdr->kt8, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kt9,       sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kt9[8] = 0;
    if ((space = strchr(hdr->kt9, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kf,        sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kf[8] = 0;
    if ((space = strchr(hdr->kf, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kuser0,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kuser0[8] = 0;
    if ((space = strchr(hdr->kuser0, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kuser1,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kuser1[8] = 0;
    if ((space = strchr(hdr->kuser1, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kuser2,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kuser2[8] = 0;
    if ((space = strchr(hdr->kuser2, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kcmpnm,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kcmpnm[8] = 0;
    if ((space = strchr(hdr->kcmpnm, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->knetwk,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->knetwk[8] = 0;
    if ((space = strchr(hdr->knetwk, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kdatrd,    sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kdatrd[8] = 0;
    if ((space = strchr(hdr->kdatrd, ' ')) != (char *) NULL) *space = 0;

    fread( hdr->kinst,     sizeof(char),  8, fp); if (ferror(fp)) return -1;
    hdr->kinst[8] = 0;
    if ((space = strchr(hdr->kinst, ' ')) != (char *) NULL) *space = 0;

    return 0;
}
