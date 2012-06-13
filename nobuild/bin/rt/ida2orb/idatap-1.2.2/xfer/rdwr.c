/* @(#)rdwr.c	1.7 03/18/97 */
/*======================================================================
 *
 * Read/write routines for FILE I/O of various structures
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include "xfer.h"
#include "util.h"

static size_t ULlen = sizeof (unsigned long);
static size_t Ilen  = sizeof (int);
static size_t Flen  = sizeof (float);
static size_t Tlen  = sizeof (time_t);
static size_t Dlen  = sizeof (double);
static size_t Llen  = sizeof (long);

int Xfer_WriteCnfGen1(fp, cnf)
FILE *fp;
struct xfer_cnfgen1 *cnf;
{
int i, j;

    clearerr(fp);

    fwrite(&cnf->order, 1, ULlen, fp);
    fwrite(&cnf->nsta,  1, Llen,  fp);
    for (i = 0; i < cnf->nsta; i++) {
        fwrite(cnf->sta[i].name,   1, XFER_SNAMLEN+1, fp);
        fwrite(&cnf->sta[i].lat,   1, Flen, fp);
        fwrite(&cnf->sta[i].lon,   1, Flen, fp);
        fwrite(&cnf->sta[i].elev,  1, Flen, fp);
        fwrite(&cnf->sta[i].depth, 1, Flen, fp);
        fwrite(&cnf->sta[i].nchn,  1, Ilen, fp);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fwrite(&cnf->sta[i].chn[j].name,    1, XFER_CNAMLEN+1, fp);
            fwrite(&cnf->sta[i].chn[j].instype, 1, XFER_INAMLEN+1, fp);
            fwrite(&cnf->sta[i].chn[j].wrdsiz , 1, Ilen,  fp);
            fwrite(&cnf->sta[i].chn[j].order,   1, ULlen, fp);
            fwrite(&cnf->sta[i].chn[j].sint,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].calib,   1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].calper,  1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].vang,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].hang,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].beg,     1, Dlen,  fp);
            fwrite(&cnf->sta[i].chn[j].end,     1, Dlen,  fp);
        }
    }

    return ferror(fp);
}

int Xfer_ReadCnfGen1(fp, cnf)
FILE *fp;
struct xfer_cnfgen1 *cnf;
{
int i, j;

    clearerr(fp);

    fread(&cnf->order, 1, ULlen, fp);
    fread(&cnf->nsta,  1, Llen,  fp);
    for (i = 0; i < cnf->nsta; i++) {
        fread(cnf->sta[i].name,   1, XFER_SNAMLEN+1, fp);
        fread(&cnf->sta[i].lat,   1, Flen, fp);
        fread(&cnf->sta[i].lon,   1, Flen, fp);
        fread(&cnf->sta[i].elev,  1, Flen, fp);
        fread(&cnf->sta[i].depth, 1, Flen, fp);
        fread(&cnf->sta[i].nchn,  1, Ilen, fp);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fread(&cnf->sta[i].chn[j].name,    1, XFER_CNAMLEN+1, fp);
            fread(&cnf->sta[i].chn[j].instype, 1, XFER_INAMLEN+1, fp);
            fread(&cnf->sta[i].chn[j].wrdsiz , 1, Ilen,  fp);
            fread(&cnf->sta[i].chn[j].order,   1, ULlen, fp);
            fread(&cnf->sta[i].chn[j].sint,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].calib,   1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].calper,  1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].vang,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].hang,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].beg,     1, Dlen,  fp);
            fread(&cnf->sta[i].chn[j].end,     1, Dlen,  fp);
        }
    }

    return ferror(fp);
}

#ifdef NRTS_SUPPORT

int Xfer_WriteCnfNrts(fp, cnf)
FILE *fp;
struct xfer_cnfnrts *cnf;
{
int i, j;

    clearerr(fp);

    fwrite(&cnf->order, 1, ULlen, fp);
    fwrite(&cnf->nsta,  1, Llen,  fp);
    for (i = 0; i < cnf->nsta; i++) {
        fwrite(cnf->sta[i].name,   1, XFER_SNAMLEN+1, fp);
        fwrite(&cnf->sta[i].lat,   1, Flen, fp);
        fwrite(&cnf->sta[i].lon,   1, Flen, fp);
        fwrite(&cnf->sta[i].elev,  1, Flen, fp);
        fwrite(&cnf->sta[i].depth, 1, Flen, fp);
        fwrite(&cnf->sta[i].nchn,  1, Ilen, fp);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fwrite(&cnf->sta[i].chn[j].name,    1, XFER_CNAMLEN+1, fp);
            fwrite(&cnf->sta[i].chn[j].instype, 1, XFER_INAMLEN+1, fp);
            fwrite(&cnf->sta[i].chn[j].wrdsiz , 1, Ilen,  fp);
            fwrite(&cnf->sta[i].chn[j].order,   1, ULlen, fp);
            fwrite(&cnf->sta[i].chn[j].sint,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].calib,   1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].calper,  1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].vang,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].hang,    1, Flen,  fp);
            fwrite(&cnf->sta[i].chn[j].beg,     1, Dlen,  fp);
            fwrite(&cnf->sta[i].chn[j].end,     1, Dlen,  fp);
            fwrite(&cnf->sta[i].chn[j].type,    1, Ilen,  fp);
            fwrite(&cnf->sta[i].chn[j].hlen,    1, Ilen,  fp);
            fwrite(&cnf->sta[i].chn[j].dlen,    1, Ilen,  fp);
            fwrite(&cnf->sta[i].chn[j].nrec,    1, Llen,  fp);
            fwrite(&cnf->sta[i].chn[j].nhide,   1, Llen,  fp);
            fwrite(&cnf->sta[i].chn[j].latency, 1, Tlen,  fp);
        }
    }

    return ferror(fp);
}

int Xfer_ReadCnfNrts(fp, cnf)
FILE *fp;
struct xfer_cnfnrts *cnf;
{
int i, j;

    clearerr(fp);

    fread(&cnf->order, 1, ULlen, fp);
    fread(&cnf->nsta,  1, Llen,  fp);
    for (i = 0; i < cnf->nsta; i++) {
        fread(cnf->sta[i].name,   1, XFER_SNAMLEN+1, fp);
        fread(&cnf->sta[i].lat,   1, Flen, fp);
        fread(&cnf->sta[i].lon,   1, Flen, fp);
        fread(&cnf->sta[i].elev,  1, Flen, fp);
        fread(&cnf->sta[i].depth, 1, Flen, fp);
        fread(&cnf->sta[i].nchn,  1, Ilen, fp);
        for (j = 0; j < cnf->sta[i].nchn; j++) {
            fread(&cnf->sta[i].chn[j].name,    1, XFER_CNAMLEN+1, fp);
            fread(&cnf->sta[i].chn[j].instype, 1, XFER_INAMLEN+1, fp);
            fread(&cnf->sta[i].chn[j].wrdsiz , 1, Ilen,  fp);
            fread(&cnf->sta[i].chn[j].order,   1, ULlen, fp);
            fread(&cnf->sta[i].chn[j].sint,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].calib,   1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].calper,  1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].vang,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].hang,    1, Flen,  fp);
            fread(&cnf->sta[i].chn[j].beg,     1, Dlen,  fp);
            fread(&cnf->sta[i].chn[j].end,     1, Dlen,  fp);
            fread(&cnf->sta[i].chn[j].type,    1, Ilen,  fp);
            fread(&cnf->sta[i].chn[j].hlen,    1, Ilen,  fp);
            fread(&cnf->sta[i].chn[j].dlen,    1, Ilen,  fp);
            fread(&cnf->sta[i].chn[j].nrec,    1, Llen,  fp);
            fread(&cnf->sta[i].chn[j].nhide,   1, Llen,  fp);
            fread(&cnf->sta[i].chn[j].latency, 1, Tlen,  fp);
        }
    }

    return ferror(fp);
}

#endif /* NRTS_SUPPORT */

int Xfer_WriteWavGen1(fp, wav)
FILE *fp;
struct xfer_wavgen1 *wav;
{
    clearerr(fp);

    fwrite(&wav->standx, 1, Ilen, fp);
    fwrite(&wav->chnndx, 1, Ilen, fp);
    fwrite(&wav->tofs,   1, Dlen, fp);
    fwrite(&wav->tear,   1, Ilen, fp);
    fwrite(&wav->comp,   1, Ilen, fp);
    fwrite(&wav->nsamp,  1, Llen, fp);
    fwrite(&wav->nbyte,  1, Ilen, fp);
    fwrite(wav->data,    1, wav->nbyte, fp);

    return ferror(fp);
}

int Xfer_ReadWavGen1(fp, wav)
FILE *fp;
struct xfer_wavgen1 *wav;
{
    clearerr(fp);

    fread(&wav->standx, 1, Ilen, fp);
    fread(&wav->chnndx, 1, Ilen, fp);
    fread(&wav->tofs,   1, Dlen, fp);
    fread(&wav->tear,   1, Ilen, fp);
    fread(&wav->comp,   1, Ilen, fp);
    fread(&wav->nsamp,  1, Llen, fp);
    fread(&wav->nbyte,  1, Ilen, fp);
    fread(wav->data,    1, wav->nbyte, fp);

    return ferror(fp);
}

#ifdef IDA_SUPPORT

int Xfer_WriteWavIda(fp, wav, strip)
FILE *fp;
struct xfer_wavida *wav;
int strip;
{
int len;

    clearerr(fp);

    if (!strip) {
        fwrite(&wav->rev,    1, Ilen, fp);
        fwrite(&wav->comp,   1, Ilen, fp);
        fwrite(&wav->nbyte,  1, Ilen, fp);
    }
    fwrite(wav->data, 1, wav->nbyte, fp);

    return ferror(fp);
}

int Xfer_ReadWavIda(fp, wav)
FILE *fp;
struct xfer_wavida *wav;
{
    clearerr(fp);

    fread(&wav->rev,    1, Ilen, fp);
    fread(&wav->comp,   1, Ilen, fp);
    fread(&wav->nbyte,  1, Ilen, fp);
    fread(wav->data,    1, wav->nbyte, fp);

    return ferror(fp);
}

#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT

int Xfer_WriteWavSeed(fp, wav, strip)
FILE *fp;
struct xfer_wavseed *wav;
int strip;
{
    clearerr(fp);

    if (!strip) {
        fwrite(&wav->nbyte, 1, Ilen, fp);
        fwrite(wav->data,   1, wav->nbyte, fp);
    } else {
        fwrite(wav->data,   1, SEED_PAKLEN, fp);
    }

    return ferror(fp);
}

int Xfer_ReadWavSeed(fp, wav)
FILE *fp;
struct xfer_wavseed *wav;
{
    clearerr(fp);

    fread(&wav->nbyte, 1, Ilen, fp);
    fread(wav->data,   1, wav->nbyte, fp);

    return ferror(fp);
}

#endif /* SEED_SUPPORT */

int Xfer_WriteWavRaw(fp, wav, strip)
FILE *fp;
struct xfer_wavraw *wav;
int strip;
{
    clearerr(fp);

    if (!strip) fwrite(&wav->nbyte, 1, Ilen, fp);
    fwrite(wav->data,   1, wav->nbyte, fp);

    return ferror(fp);
}

int Xfer_ReadWavRaw(fp, wav)
FILE *fp;
struct xfer_wavraw *wav;
{
    clearerr(fp);

    fread(&wav->nbyte, 1, Ilen, fp);
    fread(wav->data,   1, wav->nbyte, fp);

    return ferror(fp);
}

int Xfer_WriteCnf(fp, cnf)
FILE *fp;
struct xfer_cnf *cnf;
{
    fwrite(&cnf->format, 1, Ilen, fp);
    switch (cnf->format) {
      case XFER_CNFGEN1:
        return Xfer_WriteCnfGen1(fp, &cnf->type.gen1);
        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:
        return Xfer_WriteCnfNrts(fp, &cnf->type.nrts);
        break;
#endif /* NRTS_SUPPORT */

      default:          
        errno = EINVAL;
        return -1;
    }
}

int Xfer_ReadCnf(fp, cnf)
FILE *fp;
struct xfer_cnf *cnf;
{
    fread(&cnf->format, 1, Ilen, fp);
    switch (cnf->format) {
      case XFER_CNFGEN1:
        return Xfer_ReadCnfGen1(fp, &cnf->type.gen1);
        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:
        return Xfer_ReadCnfNrts(fp, &cnf->type.nrts);
        break;
#endif /* NRTS_SUPPORT */

      default:          
        errno = EINVAL;
        return -1;
    }
}

int Xfer_WriteWav(fp, wav, strip)
FILE *fp;
struct xfer_wav *wav;
int strip;
{

    if (!strip) fwrite(&wav->format, 1, Ilen, fp);
    switch (wav->format) {
      case XFER_WAVGEN1:
        return Xfer_WriteWavGen1(fp, &wav->type.gen1);
        break;

      case XFER_WAVRAW:
        return Xfer_WriteWavRaw(fp, &wav->type.raw, strip);
        break;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:
        return Xfer_WriteWavIda(fp, &wav->type.ida, strip);
        break;
#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT
      case XFER_WAVSEED:
        return Xfer_WriteWavSeed(fp, &wav->type.seed, strip);
        break;
#endif /* SEED_SUPPORT */

      case XFER_WAVPASSCAL:
      default:          
        errno = EINVAL;
        return -1;
    }
}

int Xfer_ReadWav(fp, wav)
FILE *fp;
struct xfer_wav *wav;
{

    fread(&wav->format, 1, Ilen, fp);
    switch (wav->format) {
      case XFER_WAVGEN1:
        return Xfer_ReadWavGen1(fp, &wav->type.gen1);
        break;

      case XFER_WAVRAW:
        return Xfer_ReadWavRaw(fp, &wav->type.raw);
        break;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:
        return Xfer_ReadWavIda(fp, &wav->type.ida);
        break;
#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT
      case XFER_WAVSEED:
        return Xfer_ReadWavSeed(fp, &wav->type.seed);
        break;
#endif /* SEED_SUPPORT */

      case XFER_WAVPASSCAL:
      default:          
        errno = EINVAL;
        return -1;
    }
}
