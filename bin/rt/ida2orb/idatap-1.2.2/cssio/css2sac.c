/* @(#)css2sac.c	1.5 01/28/97 */
/*======================================================================
 * 
 *  css2sac.c
 *
 *  Given an array of wfdisc records, convert all the data to which
 *  they point to SAC (binary or ASCII).  The data for a particular
 *  channel are collapsed into a single record as long as the gap or
 *  overlap between sequential records is within a user specified
 *  tolerance.
 *
 *  Important!  We *ignore* the sample rate field and just calculate it
 *  for the output files using the start/end times and new number of
 *  samples.  We *assume* that data for a given station/channel have a
 *  constant nominal sample rate between records.  What it is does not
 *  matter, as it is not used.
 *
 *  Output files have names of the form sta.chan[.n] where sta.chan are
 *  the (lower case) station and channel names.  If there are more than
 *  one output files for the same station and channel, then subsequent
 *  files are given the .n appendix, where n=1,2,3...
 *
 *  Returns 0 if completely successful,
 *         -1 if any of the wfdiscs have null chan or unknown datatype
 *         -2 if any of the data files cannot be read
 *         -3 if it cannot open and/or initialize an output file
 *         -4 if it cannot copy data to an output file
 *         -5 if it cannot re-update header on close of output
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <assert.h>
#include <stdio.h>
#include <sys/param.h>
#include <math.h>
#include <stdlib.h>
#include "sacio.h"
#include "cssio.h"
#include "util.h"
#ifdef IDA_SUPPORT
#include "ida.h"
#endif /* IDA_SUPPORT */

#ifdef __STDC__

FILE *css2sac_open(struct wfdisc *, struct sac_header *);
int css2sac_close(FILE *);
int css2sac_compare(const void *, const void *);
int css2sac_cont(struct wfdisc *, struct wfdisc *, int);
int css2sac_copy(FILE *, struct wfdisc *, int);
int css2sac_dof4(FILE *, FILE *, long);
int css2sac_doi2(FILE *, FILE *, long);
int css2sac_doi2swap(FILE *, FILE *, long);
int css2sac_doi4(FILE *, FILE *, long);
int css2sac_doi4swap(FILE *, FILE *, long);
int css2sac_doiftovf(FILE *, FILE *, long);
int css2sac_dovftoif(FILE *, FILE *, long);
int css2sac_sacwrite(FILE *, float *, long);

#else

FILE *css2sac_open();
int css2sac_close();
int css2sac_compare();
int css2sac_cont();
int css2sac_copy();
int css2sac_dof4();
int css2sac_doi2();
int css2sac_doi2swap();
int css2sac_doi4();
int css2sac_doi4swap();
int css2sac_doiftovf();
int css2sac_dovftoif();
int css2sac_sacwrite();

#endif /* ifdef ANSIC */

#define BUFLEN 1024

static struct sac_header sach;
static char buffer[BUFLEN];
static float fdata[BUFLEN];
static long  ldata[BUFLEN];
static short sdata[BUFLEN];
static int ascii;
static long numascii = 0;
static char o_path[MAXPATHLEN+1];
static char i_path[MAXPATHLEN+1];
static float depmin = 0;
static float depmax = 0;
static int first_write = 1;
            
int css2sac_compare(aptr, bptr)
#ifdef __STDC__
const void *aptr, *bptr;
#else
void *aptr, *bptr;
#endif
{
int val;
struct wfdisc *a, *b;

    a = (struct wfdisc *) aptr;
    b = (struct wfdisc *) bptr;

    if ((val = strcmp(a->sta, b->sta)) != 0) {
        return val;
    } else if ((val = strcmp(a->chan, b->chan)) != 0) {
        return val;
    } else {
        return a->time - b->time;
    }
}

int css2sac_sacwrite(fp, data, npts)
FILE *fp;
float *data;
long npts;
{
int i;

    if (first_write) {
        depmin = data[0];
        depmax = data[0];
        first_write = 0;
    }

    for (i = 0; i < npts; i++) {
        if (data[i] < depmin) depmin = data[i];
        if (data[i] > depmax) depmax = data[i];
    }

    if (ascii) {
        if (sacio_wad(fp, data, npts, &numascii) != npts) return -1;
    } else {
        if (fwrite(data, sizeof(float), npts, fp) != npts) return -1;
    }

    return 0;
}

int css2sac_doi2(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;

    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(sdata, sizeof(short), nread, in) != nread)   return -1;
        for (i = 0; i < nread; i++) fdata[i] = (float) sdata[i];
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_doi2swap(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;

    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(sdata, sizeof(short), nread, in) != nread)   return -1;
        util_sswap(sdata, nread);
        for (i = 0; i < nread; i++) fdata[i] = (float) sdata[i];
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_doi4(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;

    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(ldata, sizeof(long), nread, in) != nread)   return -1;
        for (i = 0; i < nread; i++) fdata[i] = (float) ldata[i];
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_doi4swap(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;

    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(ldata, sizeof(long), nread, in) != nread)   return -1;
        util_lswap(ldata, nread);
        for (i = 0; i < nread; i++) fdata[i] = (float) ldata[i];
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_dof4(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;

    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(fdata, sizeof(float), nread, in) != nread)   return -1;
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_dovftoif(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;
unsigned long *rawdata;

    assert(sizeof(unsigned long) == sizeof(float));

    rawdata = (unsigned long *) fdata;
    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(rawdata, sizeof(float), nread, in) != nread) return -1;
        util_vftoif(rawdata, nread);
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

int css2sac_doiftovf(in, out, npts)
FILE *in, *out;
long npts;
{
long i, nread;
unsigned long *rawdata;

    assert(sizeof(unsigned long) == sizeof(float));

    rawdata = (unsigned long *) fdata;
    while (npts > 0) {
        nread = (npts > BUFLEN) ? BUFLEN : npts;
        if (fread(rawdata, sizeof(float), nread, in) != nread) return -1;
        util_iftovf(rawdata, nread);
        if (css2sac_sacwrite(out, fdata, nread) != 0) return -1;
        npts -= nread;
    }

    return 0;
}

FILE *css2sac_open(wfdisc, defaults)
struct wfdisc *wfdisc;
struct sac_header *defaults;
{
static struct wfdisc *prev = NULL;
static int count = 0;
int i, yr, da, hr, mn, sc, ms, chan_code = -1;
FILE *out;
long chan_no;
#ifdef IDA_SUPPORT
struct iris_coords *coords;
struct iris_orient *orient;
#endif /* IDA_SUPPORT */

    first_write = 1;
    sach = *defaults;;
 
    if (
        prev != NULL && 
        strcmp(prev->sta, wfdisc->sta) == 0 &&
        strcmp(prev->chan, wfdisc->chan) == 0
    ) {
        ++count;
    } else {
        count = 0;
    }

#ifdef IDA_SUPPORT
/* BEGIN iris lib dependent code                                       */
/* The following ugly piece is "temporary" during the transition from  */
/* standard numerical channel names to SEED style names.               */

    if (strlen(wfdisc->chan) == 2) {
        sscanf(wfdisc->chan, "%ld", &chan_no);
        switch ((int) chan_no) {
          case  1: strcpy(wfdisc->chan, "blz"); break;
          case  2: strcpy(wfdisc->chan, "bln"); break;
          case  3: strcpy(wfdisc->chan, "ble"); break;
          case  4: strcpy(wfdisc->chan, "llz"); break;
          case  5: strcpy(wfdisc->chan, "lln"); break;
          case  6: strcpy(wfdisc->chan, "lle"); break;
          case  7: strcpy(wfdisc->chan, "vlz"); break;
          case  8: strcpy(wfdisc->chan, "vln"); break;
          case  9: strcpy(wfdisc->chan, "vle"); break;
          case 11: strcpy(wfdisc->chan, "bhz"); break;
          case 12: strcpy(wfdisc->chan, "bhn"); break;
          case 13: strcpy(wfdisc->chan, "bhe"); break;
          case 14: strcpy(wfdisc->chan, "lhz"); break;
          case 15: strcpy(wfdisc->chan, "lhn"); break;
          case 16: strcpy(wfdisc->chan, "lhe"); break;
          case 17: strcpy(wfdisc->chan, "vhz"); break;
          case 18: strcpy(wfdisc->chan, "vhn"); break;
          case 19: strcpy(wfdisc->chan, "vhe"); break;
          case 21: strcpy(wfdisc->chan, "vmz"); break;
          case 22: strcpy(wfdisc->chan, "vmn"); break;
          case 23: strcpy(wfdisc->chan, "vme"); break;
          case 24: strcpy(wfdisc->chan, "vmz"); break;
          case 25: strcpy(wfdisc->chan, "vmn"); break;
          case 26: strcpy(wfdisc->chan, "vme"); break;
          case 27: strcpy(wfdisc->chan, "vmz"); break;
          case 28: strcpy(wfdisc->chan, "vmn"); break;
          case 29: strcpy(wfdisc->chan, "vme"); break;
          case 31: strcpy(wfdisc->chan, "ehz"); break;
          case 32: strcpy(wfdisc->chan, "ehn"); break;
          case 33: strcpy(wfdisc->chan, "ehe"); break;
          case 34: strcpy(wfdisc->chan, "ehz1"); break;
          case 35: strcpy(wfdisc->chan, "ehz2"); break;
          case 36: strcpy(wfdisc->chan, "ehz3"); break;
          case 41: strcpy(wfdisc->chan, "mlz"); break;
          case 42: strcpy(wfdisc->chan, "mln"); break;
          case 43: strcpy(wfdisc->chan, "mle"); break;
          case 44: strcpy(wfdisc->chan, "mhz"); break;
          case 45: strcpy(wfdisc->chan, "mhn"); break;
          case 46: strcpy(wfdisc->chan, "mhe"); break;
          case 51: strcpy(wfdisc->chan, "elz"); break;
          case 52: strcpy(wfdisc->chan, "eln"); break;
          case 53: strcpy(wfdisc->chan, "ele"); break;
          case 54: strcpy(wfdisc->chan, "ehz"); break;
          case 55: strcpy(wfdisc->chan, "ehn"); break;
          case 56: strcpy(wfdisc->chan, "ehe"); break;
          case 71: strcpy(wfdisc->chan, "elz"); break;
          case 72: strcpy(wfdisc->chan, "eln"); break;
          case 73: strcpy(wfdisc->chan, "ele"); break;
          case 74: strcpy(wfdisc->chan, "ehz"); break;
          case 75: strcpy(wfdisc->chan, "ehn"); break;
          case 76: strcpy(wfdisc->chan, "ehe"); break;
          case 85: strcpy(wfdisc->chan, "ae1"); break;
          case 86: strcpy(wfdisc->chan, "wk2"); break;
          case 87: strcpy(wfdisc->chan, "wk1"); break;
          case 94: strcpy(wfdisc->chan, "vgz"); break;
          case 95: strcpy(wfdisc->chan, "ugz"); break;
          default: break;
        }
    }

    if ((coords = iris_coords(wfdisc->sta)) != NULL) {
        sach.stla = coords->lat;
        sach.stlo = coords->lon;
        sach.stel = coords->elev;
        sach.stdp = coords->depth;
    }

    switch (wfdisc->chan[0]) {
      case 'b':
      case 'm':
      case 'v':
      case 'u':
        switch (wfdisc->chan[2]) {
            case 'z': chan_code = BBZ; break;
            case 'n': chan_code = BBN; break;
            case 'e': chan_code = BBE; break;
            default:  chan_code = -1;
        }
        break;
      case 'e':
        switch (wfdisc->chan[2]) {
            case 'z': chan_code = SPZ; break;
            case 'n': chan_code = SPN; break;
            case 'e': chan_code = SPE; break;
            default:  chan_code = -1;
        }
      default:
        chan_code = -1;
    }

    if (chan_code >= 0) {
        orient = iris_orient(wfdisc->sta, chan_code, wfdisc->time);
        if (orient != NULL) {
            sach.cmpinc = orient->inc;
            sach.cmpaz  = orient->azm;
        }
    }

/* END iris lib dependent code  */
#endif /* IDA_SUPPORT */

    sprintf(o_path, "%s.%s", wfdisc->sta, wfdisc->chan);
    if (count) sprintf(o_path+strlen(o_path), ".%d", count);

    if ((out = fopen(util_lcase(o_path), "wb")) == NULL) return NULL;

    sscanf(util_dttostr(wfdisc->time, 0), "%24:%3d-%2d:%2d:%2d.%3d",
        &yr, &da, &hr, &mn, &sc, &ms);

    sach.npts   = 0;
    sach.delta  = (float) ((double) 1.0 / (double) wfdisc->smprate);
    sach.b      = 0.0;
    sach.iftype = ITIME;
    sach.leven  = TRUE;

    sach.lpspol = TRUE;
    sach.lovrok = TRUE;
    sach.lcalda = TRUE;

    sach.idep   = IUNKN;
    sach.nzyear = yr;
    sach.nzjday = da;
    sach.nzhour = hr;
    sach.nzmin  = mn;
    sach.nzsec  = sc;
    sach.nzmsec = ms;
    sach.iztype = IB;
    util_ucase(strcpy(sach.kstnm, wfdisc->sta));
    strcpy(sach.kinst, wfdisc->instype); 
    util_ucase(strcpy(sach.kcmpnm, wfdisc->chan));

    numascii = 0;

    if (ascii) {
        if (sacio_wah(out, &sach) != 0) {
            perror("sacio_wah");
            fclose(out);
            unlink(o_path);
            return NULL;
        }
    } else {
        if (sacio_wbh(out, &sach) != 0) {
            perror("sacio_wbh");
            fclose(out);
            unlink(o_path);
            return NULL;
        }
    }

    prev = wfdisc;
    return out;
}

int css2sac_close(fp)
FILE *fp;
{
    sach.depmin = depmin;
    sach.depmax = depmax;

    rewind(fp);
    if (ascii) {
        if (sacio_wah(fp, &sach) != 0) {
            perror("sacio_wah");
            return -1;
        }
    } else {
        if (sacio_wbh(fp, &sach) != 0) {
            perror("sacio_wbh");
            return -1;
        }
    }
    fclose(fp);
    return 0;
}

int css2sac_copy(fp, wfdisc, order)
FILE *fp;
struct wfdisc *wfdisc;
int order;
{
FILE *ifp;
int retval, (*convert)();

    sprintf(i_path, "%s/%s", wfdisc->dir, wfdisc->dfile);
    if ((ifp = fopen(i_path, "rb")) == NULL) {
        perror(i_path);
        return -1;
    }

    if (fseek(ifp, wfdisc->foff, 0) != 0) {
        perror(i_path);
        return -1;
    }

    if (strcmp(wfdisc->datatype, "i2") == 0) {
        convert = (order == LTL_ENDIAN_ORDER) ? css2sac_doi2 : css2sac_doi2swap;
    } else if (strcmp(wfdisc->datatype, "s2") == 0) {
        convert = (order == BIG_ENDIAN_ORDER) ? css2sac_doi2 : css2sac_doi2swap;
    } else if (strcmp(wfdisc->datatype, "i4") == 0) {
        convert = (order == LTL_ENDIAN_ORDER) ? css2sac_doi4 : css2sac_doi4swap;
    } else if (strcmp(wfdisc->datatype, "s4") == 0) {
        convert = (order == BIG_ENDIAN_ORDER) ? css2sac_doi4 : css2sac_doi4swap;
    } else if (strcmp(wfdisc->datatype, "f4") == 0) {
        convert = (order == LTL_ENDIAN_ORDER) ? css2sac_dof4 : css2sac_dovftoif;
    } else if (strcmp(wfdisc->datatype, "t4") == 0) {
        convert = (order == BIG_ENDIAN_ORDER) ? css2sac_dof4 : css2sac_doiftovf;
    } else {
        fprintf(stderr, "unsupported datatype: %s", wfdisc->datatype);
        return -1;
    }

    sach.npts += wfdisc->nsamp;
    sach.e     = (float) (sach.npts - 1) * sach.delta;

    retval = (*convert)(ifp, fp, wfdisc->nsamp);
    fclose(ifp);

    return retval;

}

int css2sac_cont(prev, crnt, tolerance)
struct wfdisc *prev;
struct wfdisc *crnt;
int tolerance;
{
double expected, error;

    if (strcmp(prev->sta,  crnt->sta)  != 0) return 0;
    if (strcmp(prev->chan, crnt->chan) != 0) return 0;
    if (strcmp(prev->datatype, crnt->datatype) != 0) return 0;

    expected = prev->time + ((double) prev->nsamp / (double) prev->smprate);
    error    = fabs(crnt->time - expected);    /* error in seconds */
    error    = (double) prev->smprate * error; /* error in samples */

    if ((int) error <= tolerance) return 1;

    return 0;
}

int css2sac(wfdisc, nrec, tolerance, defaults, doascii)
struct wfdisc *wfdisc;
long nrec;
int tolerance;
struct sac_header *defaults;
int doascii;
{
FILE *fp = NULL;
long i;
struct wfdisc *prev;
int order;

    order  = util_order();
    ascii = doascii;

    for (i = 0; i < nrec; i++) {

/*  Make sure that the channels and datatypes are OK  */

        if (strcmp(wfdisc[i].chan, wfdisc_null.chan) == 0) {
            fprintf(stderr,"css2sac(): wfdisc contains undefined chan(s)\n");
            return -1;
        } else if (cssio_wrdsize(wfdisc[i].datatype) < 0) {
            fprintf(stderr,"css2sac(): wfdisc contains illegal datatype(s)\n");
            return -1;
        }

/*  Make sure we can read the data files  */

        sprintf(i_path, "%s/%s", wfdisc[i].dir, wfdisc[i].dfile);
        if ((fp = fopen(i_path, "rb")) == NULL) {
            fprintf(stderr,"css2sac(): fopen: ");
            perror(i_path);
            return -2;
        }
        fclose(fp);
        fp = NULL;
        
    }

/*  Sort into station/channel/time order  */

    qsort((char *) wfdisc, nrec, sizeof(struct wfdisc), css2sac_compare);


/*  Convert each wfdisc to SAC, appending continuous records  */

    for (prev = &wfdisc_null, i = 0; i < nrec; i++) {
        if (!css2sac_cont(prev, &wfdisc[i], tolerance)) {
            if (fp != NULL && css2sac_close(fp) != 0) return -5;
            if ((fp = css2sac_open(&wfdisc[i], defaults)) == NULL) return -3;
        }
        if (css2sac_copy(fp, &wfdisc[i], order) != 0) return -4;
        prev = &wfdisc[i];
    }
    if (fp != NULL && css2sac_close(fp) != 0) return -5;

/*  Done  */

    return 0;
}
