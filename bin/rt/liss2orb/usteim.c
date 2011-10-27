/*  Copyright (c) 1997 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software module is wholly owned by Boulder Real Time         */
/*  Technologies, Inc. Any use of this software module without        */
/*  express written permission from Boulder Real Time Technologies,   */
/*  Inc. is prohibited.                                               */

#define ERRSTEIM  "steim decompression: "

#include <sys/types.h>
#include <netinet/in.h>

#include "tr.h"
#include "coords.h"
#include "swapbytes.h"
#include "ant_steim.h"

typedef union {
    int            *ints;
    short          *shorts;
    char           *chars;
}               SteimFrame;

static void
idrecord (Steim * conf, char *id)
{
    char            record[8],
                   *s;
    strncpy (record, conf->record, 6);
    strtrim (record);

    sprintf (id, "\n\t#%s %s:%s:%s:%s %s", record,
	     conf->sdh.net, conf->sdh.sta, conf->sdh.chan, conf->sdh.loc,
	     s = strydtime (conf->sdh.epoch));
    free (s);
}

static int
convert_frame (SteimFrame frame, int seq, int level, int **ipp, int *x0p, int *xnp, Steim * conf)
{
    int             codes[16],
                    w0,
                    i;
    char           *cp;
    int             dnib,
                    rest,
                    d;
    int             retcode = 0;
    int            *ip;
    char           *ptr;
    char            id[64];

    ip = *ipp;

    ptr = frame.chars;
    N2H4 (&w0, ptr, 1);
    for (i = 15; i >= 0; i--) {
	codes[i] = w0 & 0x3;
	w0 >>= 2;
    }

    if (seq == 0) {
	N2H4 (x0p, ptr + 4, 1);
	N2H4 (xnp, ptr + 8, 1);
	i = 3;
    } else {
	i = 1;
    }

    for (ptr += i * 4; i < 16; i++, ptr += 4) {
	switch (codes[i]) {
	  case 0:
	    break;

	  case 1:
	    cp = (char *) ptr;
	    mb2hi(&cp, ip, 4) ;
	    ip += 4; 
	    break;

	  case 2:
	    if (level == 1) {
		short int       sval[2];

		N2H2 (sval, ptr, 2);
		*ip++ = sval[0];
		*ip++ = sval[1];
	    } else {
		int             ival;

		N2H4 (&ival, ptr, 1);
		dnib = (ival >> 30) & 0x3;
		rest = ival & 0x3fffffff;
		switch (dnib) {
		  case 0:
		    if (!(retcode & 1)) {
			idrecord (conf, id);
			elog_log (0, ERRSTEIM " ck=2, dnib=0 %8X %s\n",
				  rest, id);
		    }
		    retcode |= 1;
		    break;

		  case 1:
		    if (rest & 0x20000000)
			rest |= 0xc0000000;
		    *ip++ = rest;
		    break;

		  case 2:
		    d = rest >> 15;
		    if (d & 0x4000)
			d |= 0xffff8000;
		    *ip++ = d;
		    d = rest & 0x7fff;
		    if (d & 0x4000)
			d |= 0xffff8000;
		    *ip++ = d;
		    break;

		  case 3:
		    d = rest >> 20;
		    if (d & 0x200)
			d |= 0xfffffc00;
		    *ip++ = d;
		    d = rest >> 10 & 0x3ff;
		    if (d & 0x200)
			d |= 0xfffffc00;
		    *ip++ = d;
		    d = rest & 0x3ff;
		    if (d & 0x200)
			d |= 0xfffffc00;
		    *ip++ = d;
		    break;
		}
	    }
	    break;

	  case 3:
	    if (level == 1) {
		N2H4 (ip, ptr, 1);
		ip++;
	    } else if (level == 2) {
		int             ival;

		N2H4 (&ival, ptr, 1);
		dnib = (ival >> 30) & 0x3;
		rest = ival & 0x3fffffff;
		switch (dnib) {
		  case 0:
		    d = rest >> 24 & 0x3f;
		    if (d & 0x20)
			d |= 0xffffffc0;
		    *ip++ = d;
		    d = rest >> 18 & 0x3f;
		    if (d & 0x20)
			d |= 0xffffffc0;
		    *ip++ = d;
		    d = rest >> 12 & 0x3f;
		    if (d & 0x20)
			d |= 0xffffffc0;
		    *ip++ = d;
		    d = rest >> 6 & 0x3f;
		    if (d & 0x20)
			d |= 0xffffffc0;
		    *ip++ = d;
		    d = rest & 0x3f;
		    if (d & 0x20)
			d |= 0xffffffc0;
		    *ip++ = d;
		    break;

		  case 1:
		    d = rest >> 25 & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    d = rest >> 20 & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    d = rest >> 15 & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    d = rest >> 10 & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    d = rest >> 5 & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    d = rest & 0x1f;
		    if (d & 0x10)
			d |= 0xffffffe0;
		    *ip++ = d;
		    break;

		  case 2:
		    d = rest >> 24 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest >> 20 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest >> 16 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest >> 12 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest >> 8 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest >> 4 & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    d = rest & 0xf;
		    if (d & 0x8)
			d |= 0xfffffff0;
		    *ip++ = d;
		    break;

		  case 3:
		    if (!(retcode & 2)) {
			idrecord (conf, id);
			elog_log (0, ERRSTEIM " ck=3, dnib=3 %8X %s\n",
				  rest, id);
		    }
		    retcode |= 2;
		    break;
		}
	    } else {
		idrecord (conf, id);
		elog_log (0, "unsupported steim compression level = %d : %s", level, id);
		retcode |= 4;
	    }
	    break;

	  default:
	    if (!(retcode & 4)) {
		idrecord (conf, id);
		elog_log (0, ERRSTEIM " code is %d (can't happen) : %s",
			  codes[i], id);
	    }
	    retcode |= 4;
	}
    }

    *ipp = ip;
    return retcode;
}

#define MAX_SAMPLES_PER_RECORD 6700    /* Seed reference pp 123 says 6601 */
int
usteim (Steim * conf, int **data, long *npts)
{
    SteimFrame           frame;
    int             i,
                    nframes;
    int            *ip,
                    x0,
                    xn,
                    xc,
                    nsamps;
    int             retcode = 0;
    char            id[64];

    if (conf->data == 0) {
	allot (int *, conf->data, MAX_SAMPLES_PER_RECORD);
    }
    *data = ip = conf->data;
    *npts = 0;

    nframes = (conf->record_size - conf->sdh.data_offset) / 64;
    frame.chars = conf->record + conf->sdh.data_offset;
    ip = conf->data;

    for (i = 0; i < nframes; i++) {
	retcode += convert_frame (frame, i, conf->level, &ip, &x0, &xn, conf);
	if (ip - conf->data >= conf->sdh.nsamp)
	    break;
	frame.chars += 64;
    }

    nsamps = ip - conf->data;
    if (nsamps < conf->sdh.nsamp) {
	idrecord (conf, id);
	elog_log (0, ERRSTEIM " found %d samples, expected %d samples : %s",
		  nsamps, conf->sdh.nsamp, id);
	retcode++;
    }
    ip = conf->data;
    *ip++ = xc = x0;
    for (i = 1; i < conf->sdh.nsamp; i++) {
	xc += *ip;
	*ip++ = xc;
    }

    if (xc != xn) {
	idrecord (conf, id);
	elog_log (0, ERRSTEIM " computed last sample differs from stored sample %d %d : %s",
		  xc, xn, id);
	retcode++;
    }
    *npts = conf->sdh.nsamp;
    return retcode;
}

#define MAX_SAMPLES_PER_RECORD 6700    /* Seed reference pp 123 says 6601 */
int
_usteim (Steim * conf, int *data, long *npts)
{
    SteimFrame           frame;
    int             i,
                    nframes;
    int            *ip,
                    x0,
                    xn,
                    xc,
                    nsamps;
    int             retcode = 0;
    char            id[64];


    *npts = 0;

    nframes = (conf->record_size - conf->sdh.data_offset) / 64;
    frame.chars = conf->record + conf->sdh.data_offset;

    ip = data;

    for (i = 0; i < nframes; i++) {
	retcode += convert_frame (frame, i, conf->level, &ip, &x0, &xn, conf);
	if (ip - data >= conf->sdh.nsamp)
	    break;
	frame.chars += 64;
    }

    nsamps = ip - data;
    if (nsamps < conf->sdh.nsamp) {
	idrecord (conf, id);
	elog_log (0, ERRSTEIM " found %d samples, expected %d samples : %s",
		  nsamps, conf->sdh.nsamp, id);
	retcode++;
    }
    ip = data;
    *ip++ = xc = x0;
    for (i = 1; i < conf->sdh.nsamp; i++) {
	xc += *ip;
	*ip++ = xc;
    }

    if (xc != xn) {
	idrecord (conf, id);
	elog_log (0, ERRSTEIM " computed last sample %d differs from stored sample %d : %s",
		  xc, xn, id);
	retcode++;
    }
    *npts = conf->sdh.nsamp;
    return retcode;
}

/* $Id$ */
