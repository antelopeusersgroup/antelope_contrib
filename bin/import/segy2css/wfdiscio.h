/* @(#)wfdiscio.h	1.1 03/12/96  */

/* wfdisc structure and I/O definitions */

#ifndef CSS_WFDISC_H
#define CSS_WFDISC_H

#include "trim.h"

typedef struct wfdisc {
	double time;		/* epoch time of first sample in file */
	double endtime;		/* time+(nsamp-1)/samprate */
	float  samprate;	/* sampling rate in samples/sec */
	float  calib;		/* nominal calibration */
	float  calper;		/* nominal calibration period */
	long   wfid;		/* waveform id */
	long   chanid;		/* channel operation id */
	long   jdate;		/* julian date */
	long   nsamp;		/* number of samples */
	long   foff;		/* byte offset */
	long   commid;		/* comment id */
	char   sta[8];		/* station */
	char   chan[12];	/* channel */
	char   instype[8];	/* instrument code */
	char   segtype[4];	/* indexing method */
	char   datatype[4];	/* numeric storage */
	char   clip[4];		/* clipped flag */
	char   dir[68];		/* directory */
	char   dfile[36];	/* data file */
	double lddate;	/* load date */
} WFDISC;

#define WFDISC_NF 20 /* fields */
#define WFDISC_SIZE 284 /* bytes */

#define WFDISC_RCS "%6c%*c%8c%lf%d%d%d%lf%d%f%f%f%*c%6c%*c%1c%*c%2c%*c%1c%*c%64c%*c%32c%d%d%*c%lf[^\n] \n"

#define WFDISC_SCS "%s %s %lf %d %d %d %lf %d %f %f %f %s %s %s %s %s %s %d %d %lf[^\n] \n"

#define WFDISC_RVL(SP)\
(SP)->sta, (SP)->chan, &(SP)->time, &(SP)->wfid, \
&(SP)->chanid, &(SP)->jdate, &(SP)->endtime, &(SP)->nsamp, \
&(SP)->samprate, &(SP)->calib, &(SP)->calper, (SP)->instype, \
(SP)->segtype, (SP)->datatype, (SP)->clip, (SP)->dir, \
(SP)->dfile, &(SP)->foff, &(SP)->commid,  &(SP)->lddate

#define WFDISC_WCS "%-6.6s %-8.8s %17.5lf %8ld %8ld %8ld %17.5lf %8ld %11.5f %16.6f %16.6f %-6.6s %-1.1s %-2.2s %-1.1s %-64.64s %-32.32s %10ld %8ld %17.5lf\n"

#define WFDISC_WVL(SP)\
(SP)->sta, (SP)->chan, (SP)->time, (SP)->wfid, \
(SP)->chanid, (SP)->jdate, (SP)->endtime, (SP)->nsamp, \
(SP)->samprate, (SP)->calib, (SP)->calper, (SP)->instype, \
(SP)->segtype, (SP)->datatype, (SP)->clip, (SP)->dir, \
(SP)->dfile, (SP)->foff, (SP)->commid, (SP)->lddate

#define WFDISC_TRM(SP)\
TRIM((SP)->sta,7); TRIM((SP)->chan,9); TRIM((SP)->instype,7); \
TRIM((SP)->segtype,2); TRIM((SP)->datatype,3); TRIM((SP)->clip,2); \
TRIM((SP)->dir,65); TRIM((SP)->dfile,33)

#define WFDISC_FPD(SP)\
FPAD((SP)->sta,8); FPAD((SP)->chan,12); FPAD((SP)->instype,8); \
FPAD((SP)->segtype,4); FPAD((SP)->datatype,4); FPAD((SP)->clip,4); \
FPAD((SP)->dir,68); FPAD((SP)->dfile,36)

#define WFDISC_BFL(SP)\
BFIL((SP)->sta,8); BFIL((SP)->chan,12); BFIL((SP)->instype,8); \
BFIL((SP)->segtype,4); BFIL((SP)->datatype,4); BFIL((SP)->clip,4); \
BFIL((SP)->dir,68); BFIL((SP)->dfile,36)

static struct wfdisc wfdisc_null = {
	-9999999999.999,	/* epoch time of first sample in file */
	9999999999.999,		/* time+(nsamp-1)/samprate */
	-1.0,			/* sampling rate in samples/sec */
	0.0,			/* nominal calibration */
	0.0,			/* nominal calibration period */
	-1,			/* waveform id */
	-1,			/* channel operation id */
	-1,			/* julian date */
	-1,			/* number of samples */
	-1,			/* byte offset */
	-1,			/* comment id */
	"-",			/* station */
	"-",			/* channel */
	"-",			/* instrument code */
	"-",			/* indexing method */
	"-",			/* numeric storage */
	"-",			/* clipped flag */
	"-",			/* directory */
	"-",			/* data file */
	 9999999999.999         /* load date */
};

/* don't append anything below next line */
#endif CSS_WFDISC_H
