/* @(#)seed.h	1.7 11/06/97 */
/*
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *
 */

#ifndef seed_h_included
#define seed_h_included

#include <sys/types.h>

/* Selected name lengths (all less trailing NULL) */

#define SEED_SNAMLEN 5  /* station name        */
#define SEED_CNAMLEN 3  /* channel code        */
#define SEED_LNAMLEN 2  /* location identifier */
#define SEED_NNAMLEN 2  /* network code        */

/* Selected encoding formats */

#define SEED_INT_16 1  /* 16 bit integers                      */
#define SEED_INT_24 2  /* 24 bit integers                      */
#define SEED_INT_32 3  /* 32 bit integers                      */
#define SEED_IEEE_F 4  /* IEEE floading point                  */
#define SEED_IEEE_D 5  /* IEEE double precision floating point */
#define SEED_STEIM1 10 /* Steim (1) compression                */
#define SEED_STEIM2 11 /* Steim (2) compression                */

#define SEED_LTL_ENDIAN 0
#define SEED_BIG_ENDIAN 1

/* Selected flag masks */

#define SEED_FLAG_START 0x08  /* B00001000 = start of time series */
#define SEED_FLAG_STOP  0x10  /* B00010000 = end   of time series */

/* Misc. constants */

#define SEED_MINRECEXP  8
#define SEED_MAXRECEXP 16

/* Structure templates */

struct seed_fsdh {
    long    seqno;                 /* sequence number                   */
    char    staid[SEED_SNAMLEN+1]; /* station identifier code           */
    char    locid[SEED_LNAMLEN+1]; /* location identifier               */
    char    chnid[SEED_CNAMLEN+1]; /* channel identifier                */
    char    netid[SEED_NNAMLEN+1]; /* network code                      */
    double  start;                 /* record start time                 */
    short   nsamp;                 /* number of samples                 */
    short   srfact;                /* sample rate factor                */
    short   srmult;                /* sample rate multiplier            */
    char    active;                /* activity flags                    */
    char    ioclck;                /* I/O and clock flags               */
    char    qual;                  /* data quality flags                */
    char    more;                  /* number of blockettes that follow  */
    long    tcorr;                 /* time correction                   */
    short   bod;                   /* offset to begining of data        */
    short   first;                 /* first blockette                   */
    /* The following are for internal convenience and not part of SEED  */
    u_long  order;                 /* byte ordering used in raw data    */
    int     swap;                  /* swap/no swap flag, based on above */
};

struct seed_b1000 {
    short next;     /* next blockette's byte number */
    char  format;   /* encoding format              */
    char  order;    /* word order                   */
    char  length;   /* record length                */
};

struct seed_b1001 {
    short next;    /* next blockette's byte number */
    char  tqual;   /* vendor specificy qual eval   */
    char  usec;    /* offset to start time in usec */
    char  fcount;  /* frame count                  */
};

/* A mini-seed packet */

struct seed_minipacket {
    struct seed_fsdh  fsdh;  /* a standard FSDH,                   */
    struct seed_b1000 b1000; /* followed by a blockette 1000,      */
    long *data;              /* followed by some decompressed data */
    size_t datlen;           /* size of above array                */
    /* The following come from stuff in the sub-headers */
    char *sname;             /* station name                       */
    char *cname;             /* channel name                       */
    char *nname;             /* network name                       */
    long nsamp;              /* number of samples in data          */
    double beg;              /* time of first sample               */
    double sint;             /* sample interval                    */
    long srclen;
    u_long order;
};
    
/* Function prototypes */

int seed_minihdr(
    struct seed_minipacket *,
    char *
);

char *seed_dtconv(
    char *,
    double,
    u_long
);

int seed_b1000(
    struct seed_b1000 *,
    char *
);

int seed_b1001(
    struct seed_b1001 *,
    char *
);

int seed_fsdh(
    struct seed_fsdh *,
    char *
);

long seed_mini(
    long *,
    long,
    struct seed_fsdh *,
    char *,
    int,
    long *
);

void seed_load1000(
    char *,
    struct seed_b1000 *,
    u_long
);

void seed_loadfsdh(
    char *,
    struct seed_fsdh *,
    u_long
);

double seed_makesint(
    short,
    short
);

int seed_minihdr(
    struct seed_minipacket *,
    char *
);

int seed_readmini(
    int,
    struct seed_minipacket *,
    char *,
    size_t,
    int
);

void seed_sintsplit(
    double,
    short *,
    short *
);

int seed_timetear(
    struct seed_minipacket *,
    struct seed_minipacket *,
    long *
);

double seed_ttodt(
    char *,
    u_long *
);

int seed_type(
    char *,
    long *,
    int
);

#endif
