/*
 * Trace_buf.h
 *
 * Header file for Earthworm packets that allow demultiplexed data
 *  from various sources to be handled efficiently. Added to
 *  Earthworm system by
 * Kent Lindquist and Roger Hansen
 * Geophysical Institute
 * University of Alaska, Fairbanks
 *
 * February, 1996
 *
 * November 1996: KGL Added net and quality fields to accommodate
 *  needs expressed by Alex Bittenbinder and the Earthworm team
 * February 1997: KGL Added pad field as used by Earthworm team
 *
 * Replaced hardcoded string lengths for S-C-N with
 * #defines so that they can be used elsewhere. LV 5/1999
 *
 *
 */

#ifndef TRACE_BUF_H
#define TRACE_BUF_H

#define NETWORK_NULL_STRING "-"

/* NOTE:
 * The principal time fields in the TRACE_HEADER are:
 *    starttime, nsamp, and samprate.
 * The endtime field is included as a redundant convenience.
 */

#define	TRACE_STA_LEN	7
#define	TRACE_NET_LEN	9
#define	TRACE_CHAN_LEN	9

typedef struct {
        int     pinno;          /* Pin number */
        int     nsamp;          /* Number of samples in packet */
        double  starttime;      /* time of first sample in epoch seconds
                                   (seconds since midnight 1/1/1970) */
        double  endtime;        /* Time of last sample in epoch seconds */
        double  samprate;       /* Sample rate */
        char    sta[TRACE_STA_LEN];         /* Site name */
        char    net[TRACE_NET_LEN];         /* Network name */
        char    chan[TRACE_CHAN_LEN];        /* Component/channel code */
        char    datatype[3];    /* Data format code */
        char    quality[2];     /* Data-quality field */
        char    pad[2];         /* padding */
} TRACE_HEADER;

#define LOC_NULL_STRING "--"

#define	TRACE2_STA_LEN	7	/* SEED: 5 plus terminating NUL */
#define	TRACE2_NET_LEN	9	/* SEED: 2 plus terminating NUL */
#define	TRACE2_CHAN_LEN	4	/* SEED: 3 plus terminating NUL */
#define	TRACE2_LOC_LEN	3	/* SEED: 2 plus terminating NUL */

#define	TRACE2_VERSION0 '2'	/* version[0] for TYPE_TRACEBUF2 */
#define	TRACE2_VERSION1 '0'	/* version[1] for TYPE_TRACEBUF2 */

typedef struct {
        int     pinno;          /* Pin number */
        int     nsamp;          /* Number of samples in packet */
        double  starttime;      /* time of first sample in epoch seconds
                                   (seconds since midnight 1/1/1970) */
        double  endtime;        /* Time of last sample in epoch seconds */
        double  samprate;       /* Sample rate */
        char    sta[TRACE2_STA_LEN];         /* Site name */
        char    net[TRACE2_NET_LEN];         /* Network name */
        char    chan[TRACE2_CHAN_LEN];        /* Component/channel code */
        char    loc[TRACE2_NET_LEN];        /* Location code */
        char    version[2];    /* version field */
        char    datatype[3];    /* Data format code */
        char    quality[2];     /* Data-quality field */
        char    pad[2];         /* padding */
} TRACE2_HEADER;

#define MAX_TRACEBUF_SIZ 4096   /* define maximum size of trace message buffer */

typedef union {
        char    msg[MAX_TRACEBUF_SIZ];
        TRACE_HEADER trh;
        TRACE2_HEADER trh2;
        int     i;
} TracePacket;

/* Byte 0 of data quality flags, as in SEED format
   ***********************************************/
#define AMPLIFIER_SATURATED    0x01
#define DIGITIZER_CLIPPED      0x02
#define SPIKES_DETECTED        0x04
#define GLITCHES_DETECTED      0x08
#define MISSING_DATA_PRESENT   0x10
#define TELEMETRY_SYNCH_ERROR  0x20
#define FILTER_CHARGING        0x40
#define TIME_TAG_QUESTIONABLE  0x80

/* CSS datatype codes
   ******************/
/*
        t4      SUN IEEE single precision real
        t8      SUN IEEE double precision real
        s4      SUN IEEE integer
        s2      SUN IEEE short integer
        f4      VAX/Intel IEEE single precision real
        f8      VAX/Intel IEEE double precision real
        i4      VAX/Intel IEEE integer
        i2      VAX/Intel IEEE short integer
        g2      NORESS gain-ranged
*/
#endif
