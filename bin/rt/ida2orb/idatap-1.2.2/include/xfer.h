/* @(#)xfer.h	1.23 03/24/98 */
/*======================================================================
 *
 * various defines in support of the IDA data exchange protocols
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#ifndef xfer_h_included
#define xfer_h_included

#include <stdio.h>
#include <sys/types.h>
#include <netdb.h>
#include "deviants.h"

/* Protocol requires RAW and GEN1 waveform formats, but permits others
 * to be offered.  Below are the additional formats supported by this
 * implementation.
 */

#undef  PASSCAL_SUPPORT
#undef  NRTS_SUPPORT
#undef  IDA_SUPPORT
#define SEED_SUPPORT
#undef  PASSCAL_SUPPORT

/* Define any additional formats that may be supported by
 * the client side of this impplementation.
 */

#define CSS_SUPPORT
#define SAC_SUPPORT
#define GSE_SUPPORT

/* Format specific includes and defines */

#ifdef NRTS_SUPPORT
#include "nrts.h"
#endif

#ifdef IDA_SUPPORT
#include "ida.h"
#define IDA_PAKLEN IDA_BUFSIZ
#else
#define IDA_PAKLEN 1
#endif

#ifdef PASSCAL_SUPPORT
#include "reftek.h"
#define PAS_PAKLEN REFTEK_MAXPAKLEN
#else
#define PAS_PAKLEN 1
#endif

#ifdef SEED_SUPPORT
#include "seed.h"
#define SEED_PAKLEN 4096
#else
#define SEED_PAKLEN 1
#endif

#ifdef CSS_SUPPORT
#include "cssio.h"
#endif

#ifdef SAC_SUPPORT
#include "sacio.h"
#endif

#ifdef GSE_SUPPORT
#include "util.h"
#endif

/* Globally available objects */

extern int xfer_errno;
extern int _xfer_timeout;
extern unsigned long xfer_nsend;
extern unsigned long xfer_nrecv;
extern char *Xfer_Buffer;
extern int Xfer_Buflen;
#ifdef IDA_SUPPORT
extern IDADB *Xfer_DB;
#endif

/* BEGIN PROTOCOL SPECIFIC CONSTANTS */

/* The highest protocol version which this source supports */

#define XFER_PROTOCOL 1

/* request codes */
/* We use a long, in case we want to bitmask the code in the future */

#define XFER_CNFREQ  0x00000001 /* configuration request */
#define XFER_WAVREQ  0x00000002 /* waveform request      */

/* Server ack codes, common for ALL protocol versions */

#define XFER_HEARTBEAT  0 /* server heartbeat                           */
#define XFER_EPROTOCOL  1 /* unsupported protocol version               */
#define XFER_EREQUEST   2 /* unsupported request type                   */
#define XFER_EFORMAT    3 /* unsupported return format                  */
#define XFER_ENOSUCH    4 /* all requested sta/chn are unsupported      */
#define XFER_EFAULT     5 /* server error                               */
#define XFER_EBUSY      6 /* server too busy to accept connection       */
#define XFER_EREFUSED   7 /* connection refused                         */
#define XFER_EREJECT    8 /* request rejected by server (content error) */
#define _MAX_ACK        8 /* largest of the above                       */

/* Configuration message formats */

#define XFER_CNFGEN1 0x01 /* Generic format 1         */
#define XFER_CNFNRTS 0x02 /* NRTS style configuration */

/* Waveform message formats */

#define XFER_WAVGEN1    1  /* Generic format 1              */
#define XFER_WAVSEED    2  /* mini-SEED data logger packets */
#define XFER_WAVRAW     3  /* Raw data logger output        */
#define XFER_WAVIDA     4  /* IDA data logger packets       */
#define XFER_WAVPASSCAL 5  /* PASSCAL data logger packets   */

#ifdef NRTS_SUPPORT
#define XFER_WAVNRTS 1000  /* not served, but client side support is here */
#endif

/* Request time wild cards */

#define XFER_YNGEST (double) -1
#define XFER_OLDEST (double) -2

/* protocol version 1 name lengths */

#define XFER01_SNAMLEN 7  /* station name, less null   */
#define XFER01_CNAMLEN 7  /* channel name, less null   */
#define XFER01_INAMLEN 7  /* GSE2.0 instype, less null */
#define XFER01_NNAMLEN 7  /* network name, less null   */

/* END PROTOCOL SPECIFIC CONSTANTS */

/* Implementation specific error codes */

#define XFER_EHANDLER  (_MAX_ACK+ 1) /* unable to install signal handler */
#define XFER_ELIMIT    (_MAX_ACK+ 2) /* implementation limit exceeded    */
#define XFER_EINVAL    (_MAX_ACK+ 3) /* illegal data received            */
#define XFER_ETOOBIG   (_MAX_ACK+ 4) /* message too large to receive     */
#define XFER_ETIMEDOUT (_MAX_ACK+ 5) /* connection timed out             */
#define XFER_ECONNECT  (_MAX_ACK+ 6) /* no connection with peer          */
#define XFER_EPIPE     (_MAX_ACK+ 7) /* connection reset by peer         */
#define XFER_EIO       (_MAX_ACK+ 8) /* I/O error                        */
#define XFER_EHOSTNAME (_MAX_ACK+ 9) /* can't resolve hostname           */
 
/* Default socket buffer lengths */
 
#define XFER_SO_SNDBUF 16384
#define XFER_SO_RCVBUF XFER_SO_SNDBUF

/* Function return codes */

#define XFER_OK        0
#define XFER_FINISHED  1
#define XFER_ERROR    -1  /* MUST be negative */

/* Data compression types */

#define XFER_CMPNONE   0x00 /* uncompressed          */
#define XFER_CMPIGPP   0x01 /* IGPP first difference */
#define XFER_CMPSTM1   0x02 /* Steim 1               */

/* relative name of the file containing authorized client IP addresses */

#define XFER_CLIENTS "etc/clients"

/* Define these to be the largest of the protocol specific lengths */

#define XFER_SNAMLEN XFER01_SNAMLEN
#define XFER_CNAMLEN XFER01_CNAMLEN
#define XFER_INAMLEN XFER01_INAMLEN

/* The following are not part of the protocol, but they do make
 * the implementation much simpler as one can just declare an
 * array big enough to hold everything.  It makes life hell when
 * they turn out to be too small, however.
 */

#ifndef   XFER_MAXSTA
#define   XFER_MAXSTA   64  /* maximum number of stations  */
#endif /* XFER_MAXSTA */

#ifndef   XFER_MAXCHN
#define   XFER_MAXCHN   64  /* maximum number of chan/sta  */
#endif /* XFER_MAXCHN */

#ifndef   XFER_MAXDAT
#define   XFER_MAXDAT 1024  /* maximum allowed data length */
#endif /* XFER_MAXDAT */

/* Other constants */

#ifndef   XFER_PORT
#define   XFER_PORT 14002 /* default port number */
#endif /* XFER_PORT */

#ifndef   XFER_SERVICE
#define   XFER_SERVICE "edep" /* Service name */
#endif /* XFER_SERVICE */

#ifndef   XFER_PROTOCOL
#define   XFER_PROTOCOL "tcp" /* default communications protocol */
#endif /* XFER_PROTOCOL */

#ifndef   XFER_DEFTO
#define   XFER_DEFTO 30  /* default socket I/O timeout in seconds */
#endif /* XFER_DEFTO */

#ifndef   XFER_MINTO
#define   XFER_MINTO 15  /* minimum socket I/O timeout in seconds */
#endif /* XFER_MINTO */

#ifndef   XFER_DEFTTO
#define   XFER_DEFTTO 5  /* default "tiny-time-out" in seconds */
#endif /* XFER_DEFTTO */

/* Reconnect policy to be used by the "EZ" interface */

#define XFER_MINIMAL_GAP   1
#define XFER_MINIMAL_DELAY 2

/* Epoch time, for data xfer purposes */

struct xfer_time {
    long sec;       /* seconds since Jan 1, 1970      */
    u_long usec;    /* factional part in microseconds */
};

/* Request preamble */

/* for protocol version 0x01 */

struct xfer01_preamble {
    long client_id;   /* client identifier (usually pid)          */
    int  format;      /* format of ack message                    */
};

/* protocol version 0x02 does not exist */

struct xfer02_preamble {
    int dummy1;
    int dummy2;
};

union xfer_preamble {
    struct xfer01_preamble ver01;
    struct xfer02_preamble ver02;
};

/* Remote configuration request */

/* for protocol version 0x01 */

/* Nothing further is required for a protocol version 0x01 config request */

/* Waveform data request */

/* for protocol version 0x01 */

struct xfer01_chnreq {
    char name[XFER01_CNAMLEN+1]; /* channel name       */
    double beg;                  /* segment begin time */
    double end;                  /* segment end   time */
};

struct xfer01_stareq {
    char name[XFER01_SNAMLEN+1];           /* station name              */
    int nchn;                              /* number of chans to follow */
    struct xfer01_chnreq chn[XFER_MAXCHN]; /* per channel requests      */
};

struct xfer01_wavreq {
    int format;               /* desired format of response packet */
    int keepup;               /* keep up time                      */
    int comp;                 /* desired compression flag          */
    int nsta;                 /* number of stations to follow      */
    struct xfer01_stareq sta[XFER_MAXSTA]; /* per station requests */
};

/* protocol version 0x02 waveform request does not yet exist */

struct xfer02_wavreq {
    int dummy1;
    int dummy2;
};

union xfer_wavreq {
    struct xfer01_wavreq ver01;
    struct xfer02_wavreq ver02;
};

/* Generic service request */

struct xfer_req {
    int  protocol;  /* protocol number, defines preamble and req  */
    long type;      /* request code, required for ALL versions    */
    int  timeout;   /* socket i/o timeout, required for ALL       */
    long sndbuf;    /* client socket transmit buffer length       */
    long rcvbuf;    /* client socket receive  buffer length       */
    union xfer_preamble preamble;
    union {
        union xfer_wavreq wav;  /* waveform data request */
    } request;
};

/* Remote configuration descriptor */

/* reply format XFER_CNFGEN1 */

struct xfer_gen1chncnf {
    char   name[XFER01_CNAMLEN+1];    /* channel name              */
    char   instype[XFER01_INAMLEN+1]; /* GSE2.0 instype            */
    int    wrdsiz;                    /* sample word size in bytes */
    unsigned long order;              /* sample byte order         */
    float  sint;                      /* nominal sample interval   */
    float  calib;                     /* calib                     */
    float  calper;                    /* calper                    */
    float  vang;                      /* GSE2.0 vang               */
    float  hang;                      /* GSE2.0 hang               */
    double beg;                       /* time of earliest datum    */
    double end;                       /* time of latest datum      */
};

struct xfer_gen1stacnf {
    char   name[XFER01_SNAMLEN+1];  /* station name                 */
    float  lat;                     /* latitutude                   */
    float  lon;                     /* longitude                    */
    float  elev;                    /* elevation                    */
    float  depth;                   /* sensor depth                 */
    int    nchn;                    /* number of channels to follow */
    struct xfer_gen1chncnf chn[XFER_MAXCHN]; /* channel descriptors */
};

struct xfer_cnfgen1 {
    u_long  order;                  /* remote host byte order       */
    int     nsta;                   /* number of stations to follow */
    struct xfer_gen1stacnf sta[XFER_MAXSTA]; /* station descriptors */
};

/* reply format XFER_CNFNRTS */

struct xfer_nrtschncnf {
    char   name[XFER01_CNAMLEN+1];    /* channel name                   */
    char   instype[XFER01_INAMLEN+1]; /* GSE2.0 instype                 */
    int    wrdsiz;                    /* sample word size in bytes      */
    unsigned long order;              /* sample byte order              */
    float  sint;                      /* nominal sample interval        */
    float  calib;                     /* calib                          */
    float  calper;                    /* calper                         */
    float  vang;                      /* GSE2.0 vang                    */
    float  hang;                      /* GSE2.0 hang                    */
    double beg;                       /* time of earliest datum         */
    double end;                       /* time of latest datum           */
    int    type;                      /* data logger type               */
    int    hlen;                      /* raw packet header length       */
    int    dlen;                      /* raw packet data length         */
    long   nrec;                      /* number of records in disk loop */
    long   nhide;                     /* number of "hidden" records     */
    time_t latency;                   /* time since data were written   */
};

struct xfer_nrtsstacnf {
    char   name[XFER01_SNAMLEN+1];    /* station name                 */
    float  lat;                       /* latitutude                   */
    float  lon;                       /* longitude                    */
    float  elev;                      /* elevation                    */
    float  depth;                     /* sensor depth                 */
    int    nchn;                      /* number of channels to follow */
    struct xfer_nrtschncnf chn[XFER_MAXCHN]; /* channel descriptors   */
};

struct xfer_cnfnrts {
    u_long  order;    /* remote host byte order        */
    int     nsta;     /* number of stations to follow  */
    struct xfer_nrtsstacnf sta[XFER_MAXSTA]; /* station descriptors */
};

/* All purpose configuration record */

struct xfer_cnf {
    int format;                /* record format */
    union {
        struct xfer_cnfgen1 gen1;
        struct xfer_cnfnrts nrts;
    } type;                    /* format specific info */
};

/* Waveform packet */

/* format XFER_WAVGEN1 */

struct xfer_wavgen1 {
    int    standx;              /* station index                  */
    int    chnndx;              /* channel index                  */
    double tofs;                /* time of first sample           */
    int    tear;                /* time tear flag                 */
    int    comp;                /* compression flag               */
    long   nsamp;               /* number of uncompressed samples */
    long   nbyte;               /* number of bytes to follow      */
    char   data[XFER_MAXDAT*4]; /* data, compressed as per comp   */
};

/* format XFER_WAVRAW */

struct xfer_wavraw {
    long nbyte;             /* number of bytes to follow  */
    char data[XFER_MAXDAT]; /* raw data logger packet     */
};

/* format XFER_WAVIDA */

struct xfer_wavida {
    int  rev;               /* IDA data format rev code   */
    int  comp;              /* compression flag           */
    long nbyte;             /* number of bytes to follow  */
    char data[IDA_PAKLEN];  /* raw data logger packet     */
};

/* format XFER_WAVPASSCAL */

struct xfer_wavpas {
    long nbyte;            /* number of bytes to follow   */
    char data[PAS_PAKLEN]; /* raw data logger packet      */
};

/* format XFER_WAVSEED */

struct xfer_wavseed {
    long nbyte;             /* number of bytes to follow  */
    char data[SEED_PAKLEN]; /* one mini-SEED record       */
};

/* All purpose waveform record */

struct xfer_wav {
    int format;                /* record format */
    union {
        struct xfer_wavgen1 gen1;
        struct xfer_wavraw  raw;
        struct xfer_wavida  ida;
        struct xfer_wavpas  pas;
        struct xfer_wavseed seed;
    } type;                    /* format specific info */
};

/* This implementation offers an API that masks all details of
 * the protocol and underlying data formats.  For casual users who
 * don't require some special return format, like NRTS or SEED,
 * the following structure contains everything the protocol
 * can provide and you don't have to bother with the storing
 * and referencing of cnf packets.
 */

struct xfer_packet {
    char   sname[XFER_SNAMLEN+1];  /* station name                     */
    float  lat;                    /* latitutude                       */
    float  lon;                    /* longitude                        */
    float  elev;                   /* elevation                        */
    float  depth;                  /* sensor depth                     */
    char   cname[XFER_CNAMLEN+1];  /* channel name                     */
    char   instype[XFER_INAMLEN+1];/* GSE2.0 instype                   */
    float  sint;                   /* nominal sample interval, sec     */
    float  calib;                  /* CSS 3.0 calib                    */
    float  calper;                 /* CSS 3.0 calper                   */
    float  vang;                   /* vertical orientation of sensor   */
    float  hang;                   /* horizontal orientation of sensor */
    double beg;                    /* time of first sample in packet   */
    double end;                    /* time of last  sample in packet   */
    int    tear;                   /* if set, there was a time tear    */
    long   nsamp;                  /* number of samples                */
    long   *data;                  /* data, as properly ordered longs  */
/* The following are for (possible) internal use only */
    long   *dbuf[XFER_MAXDAT*4];   /* raw data storage (if required)   */
    int    hlen;                   /* raw packet header length         */
    int    dlen;                   /* raw packet data length           */
    char   *hdr;                   /* start of raw packet header       */
    char   *dat;                   /* start of raw packet data         */
};

/* This structure gets passed around by the Xfer_Open/Close/Read
 * routines.  It holds all the things necessary for the underlying
 * functions to deal with submitting requests, dealing with timeouts
 * and reconnects, and reformatting into the above packet format.
 */

struct xfer_tapinfo {
  /* user supplied */
    char host[MAXHOSTNAMELEN+1];/* server name  */
    int port;    /* port number                 */
    int keepup;  /* keepup flag                 */
    int retry;   /* retry flag                  */
    int tto;     /* tiny-time-out for connect() */
  /* internal use only */
    int sd;              /* socket descriptor             */
    struct xfer_req req; /* stores request information    */
    struct xfer_cnf cnf; /* stores returned configuration */
    struct xfer_wav wav; /* stores returned waveform      */
  /* Mapping from cnf indices to req indicies */
    int rsi[XFER_MAXSTA];
    int rci[XFER_MAXSTA][XFER_MAXCHN];
  /* Mapping from req indices to cnf indicies */
    int csi[XFER_MAXSTA];
    int cci[XFER_MAXSTA][XFER_MAXCHN];
};

typedef struct xfer_tapinfo XFER;

/* You can use this object to declare a buffer that will hold the
 * largest of all possible xfer related data structures.
 *
 * For example:
 * static xfer_buffer xbuf;
 * char *buffer;
 *
 * buffer = (char *) &xbuf;
 *
 */

union xfer_buffer {
    char wavreq[ sizeof(u_long) + sizeof(union xfer_wavreq) ];
    char cnf   [ sizeof(u_long) + sizeof(struct xfer_cnf)   ];
    char wav   [ sizeof(u_long) + sizeof(struct xfer_wav)   ];
};

/* Macros */

/* Original Xfer_Connect() did not support the "tiny-time-out" option.
 * This macro ought to prevent breaking any existing code that is
 * happy with the default connect() timeout behaviour.
 */

#define Xfer_Connect(a, b, c, d, e, f, g) \
       Xfer_Connect2(a, b, c, d, e, f, g, 0)

/* The "EZ" interface does not want users coming in on anything
 * other than the standard port, nor should they concern themselves
 * with the various timeout settings.  The following macro allows
 * us to hide that, but still permit modification of those parameters
 * in special cases (by calling Xfer_Open2() directly).
 */

#define Xfer_Open(host,     sc, beg, end, keepup, retry) \
       Xfer_Open2(host, 0,  sc, beg, end, keepup, retry, 0, 0)

/* External use functions */

void Xfer_Ack(
    int,
    int
);

int Xfer_Allowed(
    int
);

void Xfer_Break(
    int
);

void Xfer_Close(
    XFER *
);

void Xfer_Exit(
    int,
    int
);

int Xfer_Compress(
    struct xfer_cnf *,
    struct xfer_wav *
);

int Xfer_Connect2(
    char *,
    char *,
    int,
    char *,
    struct xfer_req *,
    struct xfer_cnf *,
    int,
    int
);

int Xfer_Decompress(
    struct xfer_cnf *,
    struct xfer_wav *wav
);

char *Xfer_ErrMsg(
    int
);

int Xfer_FillReq(
    struct xfer_req *,
    int,
    int,
    char *,
    double,
    double,
    int,
    int
);
    
char *Xfer_HeartBeat(
    int
);

XFER *Xfer_Open2(
    char *,
    int,
    char *,
    double,
    double,
    int,
    int,
    int,
    int
);

int Xfer_ParseCC(
    struct xfer_req *,
    int,
    char *,
    double,
    double
);

int Xfer_ParseSC(
    struct xfer_req *,
    char *,
    double,
    double
);

int Xfer_ParseSCSC(
    struct xfer_req *,
    char *,
    double,
    double
);

char *Xfer_Peer(
    int
);

int Xfer_Read(
    XFER *,
    struct xfer_packet *
);

int Xfer_RdPacket(
    FILE *,
    struct xfer_packet *
);

int Xfer_RecvReq(
    int,
    struct xfer_req *
);

int Xfer_RecvWav(
    int,
    struct xfer_cnf *,
    struct xfer_wav *
);

void Xfer_SendCnf(
    int,
    struct xfer_cnf *
);

int Xfer_SendWav(
    int,
    struct xfer_cnf *,
    struct xfer_wav *
);

int Xfer_WrtPacket(
    FILE *,
    struct xfer_packet *
);

#ifdef CSS_SUPPORT

int Xfer_ToCSS(
    FILE *,
    int
);

#endif /* CSS_SUPPORT */

#ifdef GSE_SUPPORT

int Xfer_ToGSE(
    FILE *,
    int,
    int,
    int,
    int,
    int
);

#endif /* GSE_SUPPORT */

#ifdef SAC_SUPPORT

int Xfer_ToSAC(
    FILE *,
    int,
    struct sac_header *,
    int
);

#endif /* SAC_SUPPORT */

#ifdef SEED_SUPPORT

int Xfer_ToSEED(
    FILE *,
    char *,
    int,
    char *,
    int
);

#endif /* SEED_SUPPORT */

#ifdef NRTS_SUPPORT

int Xfer_CvtNRTS(
    struct xfer_cnf *,
    struct xfer_wav *,
    struct nrts_packet *
);

#endif /* NRTS_SUPPORT */

/* Internal use functions */

void xfer_Alarm(
    int
);

void xfer_CatchSignal(
    int
);

void xfer_ClearTimer(
    void
);

int xfer_DecodeChnCnf(
    void *,
    char **,
    int
);

int xfer_Convert(
    struct xfer_cnf *,
    struct xfer_wav *,
    struct xfer_packet *
);

int xfer_DecodeCnf(
    struct xfer_cnf *,
    char *
);

int xfer_DecodePreamble(
    union xfer_preamble *,
    char **,
    int
);

int xfer_DecodeReq(
    struct xfer_req *,
    char *
);

int xfer_DecodeStaCnf(
    void *,
    char **,
    int
);

int xfer_DecodeWav(
    struct xfer_cnf *,
    struct xfer_wav *,
    char *
);

int xfer_DecodeWavReq(
    union xfer_wavreq *,
    char **,
    int
);

long xfer_EncodeChnCnf(
    char *,
    void *,
    int
);

long xfer_EncodeChnReq(
    char *,
    void *,
    int
);

long xfer_EncodeCnf(
    char *,
    struct xfer_cnf *
);

long xfer_EncodePreamble(
    char *,
    union xfer_preamble *,
    int
);

long xfer_EncodeReq(
    char *,
    struct xfer_req *
);

long xfer_EncodeStaCnf(
    char *,
    void *,
    int
);

long xfer_EncodeStaReq(
    char *,
    void *,
    int
);

long xfer_EncodeWav(
    char *,
    struct xfer_wav *
);

long xfer_EncodeWavReq(
    char *,
    union xfer_wavreq *,
    int
);

long xfer_Read(
    int,
    char *,
    long
);

int xfer_RecvCnf(
    int,
    struct xfer_cnf *
);

int xfer_RecvMsg(
    int,
    char *,
    long,
    long *
);

int xfer_SendMsg(
    int,
    char *,
    long
);

int xfer_SendReq(
    int,
    struct xfer_req *
);

void xfer_SetTimer(
    void
);

int xfer_Signals(
    void
);

long xfer_Write(
    int,
    char *,
    long
);

struct xfer_time *xfer_time(
    double
);

double xfer_dtime(
    struct xfer_time *
);

#ifdef IDA_SUPPORT

int ida_compress(
    int,
    int *,
    char *,
    char *
);

char *ida_decompress(
    int,
    int *,
    long *,
    char *
);

#endif

#endif /* xfer_h_included */
