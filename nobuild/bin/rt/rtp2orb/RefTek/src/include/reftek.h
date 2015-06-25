#pragma ident "$Id$"

#ifndef reftek_h_included
#define reftek_h_included

#include "rtp.h"
#include "util.h"

/* Misc constants */

#define REFTEK_MAXPAKLEN 1024 /* largest raw packet size              */
#define REFTEK_MAXDAT    1000 /* largest sample data bytes            */
#define REFTEK_MAXC0      892 /* max number of C0 compressed samples  */
#define REFTEK_MAXNAMLEN   64 /* max ascii name length                */
#define REFTEK_MAXNSTRM     4 /* max number of streams in a DS record */

/* packet types */

#define REFTEK_AD   RTP_PMASK_AD
#define REFTEK_CD   RTP_PMASK_CD
#define REFTEK_DS   RTP_PMASK_DS
#define REFTEK_DT   RTP_PMASK_DT
#define REFTEK_EH   RTP_PMASK_EH
#define REFTEK_ET   RTP_PMASK_ET
#define REFTEK_OM   RTP_PMASK_OM
#define REFTEK_SH   RTP_PMASK_SH
#define REFTEK_SC   RTP_PMASK_SC
#define REFTEK_SPEC RTP_PMASK_SPEC
#define REFTEK_CMND RTP_PMASK_CMND

/* Data format codes */

#define REFTEK_F16  1  /* 16-bit uncompressed */
#define REFTEK_F32  2  /* 32-bit uncompressed */
#define REFTEK_FC0  3  /* Steim 1 compressed  */

/* Auxlliary Data (AD) packet */

struct reftek_ad {
    UINT16 exp;                  /* experiment number      */
    UINT16 unit;                 /* unit id                */
    UINT16 seqno;                /* sequence number        */
    REAL64 tstamp;               /* timestamp              */
    REAL32 sint;                 /* sample interval        */
    UINT16 format;               /* data format flag       */
};

/* Calibration Definition (CD) packet */

struct reftek_cd {
    UINT16 exp;      /* experiment number      */
    UINT16 unit;     /* unit id                */
    UINT16 seqno;    /* sequence number        */
    REAL64 tstamp;   /* timestamp              */
};

/* Data Stream (DS) packet */

#define REFTEK_TRGCON 1
#define REFTEK_TRGCRS 2
#define REFTEK_TRGEVT 3
#define REFTEK_TRGEXT 4
#define REFTEK_TRGLVL 5
#define REFTEK_TRGRAD 6
#define REFTEK_TRGTIM 7

struct reftek_contrg {
    REAL32 dur;       /* event duration (sec) */
};

struct reftek_crstrg {
    UINT16 key;       /* trigger stream number   */
    REAL32 pretrig;   /* pretrigger length (sec) */
    REAL32 dur;       /* event duration (sec)    */
};

struct reftek_evttrg {
    UINT16 dummy1;
    UINT16 dummy2;
};

struct reftek_exttrg {
    UINT16 dummy1;
    UINT16 dummy2;
};

struct reftek_lvltrg {
    UINT16 dummy1;
    UINT16 dummy2;
};

struct reftek_radtrg {
    UINT16 dummy1;
    UINT16 dummy2;
};

struct reftek_timtrg {
    UINT16 dummy1;
    UINT16 dummy2;
};

struct reftek_stream {
    UINT16 ident;                    /* stream number   */
    CHAR   name[REFTEK_MAXNAMLEN+1]; /* stream name     */
    REAL32 sint;                     /* sample interval */
    UINT16 trgtype;                  /* trigger type    */
    union {
        struct reftek_contrg con;
        struct reftek_contrg crs;
        struct reftek_contrg evt;
        struct reftek_contrg ext;
        struct reftek_contrg lvl;
        struct reftek_contrg rad;
        struct reftek_contrg tim;
    } trg;                          /* trigger info     */
};

struct reftek_ds {
    UINT16 exp;      /* experiment number                   */
    UINT16 unit;     /* unit id                             */
    UINT16 seqno;    /* sequence number                     */
    REAL64 tstamp;   /* timestamp                           */
    UINT16 nstream;  /* no. stream descriptors to follow    */
    struct reftek_stream stream[REFTEK_MAXNSTRM]; /* stream descriptors */
};

/* Data (DT) packet */

struct reftek_dt {
    UINT16 exp;                  /* experiment number            */
    UINT16 unit;                 /* unit id                      */
    UINT16 seqno;                /* sequence number              */
    REAL64 tstamp;               /* timestamp                    */
    UINT16 evtno;                /* event number                 */
    UINT16 stream;               /* stream id                    */
    UINT16 chan;                 /* channel id                   */
    UINT16 nsamp;                /* number of samples            */
    UINT16 format;               /* data format flag             */
    INT32 *data;                 /* pointer to decompressed data */
    UINT8  raw[REFTEK_MAXDAT];   /* raw data                     */
    INT32  dcmp[REFTEK_MAXC0];   /* decompressed data            */
    BOOL   dcerr;                /* decompression error flag     */
};

/* Event Header (EH) packet */

struct reftek_eh {
    UINT16 exp;                  /* experiment number      */
    UINT16 unit;                 /* unit id                */
    UINT16 seqno;                /* sequence number        */
    REAL64 tstamp;               /* timestamp              */
    UINT16 evtno;                /* event number           */
    UINT16 stream;               /* stream id              */
    UINT16 format;               /* data format flag       */
    CHAR   name[REFTEK_MAXNAMLEN+1]; /* stream name        */
    REAL32 sint;                 /* sample interval        */
    UINT16 trgtype;              /* trigger type           */
    REAL64 on;                   /* trigger time           */
    REAL64 tofs;                 /* time of first sample   */
};

/* Event Trailer (ET) packet */

struct reftek_et {
    UINT16 exp;                  /* experiment number      */
    UINT16 unit;                 /* unit id                */
    UINT16 seqno;                /* sequence number        */
    REAL64 tstamp;               /* timestamp              */
    UINT16 evtno;                /* event number           */
    UINT16 stream;               /* stream id              */
    UINT16 format;               /* data format flag       */
    CHAR   name[REFTEK_MAXNAMLEN+1]; /* stream name        */
    REAL32 sint;                 /* sample interval        */
    UINT16 trgtype;              /* trigger type           */
    REAL64 on;                   /* trigger on  time       */
    REAL64 off;                  /* trigger off time       */
    REAL64 tofs;                 /* time of first sample   */
    REAL64 tols;                 /* time of last  sample   */
};

/* Operating Mode (OM) packet */

struct reftek_om {
    UINT16 exp;                  /* experiment number      */
    UINT16 unit;                 /* unit id                */
    UINT16 seqno;                /* sequence number        */
    REAL64 tstamp;               /* timestamp              */
};

/* Station/Channel (SC) packet */
/* Only some fields currently supported */

#define REFTEK_MAXSCCHN 5

struct reftek_sc {
    UINT16 exp;                  /* experiment number      */
    UINT16 unit;                 /* unit id                */
    UINT16 seqno;                /* sequence number        */
    REAL64 tstamp;               /* timestamp              */
    UINT16 nchan;                /* no. of channels below  */
    struct {
        UINT16 num;              /* channel number */
        CHAR   name[10+1];       /* channel name   */
        REAL32 gain;             /* pre-amp gain   */
        CHAR   model[12+1];      /* sensor model   */
        CHAR   sn[12+1];         /* sensor S/N     */
        REAL32 scale;            /* volts/bit      */
    } chan[REFTEK_MAXSCCHN];
};

/* State of Health (SH) packet */

struct reftek_sh {
    UINT16 exp;      /* experiment number      */
    UINT16 unit;     /* unit id                */
    UINT16 seqno;    /* sequence number        */
    REAL64 tstamp;   /* timestamp              */
};

/* Function prototypes */

VOID reftek_com(UINT8 *src, UINT16 *exp, UINT16 *unit, UINT16 *seqno, REAL64 *tstamp);

BOOL reftek_ad(struct reftek_ad *dest, UINT8 *src);
CHAR *reftek_adstr(struct reftek_ad *ad, CHAR *buf);

BOOL reftek_cd(struct reftek_cd *dest, UINT8 *src);
CHAR *reftek_cdstr(struct reftek_cd *cd, CHAR *buf);

BOOL reftek_ds(struct reftek_ds *dest, UINT8 *src);
CHAR *reftek_dsstr(struct reftek_ds *ds, CHAR *buf);

BOOL reftek_dt(struct reftek_dt *dest, UINT8 *src, BOOL convert);
CHAR *reftek_dtstr(struct reftek_dt *dt, CHAR *buf);

BOOL reftek_eh(struct reftek_eh *dest, UINT8 *src);
CHAR *reftek_ehstr(struct reftek_eh *eh, CHAR *buf);

BOOL reftek_et(struct reftek_et *dest, UINT8 *src);
CHAR *reftek_etstr(struct reftek_et *et, CHAR *buf);

BOOL reftek_om(struct reftek_om *dest, UINT8 *src);
CHAR *reftek_omstr(struct reftek_om *om, CHAR *buf);

BOOL reftek_sc(struct reftek_sc *dest, UINT8 *src);
CHAR *reftek_scstr(struct reftek_sc *sc, CHAR *buf);

BOOL reftek_sh(struct reftek_sh *dest, UINT8 *src);
CHAR *reftek_shstr(struct reftek_sh *sh, CHAR *buf);

UINT16 reftek_type(UINT8 *raw);
CHAR *reftek_str(UINT8 *pkt, CHAR *buf);
CHAR *reftek_comstr(UINT8 *pkt, CHAR *buf);

#endif /* reftek_h_included */

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.2  2002/01/18 17:49:01  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
