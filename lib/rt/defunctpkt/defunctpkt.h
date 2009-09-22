/*======================================================================
 *
 *  pkt.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *====================================================================*/

#ifndef pkt_h_included
#define pkt_h_included
#include <fcntl.h>
#include <malloc.h>
#include <sys/types.h>
#include <netinet/in.h>
#include "_pkt.h"
#include "stock.h"
#include "orb.h"



 /* RAW packet type  */

#define B3S2DP 0x4441		       /* B3S2 Data Packet                  */
#define B3S2SP 0x5354		       /* B3S2 Data Packet                  */
#define PSCLAD 0x4144		       /* PASSCAL Auxiliary Data Def Packet    */
#define PSCLCD 0x4344		       /* PASSCAL Calibration Def Packet       */
#define PSCLDR 0x4452		       /* PASSCAL ?????Packet                  */
#define PSCLDS 0x4453		       /* PASSCAL Data Stream DEf              */
#define PSCLDT 0x4454		       /* PASSCAL Data Packet                  */
#define PSCLEH 0x4548		       /* PASSCAL Event Header Packet          */
#define PSCLET 0x4554		       /* PASSCAL Event Trailer Packet         */
#define PSCLOM 0x4F4D		       /* PASSCAL Operational Mode Packet      */
#define PSCLSH 0x5348		       /* PASSCAL State-Of-Health Packet       */
#define PSCLSC 0x5343		       /* PASSCAL Station/Channel Packet       */
#define CBBHS  0xBBCD		       /* BBArray High Speed Data Packet       */
#define CBBLS  0xBBDE		       /* BBArray LOW Speed Data Packet        */
#define CBB1S  0xBBBC		       /* BBArray LOW Speed Data Packet        */
#define CAHS   0xABCD		       /* ANZA High Speed Data Packet          */
#define CALS   0xABDE		       /* ANZA Low Speed Data Packet           */
#define ASP    0xABEF		       /* ANZA Status Packet                   */
#define BSP    0xBBDC		       /* ANZA Status Packet                   */
#define IWTB   0xAADB		       /* 'iwtb' Alaska Data Buffer           */
#define ORBDBUG 0xDEB		       /* 'gubd' */

/* PASSCAL data packet type  */

#define PSCLHS  0x10
#define CPSCLHS 0x100
#define PSCLLS  0x40
#define CPSCLLS 0x400
#define PSCLIP  0x111

/* Quanterra types */

#define QUANTERRA_DATA_PKT_TYPE 0x7164 /* 'qd' */
#define QUANTERRA_DATA_HDR_TYPE 0x7164 /* 'qd' */

#define QUANTERRA_DATA_PKT_TYPE2 0x7165/* 'qe' */
#define QUANTERRA_DATA_HDR_TYPE2 0x7165/* 'qe' */

#define QUANTERRA_LOG_PKT_TYPE  0x716c /* 'ql' */
#define QUANTERRA_LOG_HDR_TYPE  0x716c /* 'ql' */

#define LISSPKT  0x6c6c

 /* Packets header type */

#define DPH     0xDDD		       /* 'dph' */
#define BBA     0xBBA		       /* 'bba' */
#define IPH     0xCBF		       /* 'iph' */
#define IWH     0xADB		       /* 'iwh' */


/* Status Parameters Packet ( DC & DAS status parameters );
   Packet type and header type
*/

#define SPP    0x5350

typedef struct Orbpipe_packet {
    double          time;
    int             pktid;
    char            srcname[ORBSRCNAME_SIZE];
    char           *packet;
    int             nbytes,
                    bufsize;
}               Orbpipe_packet;

typedef struct Orbpipe {
    Tbl            *tbl;
    int             maxpkts;
    double          last;
    Orbpipe_packet  rpkt;
}               Orbpipe;

/* 
    RAW Packets Parsing & reading routines; 
	RD - read; 
	PRS - parse; 
	DP - Data * Packet; 
	SP - Status Packet  
*/

int             none ();

typedef struct Raw {
    ushort_t        pkttype;
    int             (*parse) ();
    int             (*read) ();
}               Raw;


/*  ORB Packet structure/types definitions  */

#define PKT_NAMESIZE 10

typedef struct PktChannel {
    char            net[PKT_NAMESIZE],
                    sta[PKT_NAMESIZE],
                    chan[PKT_NAMESIZE];
    char            segtype[4];
    double          time,
                    samprate,
                    calib;
    int             nsamp,
                    nbytes;
    int             datatype;
    void           *data;
}               PktChannel;

typedef struct Packet {
    int             nchannels;
    int             pkttype;
    int             hdrtype;
    Tbl            *chan;
}               Packet;


/* Client Packet headers . Do NOT move around structure fields! */
struct PreHdr {
    short           hdrsiz;	       /* header size */
    short           pktsiz;	       /* raw packet size */
    ushort_t        hdrtype;	       /* header type  */
    ushort_t        pkttype;	       /* packet type tag  */
};

typedef struct IPHdr {
    struct PreHdr   prehdr;
    double          epoch;
    char            net[PKT_NAMESIZE];
}               IPHdr;

typedef struct DPHdr {
    struct PreHdr   prehdr;
    char            net[PKT_NAMESIZE]; /* network type */
    char            sta[PKT_NAMESIZE]; /* Sta name */
    char            channels[ORBSRCNAME_SIZE];	/* channels names  */
    int             datatype;
    double          epoch;	       /* calibration coef */
    double          calib;	       /* calibration coef */
    float           samprate;	       /* SAMPRATE */
    int             nsamp;	       /* sample #  */
    int             nchan;	       /* number of channels per packet  */
    int             doff;	       /* offset to the data  */

}               DPHdr;

typedef struct BBAHdr {
    struct PreHdr   prehdr;
    float           calib;	       /* calibration coef */
    float           samprate;	       /* SAMPRATE */
    short           datatype;
    short           nsamp;	       /* sample #  */
    short           nchan;	       /* number of channels per packet  */
    short           doff;	       /* offset to the data  */
    short           chanlen;	       /* offset to the data  */
    char            channels[ORBSRCNAME_SIZE];	/* channels names  */

}               BBAHdr;

typedef struct SPHdr {
    struct PreHdr   prehdr;
    short           dcnum;
    short           dasnum;
    float           samprate;
}               SPHdr;

struct DataPar {
    int             hdrsize;
    int             pktsize;
    int             datatype;
    int             doff;
    int             nsamp;
    ushort_t        hdrtype;
    ushort_t        pkttype;
};


typedef struct {
    float           samprate;	       /* Sample rate */
    float           calib;	       /* calib */
    short           pinno;	       /* Pin number */
    short           nsamp;	       /* Number of samples in packet */
    char            datatype[2];       /* Data format code */
    char            quality[2];	       /* Data-quality field */
}               IW_ORB_TRACE_HEADER;

typedef struct Prm {
    double          time;
    int             hdrtype;
    int             staid;
    int             chan;
    int             stream;
    PktPar          packet;
    Raw             raw;
}               Prm;

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
    gc      generic compressed data 
*/


/* int             Raw_decomp; */

/* Parameters name for querypkt call  */

#define pktBYTES 		900    /* data packet size in bytes  */
#define pktHDRSIZE 		901    /* data packet header size in bytes  */
#define pktSTANUM		902    /* # of channels/packet */
#define pktCHANNEL_COUNT	903    /* # of channels/packet */
#define pktCHANNEL_NSAMP	904    /* # sample/packet/channel */
#define pktCHANNEL_BYTES	905    /* # bytes/channel */
#define pktDATATYPE		906    /* data type  */
#define pktSMPRATE		907    /* sample rate  */
#define pktNET			908    /* net type  */
#define pktSOURCE		909    /* net type  */

/* External functions  */

extern void    *bcdval (uchar_t * data, int code, int nbyte);
extern int      bcdhex (uchar_t * data, int nbyte);
extern void     bsname (char *fname, char *name, char *exten);
extern void     pathfrname (char *path, char *name);
extern void     fexten (char *fname, char *name);
extern void     initpf (char *pf);
extern void     init_StaCh (void);
extern void     init_StaID (void);
extern void     init_RawPkts (void);
extern void     init_packets (void);
extern void     init_stream (Stream * stream);
extern int      set_Stream (int id, int dasid, Stream * stream);
extern void	freePkt (Packet * Pkt);
extern void	freePktCh (PktChannel * PktCh);
extern int      str2int (uchar_t * str, int i);
extern int      get_site (char *pkttype, int staid, int chid, Site * site);
extern int      get_staid (char *pkttype, char *sta, Site * site);
extern int      get_packet (ushort_t key, PktPar * pack);
extern int      get_raw (ushort_t key, Raw * raw);
extern int      match_stream (Stream ** new, Stream * s2);
extern DASPar  *getDAS (int dasid);
extern int      hdr2packet (char **packet, int hdrtype, char *srcname);
extern int      packet_frhdr (char **packet);
extern int      decode (char *token);
extern int      stapar (uchar_t * data, char *sta, char *chan, int *chlen, double *calib);
extern int      whatis_pkttype (uchar_t * packet);
extern int      querypkt (int pkttype, int pcode, void *result);
extern int      ucsd_ucompress (int *smpptr, char *buf, int smpcnt, int nchan);
extern int      pscl_ucompress (char **data, int nsamp, int doff);
extern int      parse_raw (uchar_t * packet, int (*parse) (), ushort_t pkttype);
extern int      parse_pscl_IP (uchar_t * packet, ushort_t pkttype);
extern int      parse_anza_SP (uchar_t * packet, ushort_t pkttype);
extern int      parse_anza_DP (uchar_t * packet, ushort_t pkttype);
extern int      parse_b3s2 (uchar_t * packet, ushort_t pkttype);
extern int      parse_bba_DP (uchar_t * packet, ushort_t pkttype);
extern int      parse_pscl_DP (uchar_t * packet, ushort_t pkttype);
extern int	read_raw (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private, int (*rd) ());
extern int      read_anza_SP (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_DP (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_DBUG (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_b3s2_DP (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_b3s2_SP (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_psclDP (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *par);
extern int      read_pscl_AD (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_CD (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_DS (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_EH (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_ET (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_OM (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_SH (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_pscl_SC (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      read_sppar (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      stuff_liss (char **data, char *srcname, double *time, int size);
extern int      unstuff_liss (double time, char *srcid, uchar_t * packet, Packet ** Pkt, void *private);
extern int      stuff_iw_tracebuf (ushort_t quality, ushort_t pinno, PktChannel * pktchan, char **packet, int *nbytes, int *bufsiz);
extern int      unstuff_iw_tracebuf (double time, char *srcid, char *packet, Packet ** Pkt, void *private);
extern int      stuffpkttime (char *packet, double time);
extern int      unstuffpkt (double time, char *srcid, char *packet, Packet ** Pkt);
extern int      unstuff_IPhdr (uchar_t * packet);
extern int      unstuff_BBAHdr (uchar_t * packet, Packet ** Pkt, void *private);
extern int      unstuff_DPHdr (uchar_t * packet, Packet ** Pkt, void *private);
extern int      st_dbug (Packet * unstuffed, char **stuffed, int *nalloc, char *srcname, double *time, int *size);
extern int      un_dbug (double time, char *srcid, char *stuffed, Packet ** unstuffed, void *private);
extern Packet  * newpkt (void);
extern int      netstachan (int orb, Arr ** channels, Arr ** srcnames);
extern int      orbacc (int orb, char *accselect, double req_twin, double req_overlap, double latency, Dbptr * trp, Hook ** hookp);

extern int      unstuffqorbpkt (double time, char *srcname, char *packet, Packet ** pkt, void *private);

extern Orbpipe *new_orbpipe (int maxpkts);
extern void     free_orbpipe (Orbpipe * pipe, void (*free_packet) (char *));
extern int      orbsort (Orbpipe * pipe, int *rpktid, double *rtime, char *rsrcname, char **rpacket, int *rnbytes, int *rbufsize);

#endif
