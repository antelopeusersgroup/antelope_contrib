/***********************************************************************//**
 * @file libdali.h
 * 
 * Interface declarations for the DataLink library (libdali).
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License (GNU-LGPL) for more details.  The
 * GNU-LGPL and further information can be found here:
 * http://www.gnu.org/
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2011.003
 ***************************************************************************/

#ifndef LIBDALI_H
#define LIBDALI_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include "portable.h"

#define LIBDALI_VERSION "1.6"        /**< libdali version */
#define LIBDALI_RELEASE "2013.210"   /**< libdali release date */

#define MAXPACKETSIZE       16384    /**< Maximum packet size for libdali */
#define MAXREGEXSIZE        16384    /**< Maximum regex pattern size */
#define MAX_LOG_MSG_LENGTH  200      /**< Maximum length of log messages */

#define LIBDALI_POSITION_EARLIEST -2 /**< Earliest position in the buffer */
#define LIBDALI_POSITION_LATEST   -3 /**< Latest position in the buffer */

/** Maximium stream ID string length */
#define MAXSTREAMID 60

/* Return values for dl_collect() and dl_collect_nb() */
#define DLERROR    -1      /**< Error occurred */
#define DLENDED     0      /**< Connection terminated */
#define DLPACKET    1      /**< Packet returned */
#define DLNOPACKET  2      /**< No packet for non-blocking dl_collect_nb() */

/** High precision time tick interval as 1/modulus seconds *
 * Default modulus of 1000000 defines tick interval as a microsecond */
#define DLTMODULUS 1000000

/** Error code for routines that normally return a high precision time.
 * The time value corresponds to '1902/1/1 00:00:00.000000' with the
 * default DLTMODULUS */
#define DLTERROR -2145916800000000LL

/** Macro to scale a Unix/POSIX epoch time to a high precision time */
#define DL_EPOCH2DLTIME(X) X * (dltime_t) DLTMODULUS
/** Macro to scale a high precision time to a Unix/POSIX epoch time */
#define DL_DLTIME2EPOCH(X) X / DLTMODULUS
  
/** Data type for high-precision time values.
 *  Require a large (>= 64-bit) integer type */
typedef int64_t dltime_t;

/** Logging parameters */
typedef struct DLLog_s
{
  void (*log_print)();          /**< Function pointer for log message printing */
  const char *logprefix;        /**< Log message prefix */
  void (*diag_print)();         /**< Function pointer for diagnostic/error message printing */
  const char *errprefix;        /**< Error message prefix */
  int  verbosity;               /**< Verbosity level */
} DLLog;

/** DataLink connection parameters */
typedef struct DLCP_s
{
  char        addr[100];        /**< The host:port of DataLink server */
  char        clientid[200];    /**< Client program ID as "progname:username:pid:arch", see dlp_genclientid() */
  int         keepalive;        /**< Interval to send keepalive/heartbeat (seconds) */
  int         iotimeout;        /**< Timeout for network I/O operations (seconds) */
  
  /* Connection parameters maintained internally */
  int         link;		/**< The network socket descriptor, maintained internally */
  float       serverproto;      /**< Server version of the DataLink protocol, maintained internally */
  int32_t     maxpktsize;       /**< Maximum packet size for server, maintained internally */
  int8_t      writeperm;        /**< Write permission status from server, maintained internally */
  int64_t     pktid;            /**< Packet ID of last packet received, maintained internally */
  dltime_t    pkttime;          /**< Packet time of last packet received, maintained internally */
  int8_t      keepalive_trig;   /**< Send keepalive trigger, maintained internally */
  dltime_t    keepalive_time;   /**< Keepalive time stamp, maintained internally */
  int8_t      terminate;        /**< Boolean flag to control connection termination, maintained internally */
  int8_t      streaming;        /**< Boolean flag to indicate streaming status, maintained internally */
  
  DLLog      *log;              /**< Logging parameters, maintained internally */
} DLCP;

/** DataLink packet */
typedef struct DLPacket_s
{
  char        streamid[MAXSTREAMID]; /**< Stream ID */
  int64_t     pktid;            /**< Packet ID */
  dltime_t    pkttime;          /**< Packet time */
  dltime_t    datastart;        /**< Data start time */
  dltime_t    dataend;          /**< Data end time */
  int32_t     datasize;         /**< Data size in bytes */
} DLPacket;


/* connection.c */
extern DLCP *  dl_newdlcp (char *address, char *progname);
extern void    dl_freedlcp (DLCP *dlconn);
extern int     dl_exchangeIDs (DLCP *dlconn, int parseresp);
extern int64_t dl_position (DLCP *dlconn, int64_t pktid, dltime_t pkttime);
extern int64_t dl_position_after (DLCP *dlconn, dltime_t datatime);
extern int64_t dl_match (DLCP *dlconn, char *matchpattern);
extern int64_t dl_reject (DLCP *dlconn, char *rejectpattern);
extern int64_t dl_write (DLCP *dlconn, void *packet, int packetlen, char *streamid,
			 dltime_t datastart, dltime_t dataend, int ack);
extern int     dl_read (DLCP *dlconn, int64_t pktid, DLPacket *packet,
			void *packetdata, size_t maxdatasize);
extern int     dl_getinfo (DLCP *dlconn, const char *infotype, char *infomatch,
			   char **infodata, size_t maxinfosize);
extern int     dl_collect (DLCP *dlconn, DLPacket *packet, void *packetdata,
			   size_t maxdatasize, int8_t endflag);
extern int     dl_collect_nb (DLCP *dlconn, DLPacket *packet, void *packetdata,
			      size_t maxdatasize, int8_t endflag);
extern int     dl_handlereply (DLCP *dlconn, void *buffer, int buflen, int64_t *value);
extern void    dl_terminate (DLCP *dlconn);

/* config.c */
extern char   *dl_read_streamlist (DLCP *dlconn, const char *streamfile);

/* network.c */
extern int     dl_connect (DLCP *dlconn);
extern void    dl_disconnect (DLCP *dlconn);
extern int     dl_senddata (DLCP *dlconn, void *buffer, size_t sendlen);
extern int     dl_sendpacket (DLCP *dlconn, void *headerbuf, size_t headerlen,
			      void *databuf, size_t datalen,
			      void *respbuf, int resplen);
extern int     dl_recvdata (DLCP *dlconn, void *buffer, size_t readlen, uint8_t blockflag);
extern int     dl_recvheader (DLCP *dlconn, void *buffer, size_t buflen, uint8_t blockflag);

/* timeutils.c */
extern int     dl_doy2md (int year, int jday, int *month, int *mday);
extern int     dl_md2doy (int year, int month, int mday, int *jday);
extern char   *dl_dltime2isotimestr (dltime_t dltime, char *isotimestr, int8_t subseconds);
extern char   *dl_dltime2mdtimestr (dltime_t dltime, char *mdtimestr, int8_t subseconds);
extern char   *dl_dltime2seedtimestr (dltime_t dltime, char *seedtimestr, int8_t subseconds);
extern dltime_t dl_time2dltime (int year, int day, int hour, int min, int sec, int usec);
extern dltime_t dl_seedtimestr2dltime (char *seedtimestr);
extern dltime_t dl_timestr2dltime (char *timestr);

/* genutils.c */
extern int     dl_splitstreamid (char *streamid, char *w, char *x, char *y, char *z, char *type);
extern int     dl_bigendianhost (void);
extern double  dl_dabs (double value);
extern int     dl_readline (int fd, char *buffer, int buflen);

/* logging.c */
extern int     dl_log (int level, int verb, ...);
extern int     dl_log_r (const DLCP *dlconn, int level, int verb, ...);
extern int     dl_log_rl (DLLog *log, int level, int verb, ...);
extern void    dl_loginit (int verbosity,
			   void (*log_print)(char*), const char *logprefix,
			   void (*diag_print)(char*), const char *errprefix);
extern void    dl_loginit_r (DLCP *dlconn, int verbosity,
			     void (*log_print)(char*), const char *logprefix,
			     void (*diag_print)(char*), const char *errprefix);
extern DLLog  *dl_loginit_rl (DLLog *log, int verbosity,
			      void (*log_print)(char*), const char *logprefix,
			      void (*diag_print)(char*), const char *errprefix);

/* statefile.c */
extern int  dl_recoverstate (DLCP *dlconn, const char *statefile);
extern int  dl_savestate (DLCP *dlconn, const char *statefile);

/* strutils.c */

/* For a linked list of strings, as filled by strparse() */
typedef struct DLstrlist_s {
  char               *element;
  struct DLstrlist_s *next;
} DLstrlist;

extern int  dl_strparse (const char *string, const char *delim, DLstrlist **list);
extern int  dl_strncpclean (char *dest, const char *source, int length);
extern int  dl_addtostring (char **string, char *add, char *delim, int maxlen);


#ifdef __cplusplus
}
#endif
 
#endif /* LIBDALI_H */
