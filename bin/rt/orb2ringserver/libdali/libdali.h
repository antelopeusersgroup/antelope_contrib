/***********************************************************************//**
 * @file libdali.h
 *
 * Interface declarations for the DataLink Library (libdali).
 *
 * This file is part of the DataLink Library.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (C) 2020
 * @author Chad Trabant, IRIS Data Management Center
 ***************************************************************************/

#ifndef LIBDALI_H
#define LIBDALI_H 1

#ifdef __cplusplus
extern "C" {
#endif

#define LIBDALI_VERSION "1.8.0"      /**< libdali version */
#define LIBDALI_RELEASE "2020.108"   /**< libdali release date */

/** @defgroup connection Connection managment functions */
/** @defgroup network Connection network functions */
/** @defgroup time-related Time definitions and functions */
/** @defgroup logging Central Logging */
/** @defgroup utility-functions General Utility Functions */

/* C99 standard headers */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

/** @def PRIsize_t
    @brief A printf() macro for portably printing size_t values */
#define PRIsize_t "zu"

#if defined(WIN32) || defined(_WIN32) || defined(WIN64) || defined(_WIN64)
  #define DLP_WIN 1
#endif

/* Set platform specific features, Windows, Solaris, then everything else */
#if defined(DLP_WIN)
  #include <winsock2.h>
  #include <ws2tcpip.h>
  #include <windows.h>
  #include <process.h>
  #include <io.h>

  /* Re-define print conversion for size_t values */
  #undef PRIsize_t
  #if defined(WIN64) || defined(_WIN64)
    #define PRIsize_t "I64u"
  #else
    #define PRIsize_t "I32u"
  #endif

  /* For MSVC 2012 and earlier define standard int types, otherwise use inttypes.h */
  #if defined(_MSC_VER) && _MSC_VER <= 1700
    typedef signed char int8_t;
    typedef unsigned char uint8_t;
    typedef signed short int int16_t;
    typedef unsigned short int uint16_t;
    typedef signed int int32_t;
    typedef unsigned int uint32_t;
    typedef signed __int64 int64_t;
    typedef unsigned __int64 uint64_t;
  #else
    #include <inttypes.h>
  #endif

  #if defined(_MSC_VER)
    #if !defined(PRId64)
      #define PRId64 "I64d"
    #endif
    #if !defined(SCNd64)
      #define SCNd64 "I64d"
    #endif

    #define strdup _strdup
    #define read _read
    #define write _write
    #define open _open
    #define close _close
    #define snprintf _snprintf
    #define vsnprintf _vsnprintf
    #define strncasecmp _strnicmp
  #endif

#elif defined(__sun__) || defined(__sun)
  #include <unistd.h>
  #include <inttypes.h>
  #include <errno.h>
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
  #include <sys/time.h>
  #include <sys/utsname.h>
  #include <pwd.h>

#else
  #include <unistd.h>
  #include <inttypes.h>
  #include <errno.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netdb.h>
  #include <sys/time.h>
  #include <sys/utsname.h>
  #include <pwd.h>

#endif

/** @def SOCKET
    @brief Use int for SOCKET if platform includes have not defined it */
#ifndef SOCKET
  #define SOCKET int
#endif

#define LD_DEFAULT_HOST "localhost"  /**< Default host for libdali */
#define LD_DEFAULT_PORT "16000"      /**< Default port for libdali */

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

/** @addtogroup time-related
    @brief Definitions and functions for related to library time values

    Internally the library uses an integer value to represent time as
    the number of microseconds since the Unix/POSIX epoch (Jan 1 1970).

    @{ */

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

extern char *dl_dltime2isotimestr (dltime_t dltime, char *isotimestr, int8_t subseconds);
extern char *dl_dltime2mdtimestr (dltime_t dltime, char *mdtimestr, int8_t subseconds);
extern char *dl_dltime2seedtimestr (dltime_t dltime, char *seedtimestr, int8_t subseconds);
extern dltime_t dl_time2dltime (int year, int day, int hour, int min, int sec, int usec);
extern dltime_t dl_seedtimestr2dltime (char *seedtimestr);
extern dltime_t dl_timestr2dltime (char *timestr);
extern int dl_doy2md (int year, int jday, int *month, int *mday);
extern int dl_md2doy (int year, int month, int mday, int *jday);

/** @} */

/** @addtogroup logging
    @brief Central logging functions for the library and calling programs

    This central logging facility is used for all logging performed by
    the library.

    The logging can be configured to send messages to arbitrary
    functions, referred to as \c log_print() and \c diag_print().
    This allows output to be re-directed to other logging systems if
    needed.

    It is also possible to assign prefixes to log messages for
    identification, referred to as \c logprefix and \c errprefix.

    @anchor logging-levels
    Three message levels are recognized:
    - 0 : Normal log messages, printed using \c log_print() with \c logprefix
    - 1  : Diagnostic messages, printed using \c diag_print() with \c logprefix
    - 2+ : Error messages, printed using \c diag_print() with \c errprefix

    It is the task of the \c ms_log() and \c ms_log_l() functions to
    format a message using printf conventions and pass the formatted
    string to the appropriate printing function.

    @{ */

/** Logging parameters */
typedef struct DLLog_s
{
  void (*log_print)();          /**< Function pointer for log message printing */
  const char *logprefix;        /**< Log message prefix */
  void (*diag_print)();         /**< Function pointer for diagnostic/error message printing */
  const char *errprefix;        /**< Error message prefix */
  int  verbosity;               /**< Verbosity level */
} DLLog;
/** @} */

/** @addtogroup connection
    @brief Definitions and functions for DataLink connections

    @{ */

/** DataLink connection parameters */
typedef struct DLCP_s
{
  char        addr[100];        /**< The host:port of DataLink server */
  char        clientid[200];    /**< Client program ID as "progname:username:pid:arch", see dlp_genclientid() */
  int         keepalive;        /**< Interval to send keepalive/heartbeat (seconds) */
  int         iotimeout;        /**< Timeout for network I/O operations (seconds) */

  /* Connection parameters maintained internally */
  SOCKET      link;		/**< The network socket descriptor, maintained internally */
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
extern char   *dl_read_streamlist (DLCP *dlconn, const char *streamfile);
extern int     dl_recoverstate (DLCP *dlconn, const char *statefile);
extern int     dl_savestate (DLCP *dlconn, const char *statefile);
/** @} */


/** @addtogroup network
    @brief Functions for network DataLink connections

    @{ */
extern SOCKET  dl_connect (DLCP *dlconn);
extern void    dl_disconnect (DLCP *dlconn);
extern int     dl_senddata (DLCP *dlconn, void *buffer, size_t sendlen);
extern int     dl_sendpacket (DLCP *dlconn, void *headerbuf, size_t headerlen,
			      void *databuf, size_t datalen,
			      void *respbuf, int resplen);
extern int     dl_recvdata (DLCP *dlconn, void *buffer, size_t readlen, uint8_t blockflag);
extern int     dl_recvheader (DLCP *dlconn, void *buffer, size_t buflen, uint8_t blockflag);
/** @} */

/** @addtogroup logging
    @{ */
#if defined(__GNUC__) || defined(__clang__)
__attribute__((__format__ (__printf__, 3, 4)))
#endif
extern int     dl_log (int level, int verb, const char *format, ...);
#if defined(__GNUC__) || defined(__clang__)
__attribute__((__format__ (__printf__, 4, 5)))
#endif
extern int     dl_log_r (const DLCP *dlconn, int level, int verb, const char *format, ...);
#if defined(__GNUC__) || defined(__clang__)
__attribute__((__format__ (__printf__, 4, 5)))
#endif
extern int     dl_log_rl (DLLog *log, int level, int verb, const char *format, ...);
extern void    dl_loginit (int verbosity,
			   void (*log_print)(char*), const char *logprefix,
			   void (*diag_print)(char*), const char *errprefix);
extern void    dl_loginit_r (DLCP *dlconn, int verbosity,
			     void (*log_print)(char*), const char *logprefix,
			     void (*diag_print)(char*), const char *errprefix);
extern DLLog  *dl_loginit_rl (DLLog *log, int verbosity,
			      void (*log_print)(char*), const char *logprefix,
			      void (*diag_print)(char*), const char *errprefix);
/** @} */

/** @addtogroup utility-functions
    @brief General utility functions

    @{ */
extern const char *dlp_strerror (void);
extern int     dlp_openfile (const char *filename, char perm);
extern int64_t dlp_time (void);
extern void    dlp_usleep (unsigned long int useconds);
extern int     dlp_genclientid (char *progname, char *clientid, size_t maxsize);
extern int     dl_splitstreamid (char *streamid, char *w, char *x, char *y, char *z, char *type);
extern int     dl_bigendianhost (void);
extern double  dl_dabs (double value);
extern int     dl_readline (int fd, char *buffer, int buflen);

/** A linked list of strings, as filled by strparse() */
typedef struct DLstrlist_s {
  char               *element;
  struct DLstrlist_s *next;
} DLstrlist;

extern int  dl_strparse (const char *string, const char *delim, DLstrlist **list);
extern int  dl_strncpclean (char *dest, const char *source, int length);
extern int  dl_addtostring (char **string, char *add, char *delim, int maxlen);
/** @} */

#ifdef __cplusplus
}
#endif

#endif /* LIBDALI_H */
