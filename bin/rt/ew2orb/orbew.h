/*
 * orbew.h
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 *
 */

#ifndef ORBEW_H
#define ORBEW_H

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include "stock.h"
#include "swapbytes.h"
#include "orb.h"
#include "Pkt.h"
#include "bns.h"
#include "brttutil.h"
#include "brttpkt.h"
#include "pf.h"
#include "trace_buf.h"

#define STX 2
#define ETX 3
#define ESC 27

#define INST_WILDCARD 0
#define MOD_WILDCARD 0
#define TYPE_WILDCARD 0
#define DEFAULT_SEND_HEARTBEAT_SEC 120
#define DEFAULT_SEND_HEARTBEAT_STRING "alive"
#define DEFAULT_EXPECT_HEARTBEAT_SEC 300
#define DEFAULT_EXPECT_HEARTBEAT_STRING ".*alive"
#define DEFAULT_LARGE_TRACEBUF_HANDLING "send"
#define DEFAULT_TIMESORT_QUEUE_MAXPKTS 0
#define DEFAULT_LOGLEVEL "quiet"
#define DEFAULT_EARTHWORM_PFNAME "earthworm"
#define DEFAULT_INST "INST_UNKNOWN"
#define DEFAULT_MOD "MOD_UNKNOWN"
#define DEFAULT_EWEXPORT_TYPE "TYPE_TRACEBUF"
#define DEFAULT_TYPE_HEARTBEAT 3
#define DEFAULT_TYPE_TRACEBUF2 19
#define DEFAULT_TYPE_TRACEBUF 20
#define DEFAULT_TYPE_TRACE_COMP_UA 26
#define DEFAULT_SERVER_PORT 16010
#define DEFAULT_SELECT ""
#define DEFAULT_REJECT ""
#define DEFAULT_STARTTIME ""
#define NULL_STARTTIME -9999999999.999

#define PFWATCH_SLEEPTIME_SEC 1
#define CONNECT_FAILURE_SLEEPTIME_SEC 1
#define NCOMPLAIN_MAX 5
#define BNS_BUFFER_SIZE 128 * 1024
#define DEFAULT_BNS_TIMEOUT 60000
#define EWLOGO_SIZE 9

#define STREQ(a, b) (strcmp((a), (b)) == 0)

typedef struct Earthworm_Info {
	pthread_mutex_t	ew_mutex;
	char	pfname[FILENAME_MAX];
	Pf	*pf;
	Arr	*inst_names;
	Arr	*inst_ids;
	Arr	*mod_names;
	Arr	*mod_ids;
	Arr	*type_names;
	Arr	*type_ids;
} Earthworm_Info;

typedef struct Flagdef {
	int	verbose:4;
	int	VeryVerbose:4;
	int	have_banner:4;
	int		   :4;
} Flagdef;

enum Loglevel { QUIET, VERBOSE, VERYVERBOSE };

extern char	*Default_TYPE_HEARTBEAT;
extern char	*Default_TYPE_TRACEBUF;
extern char	*Default_TYPE_TRACEBUF2;
extern char	*Default_TYPE_TRACE_COMP_UA;
extern char	Program_loglevel[STRSZ];

extern Earthworm_Info Ewinfo;
extern Flagdef Flags;

extern void refresh_earthworm_info( void );
extern void set_program_loglevel( Pf *pf );
extern enum Loglevel translate_loglevel( char *loglevel );
extern void pfreplace( Pf *sourcepf, Pf *destpf, char *sourcekey,
		       char *destkey, char *type );
extern void ewlogo_tostrings( int inst, int mod, int type, char *inststr,
		              char *modstr, char *typestr );
extern void ewlogo_tologo( char *inststr, char *modstr, char *typestr,
		              int *inst, int *mod, int *type );
extern Tbl *healthy_morphlist( Tbl *morphlist );

#endif /* ORBEW_H */
