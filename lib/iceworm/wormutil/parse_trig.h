
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.2  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.5  2001/07/01 22:04:59  davidk
 *     Added prototype for t_atodbl() which converts an ascii time string to a double.
 *     Function is already defined and used from parse_trig.c.
 *
 *     Revision 1.4  2001/04/12 03:13:13  lombard
 *     Added includes to define macros used here.
 *     Added multiple inclusion exclusion.
 *
 *     Revision 1.3  2001/03/21 22:49:41  cjbryan
 *     ,
 *
 *     Revision 1.2  2001/03/21 02:16:19  alex
 *     Alex 3/20/1: addes .subnet as per CVO request
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */

#ifndef PARSE_TRIG_H
#define PARSE_TRIG_H

/*
 * parse_trig.h : Include file for anyone using parse_trig.c; 
 *                defines the SNIPPET structure. This is filled
 *		          by the parse_snippet.c routines, one line at a time.
 * 
 * Mon Nov  2 10:58:25 MST 1998 lucky
 *   Y2K compliance: 
 *    SNIPPET Struct modified - replaced startYYMMDD 
 *    with  YYYYMMDD. Added DATE_LEN and TIME_LEN defines for  
 *    better future flexibility.
 *
 */

#include <earthworm_defs.h>
#include <trace_buf.h>

#define		DATE_LEN 	10
#define		TIME_LEN 	12
typedef struct
{
char	author[AUTHOR_FIELD_SIZE]; /* from earthworm.h */
char	subnet[MAX_SUBNET_LEN];    /* Alex 2/20/1 as per CVO request: The subnet which caused all this. Optional  */
char 	eventId[EVENTID_SIZE];     /* from earthworm.h */
char 	sta[TRACE_STA_LEN];
char	chan[TRACE_CHAN_LEN];
char	net[TRACE_NET_LEN];
char	startYYYYMMDD[DATE_LEN];	/* as parsed from trigger file */
char	startHHMMSS[TIME_LEN];		/* as parsed from trigger file */
char    pad[1];                 	/* for data alignment */
double 	starttime;
int		duration;
} SNIPPET;

/* Function prototypes
 *********************/
int parseSnippet( char* , SNIPPET* , char** );

int t_atodbl(char* YYYYMMDD, char* HHMMSS, double* starttime) ;

#endif
