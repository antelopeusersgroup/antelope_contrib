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


#define		DATE_LEN 	10
#define		TIME_LEN 	12

typedef struct
{
char	author[AUTHOR_FIELD_SIZE]; /* from earthworm.h */
long 	eventId;
char 	sta[7];
char	chan[9];
char	net[9];
char	startYYYYMMDD[DATE_LEN];	/* as parsed from trigger file */
char	startHHMMSS[TIME_LEN];		/* as parsed from trigger file */
char    pad[1];                 	/* for data alignment */
double 	starttime;
int		duration;
}
SNIPPET;

/* Function prototypes
 *********************/
int parseSnippet( char* , SNIPPET* , char** );
