/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.1  2003/06/01 08:25:39  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.3  2002/09/10 17:20:03  dhanych
 *     Stable scaffold
 *
 *     Revision 1.2  2001/07/14 06:44:45  davidk
 *     added definition for EW_WARNING.
 *
 *     Revision 1.1  2001/04/06 21:03:30  davidk
 *     Initial revision
 *
 *
 ************************************************************/

#ifndef EARTHWORM_DEFS_H
# define EARTHWORM_DEFS_H

/* Define unique port numbers for interprocess communications
 ************************************************************/
#define WAVE_SERVER_PORT  16022    /* for requesting/receiving trace data */


/* Define error words (2-bytes; >9999) global to earthworm modules
   Values 0-9999 are available for private error definitions within modules
 **************************************************************************/
#define ERR_LAPPED      10000    /* data loss; overwritten in transport ring */
#define ERR_SEQGAP      10001    /* data loss; sequence gap in msgs received */
#define ERR_OVERFLOW    10002    /* data transfer failed; allocated space at */
                                 /* target address exceeded                  */
#define ERR_UNTRACKED   10003    /* transport.h tracking limit exceeded      */


/* Define global error codes 
 ****************************/
#define     EW_SUCCESS         1
#define     EW_FAILURE         0
#define     EW_WARNING         2

#define     TRUE               1
#define     FALSE              0

/* Define other global values
 ****************************/
#define     TM_YEAR_CORR    1900 /** Y2K correction for tm_year field **/


/* Set limits on certain things
 ******************************/

/*
 * Limits on the number of phases: we want to limit the number
 * of phases used in hypoinvese solutions to 250. 
 * Our DB can handle much larger events, so we have the second 
 * set of constants to accomodate the database
 */
#define MAX_PHS_PER_EQ    250     /* set the maximum #phases to include */
                                      /* when processing an earthquake      */
                                      /* with hypoinverse                   */
#define MAX_BYTES_PER_EQ  (450+225*(MAX_PHS_PER_EQ))
                                 /* generous maximum size of a Hypoinverse   */
                                 /* archive message based on Fred Klein's    */
                                 /* "shadow.doc" file dated March 12, 1997   */
#define MAX_TRIG_BYTES 		MAX_BYTES_PER_EQ


#define DB_MAX_PHS_PER_EQ    1000    /* max # phases for any event in the DB */
#define DB_MAX_BYTES_PER_EQ  (450+225*(DB_MAX_PHS_PER_EQ))
#define DB_MAX_TRIG_BYTES 	DB_MAX_BYTES_PER_EQ


#define AUTHOR_FIELD_SIZE 50    /* Used in trigger message. Alex 6/16/98 */
#define EVENTID_SIZE 50         /* Used in trigger message. Alex 1/19/00 */

#define MAX_SUBNET_LEN  10	/* Used in carlsubtrig. Carol 3/21/01 */

#define	MAX_DIR_LEN 150		/* Max path length for directories, etc */

#define MAX_EMAIL_MSG_SIZE   32000
#define MAX_MSG_PREFIX_SIZE  256

/* Hardcode lengths of module, installation, type, and ring strings */
#define MAX_MOD_STR  		32 	/* max length of module names */
#define MAX_INST_STR  		32	/* max length of installation ids */
#define MAX_RING_STR  		32 	/* max length of ring names */
#define MAX_TYPE_STR  		32 	/* max length of message types */



/* GLOBAL MAGNITUDE DEFINITIONS */

#define MAG_NULL       -1

/* Mag type integers and corresponding character string names
*************************************************************/
/* 
 * This is the official table of magnitude numbers and names. The
 * mag message contains both the number and the name. The name is 
 * included for human readability purposes. The number is what counts. 
 * For safety purposes, 0 is not a legal name.
 *
 * IMPORTANT: Since the database uses the number to determine  
 * magnitude types, it is essential to keep constant the order of
 * the magnitudes in the MagNames array, as well as the mappings 
 * between strings and numbers. Any new mappings should be appended
 * to the end. Whenever a new mag type is added to the array, the
 * corresponding update should be made to the DB population script
 * SCHEMA_DIR/src/core_schema/sql_scripts/ewdb_population_scripts.sql. 
 *
 */
 
#define MAGTYPE_UNDEFINED        0
#define MAGTYPE_LOCAL_PEAK2PEAK  1
#define MAGTYPE_MOMENT           2
#define MAGTYPE_BODYWAVE         3
#define MAGTYPE_SURFACEWAVE      4
#define MAGTYPE_SCALAR_MOMENT    5
#define MAGTYPE_DURATION         6
#define MAGTYPE_LOCAL_ZERO2PEAK  7
#define MAGTYPE_MBLG             8

#define MAGTYPE_COUNT            9  /* one higher that last item in prev list */

static char* MagNames[MAGTYPE_COUNT] = 
{
   "??"    /* 0 */
 , "ML"    /* 1 -- "Utah" method for computing Mls */
 , "Mw"    /* 2 */
 , "Mb"    /* 3 */
 , "Ms"    /* 4 */
 , "Mwp"   /* 5 -- from IDPP */
 , "Md"    /* 6 */
 , "ML"    /* 7 -- "Richter" method for computing Mls  */
 , "mblg"  /* 8 */
};

/* For backwards compatibility */

#define N_MAG_NAMES  MAGTYPE_COUNT



/* GLOBAL AMPLITUDE DEFINITIONS */

typedef short AMPLITUDE_TYPE;

#define AMPTYPE_NONE  0 /* invalid or unknown ('?') */
#define AMPTYPE_ML    1
#define AMPTYPE_MB    2
#define AMPTYPE_MBLG  3
#define AMPTYPE_MWP   4 /* from idpp */
#define AMPTYPE_MS    5

#define AMPTYPE_COUNT (AMPLITUDE_TYPE)6 /* count of valid types */


static const char * AMPLITUDE_NAMES[AMPTYPE_COUNT] =
{
    "?"
  , "ML"
  , "mb"
  , "mblg"
  , "Mwp"
  , "MS"
};

#endif /* EARTHWORM_DEFS_H */

