
     /*************************************************************
      *                          logit.c                          *
      *                                                           *
      *            Functions for maintaining log files.           *
      *                                                           *
      *        First, call logit_init.  Then, call logit.         *
      *                                                           *
      *  If the environmental variable _LOGITMT is defined,       *
      *  the resulting object file will be MT-safe.               *
      *                                                           *
      *  If _LOGITMT is not defined, the program must link to     *
      *  time_ew.o.                                               *
      *                                                           *
      *  If _LOGITMT is defined, the program must link to         *
      *  time_ew.o and sema_ew.o.                                 *
      *                                                           *
      *  In Solaris, the program must also link to the posix      *
      *  library (-lposix4)                                       *
      *                                                           *
      *************************************************************/
/*
 * 7/7/99 Removed 2 lines "res.tm_year %= 100" which were causing
 *   years 2000+ to be printed as 1900+.  Lynn Dietz
 *
 * Changed lines 69-73 to allow logit to work under the Solaris Workshop
 * debugger: Pete Lombard, 8/5/98
 *
 * Mon Oct 26 14:22:09 MST 1998 lucky
 *  Created a new routine get_prog_name which extracts the
 *  name of the program by truncating the path to it as well as
 *  any extensions
 *
 * Mon Oct 26 15:15:12 MST 1998 lucky
 *  Y2K compliance:
 *    - use 4 digits for years in file names
 *    - use 4 digits for years in status messages
 *
 * Wed Oct 28 16:19:45 MST 1998 lucky
 *  Added the YYYYMMDD date filed to the log message that
 *  are being produced. New format is now:
 *
 *      YYYYMMDD_UTC_HH:MM:SS
 *
 * 19990217 DavidK 
 *  Increased the lengths of logName, logpath, and Template to allow for
 *   longer logfile names, based on config file names.  Longest progname
 *   accepted should now be 58, with a max path length of 127
 *  Removed or atleast placed in and "#if EW_DEBUG" section code to
 *   print to stderr a message that says logit_init() opened the logfile.
 *  Changed the 2-digit year to a 4-digit year in the date printout in
 *  the logfile.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include "time_ew.h"

#ifndef EARTHWORM_H
#include "earthworm.h"
#endif

#define		DATE_LEN		10   /** Length of the date field **/

static FILE   *fp;
static char   date[DATE_LEN];
static char   date_prev[DATE_LEN];
static time_t now;
static char   logName[256]; /* Increased 19990217 DK */
static char   logpath[128]; /* Increased 19990217 DK */
static char   template[64]; /* Increased 19990217 DK */
static char   *buf;
static struct tm res;
static int    init  = 0;           /* 1 if logit_init has been called */
static int    disk  = 1;           /* 1 if output goes to disk file   */
static int    pid;

#ifdef _LOGITMT
static mutex_t mutsem;
#endif


   /*************************************************************************
    *                             logit_init                                *
    *                                                                       *
    *      Call this function once before the other logit routines.         *
    *                                                                       *
    *   prog    : Name of the calling program (argv[0] to main()).          *
    *             On OS2, prog has the suffix ".exe"                        *
    *                                                                       *
    *   mid     : Module id number of the calling program.                  *
    *                                                                       *
    *   bufSize : Size of buffer to be allocated by logit_init.             *
    *             This buffer must be large enough to accomodate the        *
    *             largest log message to be written.                        *
    *             11/13/1998, DavidK, added the ability to write the        *
    *             process id, to the logit message, which would extend      *
    *             a message by up to 6 chars.  So messages that were        *
    *             borderline length before, will now be too long, if the    *
    *             pid is included.                                          *
    *                                                                       *
    *   logflag : Switch to turn disk-file logging on or off globally       *
    *                                                                       *
    *************************************************************************/

void logit_init( char *prog, short mid, int bufSize, int logflag )
{
   char *str;
   char progName[50];

/* Set time zone using the TZ environmental variable.
   This is not required under Solaris.
   In OS2 v2 or v3.0, use _tzset().
   In OS2 v3.0, use tzset().
   *************************************************/
#if defined(_OS2) || defined(_WINNT)
   if ( getenv( "TZ" ) != NULL ) _tzset();
#endif

/* Truncate in front of and including "/", everything beyond and
   including "." in the program name
   *********************************/
   if (get_prog_name (prog, progName) != EW_SUCCESS)
   {
      fprintf( stderr, "Call to get_prog_name failed.\n");
      return;
   }

/* Check init flag
   ***************/
   if ( init )
   {
      fprintf( stderr, "WARNING: logit_init called more than once.\n" );
      return;
   }
   init = 1;

/* Allocate buffer from heap
   *************************/
   buf = (char *) malloc( (size_t)bufSize );
   if ( buf == (char *)NULL )
   {
     fprintf( stderr, "%s logit_init: malloc error. Exiting\n", progName );
     exit( 0 );
   }

/* Check the log/nolog switch
   **************************/
   if ( logflag == 0 )
   {
       disk = 0;
       return;
   }

/* Get path to log directory from environment variable EW_LOG
   **********************************************************/
   str = getenv( "EW_LOG" );

   if ( str == NULL )
   {
      fprintf( stderr, "Environment variable EW_LOG not defined; " );
      fprintf( stderr, "%s exiting.\n", progName );
      exit( -1 );
   }

/* Save the log-directory path, program name and module id.
   *******************************************************/
   strcpy ( logpath, str );
   sprintf( template, "%s%hd.log_", progName, mid );

/* Build log file name by appending time
   *************************************/
   time( &now );
   gmtime_ew( &now, &res );

   /******************* Y2K *********************
    * Add 1900 to tm_year since it represents   *
    * number of years since 1900                *
    ******************* Y2K *********************/
   sprintf( date, "%04d%02d%02d", (res.tm_year + TM_YEAR_CORR),
            (res.tm_mon + 1), res.tm_mday );
   strcpy( logName, logpath );
   strcat( logName, template );
   strcat( logName, date );
   strcpy( date_prev, date );


#if EW_DEBUG
 fprintf (stderr, "Opening %s\n", logName); 
/* this was commented by DK on 990208, because
   it was writing data to the webserver on stderr,
   which gets processed first, and causes things to
   blow up, because the webserver is trying to parse
   a header from it.  I think this should be a debug
   statement any way. */
#endif /* EW_DEBUG */

/* Open log file
   *************/
   fp = fopen( logName, "a" );
   if ( fp == NULL )
   {
      fprintf( stderr, "%s: Error opening log file <%s%s>. Exiting\n",
               progName, template, date );
      exit( 0 );
   }

/* Print startup message to log file
   *********************************/
   fprintf( fp, "\n-------------------------------------------------------\n" );
   fprintf( fp, "%s: startup at UTC_%s_%02d:%02d:%02d\n",
            progName, date, res.tm_hour, res.tm_min, res.tm_sec );

#ifdef _LOGITMT
   fprintf( fp, "This program is using the MT-Safe version of logit.\n" );
#else
   fprintf( fp, "This program is using the non-MT-Safe version of logit.\n" );
#endif

   fprintf( fp, "-------------------------------------------------------\n" );
   fflush ( fp );

/* Log a warning message
   *********************/
#if defined(_OS2) || defined(_WINNT)
   if ( getenv( "TZ" ) == NULL )
   {
      logit( "e", "WARNING: The TZ environmental variable is not set.\n" );
      logit( "e", "         UTC times in log messages may be bogus.\n" );
   }
#endif

/* Create a mutex
   **************/
#ifdef _LOGITMT
   CreateSpecificMutex( &mutsem );
#endif

   /* get the process id for use by logit("p",""); 
      davidk 11/13/1998 */
   pid=getpid();
   /*********************************************/
   return;
}


   /*****************************************************************
    *                            logit                              *
    *                                                               *
    *          Function to log a message to a disk file.            *
    *                                                               *
    *  flag: A string controlling where output is written:          *
    *        If any character is 'e', output is written to stderr.  *
    *        If any character is 'o', output is written to stdout.  *
    *        If any character is 't', output is time stamped.       *
    *        If any character is 'p', output is ProcessID stamped.  *
    *                                                               *
    *  The rest of calling sequence is identical to printf.         *
    *****************************************************************/

void logit( char *flag, char *format, ... )
{
   auto va_list ap;
   static char   *fl;
   int    stout      = 0;      /* 1 if output is also to stdout   */
   int    sterr      = 0;      /* 1 if output is also to stderr   */
   int    time_stamp = 0;      /* 1 if output is time-stamped     */
   int    pid_stamp  = 0;      /* 1 if output is pid-stamped      */

/* Check init flag
   ***************/
   if ( !init )
   {
      fprintf( stderr, "WARNING: Call logit_init before logit.\n" );
      return;
   }

/* Check flag argument
   *******************/
   fl = flag;
   while ( *fl != '\0' )
   {
      if ( *fl == 'o' ) stout      = 1;
      if ( *fl == 'e' ) sterr      = 1;
      if ( *fl == 't' ) time_stamp = 1;
      if ( *fl == 'd' ) pid_stamp  = 1;
      fl++;
   }

/* Get current system time
   ***********************/
   time( &now );
   gmtime_ew( &now, &res );

   /******************* Y2K *********************
    * Add 1900 to tm_year since it represents   *
    * number of years since 1900                *
    ******************* Y2K *********************/
   sprintf (date, "%4d%02d%02d", (res.tm_year + TM_YEAR_CORR),
	    (res.tm_mon + 1), res.tm_mday);


/* If we are writing to a disk file...
   ***********************************/
   if ( disk )
   {

#ifdef _LOGITMT
      RequestSpecificMutex( &mutsem );
#endif

/* See if the date has changed.
   If so, create a new log file.
   *****************************/
      if ( strcmp( date, date_prev ) != 0 )
      {
         fprintf( fp,
                 "UTC date changed; log output continues in file <%s%s>\n",
                  template, date );
         fclose( fp );
         strcpy( logName, logpath );
         strcat( logName, template );
         strcat( logName, date );
         fp = fopen( logName, "a" );
         if ( fp == NULL )
         {
            fprintf( stderr, "Error opening log file <%s%s>. Exiting\n",
                     template, date );
            exit( 0 );
         }
         fprintf( fp,
                 "UTC date changed; log output continues from file <%s%s>\n",
                  template, date_prev );
         strcpy( date_prev, date );

/* Send a warning message to the new log file
   ******************************************/
#if defined(_OS2) || defined(_WINNT)
         if ( getenv( "TZ" ) == NULL )
         {
            fprintf( fp, "WARNING: The TZ environmental variable is not set.\n" );
            fprintf( fp, "         UTC times in log messages may be bogus.\n" );
         }
#endif
      }

#ifdef _LOGITMT
      ReleaseSpecificMutex( &mutsem );
#endif

   }

/* Write UTC time and argument list to buffer
 *
 * Wed Oct 28 16:19:45 MST 1998 lucky
 *   Added date to the log message
   ******************************************/
   va_start( ap, format );
   buf[0] = 0;  /* NULL terminate the empty buf */

   /* DavidK 11/13/1998, changed the format of the conditionals
      for writing the variable argument stuff to buffer.
      Changed from if..else for time stamp to if(time_stamp),
      if(pid_stamp), then after all headers have been written,
      concatenate the variable argument list to end.
   */
   /* Write UTC time stamp to buffer if desired */
   if ( time_stamp )
   {
      sprintf( buf+strlen(buf), "%s_UTC_%02d:%02d:%02d ",
               date, res.tm_hour, res.tm_min, res.tm_sec );
   }

   /* Write Process ID stamp to buffer if desired */
   if ( pid_stamp )
   {
      sprintf( buf+strlen(buf), "(%d) ", pid);
   }

   /* Write argument list to buffer*/
   vsprintf( buf+strlen(buf), format, ap);

   va_end( ap );

/* Write buffer to standard output and standard error
   **************************************************/
   if ( stout )
      printf( "%s", buf );

   if ( sterr )
      fprintf( stderr, "%s", buf );

/* Write buffer to disk file
   *************************/
   if ( disk )
   {
#ifdef _LOGITMT
      RequestSpecificMutex( &mutsem );
#endif
      fprintf( fp, "%s", buf );      /* If fprintf fails, we won't know it */
      fflush( fp );
#ifdef _LOGITMT
      ReleaseSpecificMutex( &mutsem );
#endif
   }

   return;
}


/*************************************************************************
 *                         get_prog_name                                 *
 *                                                                       *
 *   extracts program name from the full path by ignoring everything     *
 *   before the first / and after the . (extension)                      *
 *                                                                       *
 *   full_name Name of the calling program (argv[0] to main()).          *
 *             On OS2, prog has the suffix ".exe"                        *
 *                                                                       *
 *   prog_name Truncated program name. Array must be allocated before    *
 *             calling this routine                                      *
 *                                                                       *
 *************************************************************************/
int get_prog_name (char *full_name, char *prog_name)
{

	char	*str;

	if ((full_name == NULL) || (prog_name == NULL))
	{
		fprintf (stderr, "Invalid arguments passed in.\n");
		return EW_FAILURE;
	}

	/* Truncate in front of and including "/", everything beyond and
	   including "." in the program name
	 *********************************/
	if ((str = strrchr (full_name, '/')) != NULL)
		strcpy (prog_name, str + 1);
	else
		strcpy (prog_name, full_name);

	str = strchr (prog_name, '.');
	if (str != NULL)
		*str = '\0';

	return EW_SUCCESS;

}
