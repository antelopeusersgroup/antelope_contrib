
  /************************************************************
   *                          logit.c                         *
   *                                                          *
   *            Functions for maintaining log files.          *
   *                                                          *
   *        First, call logit_init.  Then, call logit.        *
   *                                                          *
   *       These functions are all MT-Safe.                   *
   ************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include "time_ew.h"

static FILE   *fp;
static char   date[7];
static char   date_prev[7];
static time_t now;
static char   logName[100];
static char   logpath[70];
static char   template[25];
static char   *buf;
static struct tm res;
static int    init  = 0;           /* 1 if logit_init has been called */
static int    disk  = 1;           /* 1 if output goes to disk file   */

void logit( char *, char *, ... );          /* Function prototype  */


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

/* Truncate everything beyond and
   including "." in the program name
   *********************************/
   strcpy( progName, prog );
   str = strchr( progName, '.' );
   if ( str != NULL )
      *str = '\0';

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
     fprintf( stderr, "%s logit_init: malloc error; exitting\n", progName );
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
      fprintf( stderr, "%s exitting.\n", progName );
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
   res.tm_year %= 100;
   sprintf( date, "%02d%02d%02d", res.tm_year, res.tm_mon+1,
            res.tm_mday );
   strcpy( logName, logpath );
   strcat( logName, template );
   strcat( logName, date );
   strcpy( date_prev, date );

/* Open log file
   *************/
   fp = fopen( logName, "a" );
   if ( fp == NULL )
   {
      fprintf( stderr, "%s: Error opening log file <%s%s>; exitting\n",
               progName, template, date );
      exit( 0 );
   }

/* Print startup message to log file
   *********************************/
   fprintf( fp, "\n-------------------------------------------------\n" );
   fprintf( fp, "%s: startup at UTC_%s_%02d:%02d:%02d",
            progName, date, res.tm_hour, res.tm_min, res.tm_sec );
   fprintf( fp, "\n-------------------------------------------------\n" );
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
      fl++;
   }
/*   stout = 1; */ /*force to write to stdout*/
/*   time_stamp = 1; */ /*force to timestamp*/

/* Get current system time
   ***********************/
   time( &now );
   gmtime_ew( &now, &res );

/* See if the date has changed.
   If so, create a new log file.
   *****************************/
   if ( disk )
   {
      res.tm_year %= 100;
      sprintf( date, "%02d%02d%02d", res.tm_year, res.tm_mon+1,
               res.tm_mday );

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
            fprintf( stderr, "Error opening log file <%s%s>; exitting\n",
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
   }

/* Write UTC time and argument list to buffer
   ******************************************/
   va_start( ap, format );
   if ( time_stamp )
   {
      sprintf( buf, "UTC_%02d:%02d:%02d ",
               res.tm_hour, res.tm_min, res.tm_sec );
      vsprintf( buf+strlen(buf), format, ap );
   }
   else
      vsprintf( buf, format, ap );
   va_end( ap );

/* Write buffer to standard output,
   standard error, and disk file.
   ********************************/
   if ( stout )
      printf( "%s", buf );

   if ( sterr )
      fprintf( stderr, "%s", buf );

   if ( disk ) {
      fprintf( fp, "%s", buf );
      fflush( fp );
   }

   return;
}

