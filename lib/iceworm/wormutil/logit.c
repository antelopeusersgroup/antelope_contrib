
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.10  2002/07/24 19:45:52  patton
 *     Slight Comment change.
 *
 *     Revision 1.9  2001/09/24 21:51:38  patton
 *     Added capability to change logging
 *     level by calling logit_init again.
 *     Part of the logit changeover.
 *     JMP 9/24/2001
 *
 *     Revision 1.8  2001/08/21 00:23:55  patton
 *     Modified the logfile name to match the new format:
 *     program_name_date.log.  Also removed the need for mid
 *     in logit_init, but left it in the call for backwards
 *     compatibility reasons.  JMP 8/20/2001.
 *
 *     Revision 1.7  2000/07/08 19:11:42  lombard
 *     Added value 2 to logflag argument of logit_init(), to turn off globally
 *     logging to standard error and standard output.
 *
 *     Revision 1.6  2000/06/21 16:32:29  lucky
 *     Added html_logit: same as logit but it is suitable for routines that produce html.
 *
 *     Revision 1.5  2000/06/02 21:57:51  davidk
 *     Added logit to prevent "Message too long" errors from being logged to
 *     stderr more than once.  This should allow notification, but prevent
 *     stderr from being flooded with messages and becoming unreadable.
 *
 *     Revision 1.4  2000/06/02 21:40:39  davidk
 *     added code to check for buffer overflow in logit().  Used vsnprintf()
 *     to fill the buffer without overflowing it.  Had to add a line to the
 *     WINNT portion of platform.h in order to get vsnprintf() to work for NT.
 *
 *     Revision 1.3  2000/06/01 00:36:10  dietz
 *     logit_init: moved CreateSpecificMutex earlier in the function so that it's
 *     always created regardless of the log/nolog switch value.
 *
 *     Revision 1.2  2000/03/13 23:22:09  davidk
 *     modified the LOGIT_MT ifdef so that the entire logit() function is executed as
 *     a single critical section (mutex protected).  This was done to fix a problem
 *     experienced on a Dual-Processor Ultra 60 running ew5-getlist, where one thread
 *     was overwriting the logit buffer of another thread before the original thread
 *     could write the buffer out to file.
 *
 *     Revision 1.1  2000/02/14 18:51:48  lucky
 *     Initial revision
 *
 *
 */


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
 * 8/17/2001 Changed log file names to the format: programname_date.log.
 * Also modified logitinit so that the mid is not used.  Mid was left
 * in the call however, for backwards compatibility purposes.
 * John Patton
 *
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
#include <time_ew.h>

#ifndef EARTHWORM_H
#include <earthworm.h>
#endif

#define		DATE_LEN		10   /** Length of the date field **/

static FILE   *fp = NULL;
static char   date[DATE_LEN];
static char   date_prev[DATE_LEN];
static time_t now;
static char   logName[256]; /* Increased 19990217 DK */
static char   logpath[128]; /* Increased 19990217 DK */
static char   template[64]; /* Increased 19990217 DK */
static char   extension[12]; /* JMP ADDED, for .log extension */
static char   *buf;
static struct tm res;
static int    init  = 0;           /* 1 if logit_init has been called */
static int    disk  = 1;           /* 1 if output goes to disk file   */
static int    soe   = 1;           /* 1 of output goes to stdout/stderr */
static int    pid;

/* DK 2000/06/02  Added logbuffersize variable to track 
   the size of the allocated logit buffer. 
*******************************************************/
static int    logbuffersize;

/* DK 2000/06/02  Added bErrorIssuedToStderr variable to 
   prevent the overloading of stderr with 
   "Message Too Long" error messages.
*******************************************************/
static int    bErrorIssuedToStderr=0;

#ifdef _LOGITMT
static mutex_t mutsem;
#endif


   /*************************************************************************
    *                             logit_init                                *
    *                                                                       *
    *      Call this function once before the other logit routines.         *
    *                                                                       *
    *   prog    : Name of the calling program's configuration file          *
    *             (argv[1])                                                 *
    *                                                                       *
    *   mid     : Module id number of the calling program.  NOTE: No        *
	*             longer used but left in The call for backwards            *
	*             compatiblity.                                             *
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
    *   logflag : Switch to turn disk-file and stderr/stdout logging        *
    *             on or off globally.                                       *
    *                                                                       *
	*   If logit_init is called again, it will set the output flags         *
	*   according to the new value in logflag.                              *
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

/* Set Disk logging to new level
   (assumes that logit_init has 
   allready been called once)
   JMP 9/5/2001
   *******************************/

/* Check init flag, if we have been
   already called, just reset the 
   logflag to the new value, and
   return. JMP 9-18-2001
   *********************************/
   if ( init )
   {
     /* Check the disk log/nolog switch
     ***********************************/
     if ( logflag == 0 )
       {
         disk = 0;
         if ( fp != NULL )
         {
           fclose( fp );
         }
         return;
       }

     /* check the SOE log/nolog switch */
     if (logflag == 2)
       soe = 0;
    return;      
   }
   init = 1;

   /* DK 2000/06/02  copy the desired logit buffer size 
      into the static "logbuffersize" variable.
   *******************************************************/
   /* copy bufSize into the "logbuffersize" static variable */
   logbuffersize=bufSize;

/* Allocate buffer from heap
   *************************/
   buf = (char *) malloc( (size_t)logbuffersize );
   if ( buf == (char *)NULL )
   {
     /* DK 2000/06/02 Added the size of the malloc, to the message */
     fprintf( stderr, "%s logit_init: malloc error for %d bytes. Exiting\n",
             progName, logbuffersize );
     exit( 0 );
   }

/* Create a mutex
   **************/
#ifdef _LOGITMT
   CreateSpecificMutex( &mutsem );
#endif

/* Check the disk log/nolog switch
   *******************************/
   if ( logflag == 0 )
   {
     disk = 0;
     return;
   }

   /* check the SOE log/nolog switch */
   if (logflag == 2)
     soe = 0;
   
/* Get path to log directory from environment variable EW_LOG
   **********************************************************/
   str = getenv( "EW_LOG" );

   if ( str == NULL )
   {
      fprintf( stderr, "Environment variable EW_LOG not defined; " );
      fprintf( stderr, "%s exiting.\n", progName );
      exit( -1 );
   }

/* Save the log-directory path and program name.
   *******************************************************/
   strcpy ( logpath, str );
   sprintf( template, "%s_", progName );

/* Build date stamp
   *****************/
   time( &now );
   gmtime_ew( &now, &res );

   /******************* Y2K *********************
    * Add 1900 to tm_year since it represents   *
    * number of years since 1900                *
    ******************* Y2K *********************/
   sprintf( date, "%04d%02d%02d", (res.tm_year + TM_YEAR_CORR),
            (res.tm_mon + 1), res.tm_mday );

/* Build extension
   *****************/
   sprintf( extension, ".log" );

/* Build logfile name
   *******************/
   strcpy( logName, logpath );
   strcat( logName, template );
   strcat( logName, date );
   strcat( logName, extension );
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
      fprintf( stderr, "%s: Error opening log file <%s%s%s>. Exiting\n",
               progName, template, date, extension );
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

   /* get the process id for use by logit("p",""); 
      davidk 11/13/1998 */
   pid=getpid();
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
   int    retcode;             /* DK 2000/06/02 used to check the
                                  return code from vsnprintf()    */
   int    basebufferlen;       /* DK 2000/06/02 used to store the
                                  length of the header string 
                                  prepended to the guts of the log
                                  message                         */

/* Check init flag
   ***************/
   if ( !init )
   {
      fprintf( stderr, "WARNING: Call logit_init before logit.\n" );
      return;
   }

#ifdef _LOGITMT
      RequestSpecificMutex( &mutsem );
#endif

/* Check flag argument
   *******************/
   fl = flag;
   while ( *fl != '\0' )
   {
      if ( *fl == 'o' && soe == 1 ) stout      = 1;
      if ( *fl == 'e' && soe == 1 ) sterr      = 1;
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


/* See if the date has changed.
   If so, create a new log file.
   *****************************/
      if ( strcmp( date, date_prev ) != 0 )
      {
         fprintf( fp,
                 "UTC date changed; log output continues in file <%s%s%s>\n",
                  template, date, extension );
         fclose( fp );

         /* Build new logfile name
         **************************/
         strcpy( logName, logpath );
         strcat( logName, template );
         strcat( logName, date );
         strcat( logName, extension );

         fp = fopen( logName, "a" );
         if ( fp == NULL )
         {
            fprintf( stderr, "Error opening log file <%s%s%s>. Exiting\n",
                     template, date, extension );
            exit( 0 );
         }
         fprintf( fp,
                 "UTC date changed; log output continues from file <%s%s%s>\n",
                  template, date_prev, extension );
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

   /* Record the strlen() of the buffer after the header has been created */
   basebufferlen=strlen(buf);

   /* Write argument list to buffer */

   /* DK 2000/06/02 replaced the following vsprintf() call with 
      a vsnprintf() call that puts a limit on the number of 
      bytes written to the buffer, thus preventing buffer overflow
   ***************************************************************/
   /* vsprintf( buf+strlen(buf), format, ap);  */

   retcode=vsnprintf(buf+basebufferlen, logbuffersize-basebufferlen,
                     format, ap);

   /* check the return code from vsnprintf().  It returns the number
      of characters written to the buffer unless there is an error,
      upon error, -1 is returned.  Note:  on most unix systems,
      if the buffer is not long enough for the message, vsnprintf()
      will write "buffer length" characters to the buffer, and 
      return "message length".  On NT, -1 is returned if the 
      "message length" exceeds the "buffer length".
   ***************************************************************/
   if(retcode > logbuffersize-basebufferlen)
   {
     /* The buffer wasn't long enough for the message and we know how long 
        the message was.                              
     *********************************************************************/

     if ( disk )
     {
       fprintf( fp, "logit(%s): ERROR!!! Attempting to log too large "
                "of a message!!!\n  Logit buffer is %d bytes, message "
                "is %d bytes!\n", template, logbuffersize,
                retcode + basebufferlen);      
       /* If fprintf fails, we won't know it */
        
     }
     else
     {
       if(!bErrorIssuedToStderr)
       {
         fprintf( stderr, "logit(%s): ERROR!!! Attempting to log too large "
                  "of a message!!!\n  Logit buffer is %d bytes, message "
                  "is %d bytes!\n", template, logbuffersize,
                  retcode + basebufferlen);      
         bErrorIssuedToStderr=1;
       }
     }
     /* DK 2000/06/02 this buffer was long, and it probably had a \n at
        the end that got truncated.  So add one in there.  Don't add it
        to the very end of the buffer, because that's where the null
        terminator goes.
     ***************************************************************/
     buf[logbuffersize-2]='\n';
   }
   else if(retcode == -1)
   {
     /* The buffer wasn't long enough for the message but we don't know how
        long the message was.
     *********************************************************************/

     if ( disk )
     {
       fprintf( fp, "logit(%s): ERROR!!! Attempting to log too large "
                "of a message!!!\n  Logit buffer is %d bytes, message "
                "is more!\n", template, logbuffersize);     
       /* If fprintf fails, we won't know it */
     }
     else
     {
       if(!bErrorIssuedToStderr)
       {
         fprintf( stderr, "logit(%s): ERROR!!! Attempting to log too large "
                  "of a message!!!\n  Logit buffer is %d bytes, message "
                  "is more!\n", template, logbuffersize); 
         bErrorIssuedToStderr=1;
       }
     }
     /* DK 2000/06/02 this buffer was long, and it probably had a \n at
        the end that got truncated.  So add one in there.  Don't add it
        to the very end of the buffer, because that's where the null
        terminator goes.
     ***************************************************************/
     buf[logbuffersize-2]='\n';
   }

   /* ensure that the buffer is null terminated */
   buf[logbuffersize-1]=0;

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
      fprintf( fp, "%s", buf );      /* If fprintf fails, we won't know it */
      fflush( fp );
   }

#ifdef _LOGITMT
      ReleaseSpecificMutex( &mutsem );
#endif

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



   /*****************************************************************
    *                         html_logit                            *
    *                                                               *
    *          Function to log a message to a disk file, and        *
    *       also html suitable output to stderr.                    *
    *                                                               *
    *  flag: A string controlling where output is written:          *
    *        If any character is 't', output is time stamped.       *
    *        If any character is 'p', output is ProcessID stamped.  *
    *                                                               *
    *  The rest of calling sequence is identical to printf.         *
    *****************************************************************/

void html_logit( char *flag, char *format, ... )
{
   auto va_list ap;
   static char   *fl;
   int    time_stamp = 0;      /* 1 if output is time-stamped     */
   int    pid_stamp  = 0;      /* 1 if output is pid-stamped      */
   int    retcode;             /* DK 2000/06/02 used to check the
                                  return code from vsnprintf()    */
   int    basebufferlen;       /* DK 2000/06/02 used to store the
                                  length of the header string 
                                  prepended to the guts of the log
                                  message                         */


	/* Put up the initial HTML */
	printf ("<CENTER><HR><PRE><STRONG><BR><BR>\n");


/* Check init flag
   ***************/
   if ( !init )
   {
      printf ("WARNING: Call logit_init before logit.\n");
      goto done;
   }

#ifdef _LOGITMT
      RequestSpecificMutex( &mutsem );
#endif

/* Check flag argument
   *******************/
   fl = flag;
   while ( *fl != '\0' )
   {
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


/* See if the date has changed.
   If so, create a new log file.
   *****************************/
      if ( strcmp( date, date_prev ) != 0 )
      {
         fprintf( fp,
                 "UTC date changed; log output continues in file <%s%s%s>\n",
                  template, date, extension );
         fclose( fp );

         /* Build new logfile name
         **************************/
         strcpy( logName, logpath );
         strcat( logName, template );
         strcat( logName, date );
         strcat( logName, extension );

         fp = fopen( logName, "a" );
         if ( fp == NULL )
         {
            fprintf( stderr, "Error opening log file <%s%s%s>. Exiting\n",
                     template, date, extension );
            exit( 0 );
         }
         fprintf( fp,
                 "UTC date changed; log output continues from file <%s%s%s>\n",
                  template, date_prev, extension );
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

   /* Record the strlen() of the buffer after the header has been created */
   basebufferlen=strlen(buf);

   /* Write argument list to buffer */

   /* DK 2000/06/02 replaced the following vsprintf() call with 
      a vsnprintf() call that puts a limit on the number of 
      bytes written to the buffer, thus preventing buffer overflow
   ***************************************************************/
   /* vsprintf( buf+strlen(buf), format, ap);  */

   retcode=vsnprintf(buf+basebufferlen, logbuffersize-basebufferlen,
                     format, ap);

   /* check the return code from vsnprintf().  It returns the number
      of characters written to the buffer unless there is an error,
      upon error, -1 is returned.  Note:  on most unix systems,
      if the buffer is not long enough for the message, vsnprintf()
      will write "buffer length" characters to the buffer, and 
      return "message length".  On NT, -1 is returned if the 
      "message length" exceeds the "buffer length".
   ***************************************************************/
   if(retcode > logbuffersize-basebufferlen)
   {
     /* The buffer wasn't long enough for the message and we know how long 
        the message was.                              
     *********************************************************************/

     if ( disk )
     {
       fprintf( fp, "logit(%s): ERROR!!! Attempting to log too large "
                "of a message!!!\n  Logit buffer is %d bytes, message "
                "is %d bytes!\n", template, logbuffersize,
                retcode + basebufferlen);      
       /* If fprintf fails, we won't know it */
        
     }
     else
     {
       if(!bErrorIssuedToStderr)
       {
         printf ("html_logit(%s): ERROR!!! Attempting to log too large "
                  "of a message!!!\n  Logit buffer is %d bytes, message "
                  "is %d bytes!\n", template, logbuffersize,
                  retcode + basebufferlen);      
         
         bErrorIssuedToStderr=1;
       }
     }
     /* DK 2000/06/02 this buffer was long, and it probably had a \n at
        the end that got truncated.  So add one in there.  Don't add it
        to the very end of the buffer, because that's where the null
        terminator goes.
     ***************************************************************/
     buf[logbuffersize-2]='\n';
   }
   else if(retcode == -1)
   {
     /* The buffer wasn't long enough for the message but we don't know how
        long the message was.
     *********************************************************************/

     if ( disk )
     {
       fprintf( fp, "logit(%s): ERROR!!! Attempting to log too large "
                "of a message!!!\n  Logit buffer is %d bytes, message "
                "is more!\n", template, logbuffersize);     
       /* If fprintf fails, we won't know it */
     }
     else
     {
       if(!bErrorIssuedToStderr)
       {
         printf ("html_logit(%s): ERROR!!! Attempting to log too large "
                  "of a message!!!\n  Logit buffer is %d bytes, message "
                  "is more!\n", template, logbuffersize); 
         bErrorIssuedToStderr=1;
       }
     }
     /* DK 2000/06/02 this buffer was long, and it probably had a \n at
        the end that got truncated.  So add one in there.  Don't add it
        to the very end of the buffer, because that's where the null
        terminator goes.
     ***************************************************************/
     buf[logbuffersize-2]='\n';
   }

   /* ensure that the buffer is null terminated */
   buf[logbuffersize-1]=0;

   va_end( ap );

/* Write buffer to html
   *******************************/
   printf( "ERROR: %s", buf );


/* Write buffer to disk file
   *************************/
   if ( disk )
   {
      fprintf( fp, "%s", buf );      /* If fprintf fails, we won't know it */
      fflush( fp );
   }

#ifdef _LOGITMT
      ReleaseSpecificMutex( &mutsem );
#endif

done:
   printf ("</CENTER><HR></PRE></STRONG>\n");
   return;
}


