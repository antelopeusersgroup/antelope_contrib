
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.4  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2000/07/13 16:39:45  lombard
 *     Added checks for error return from nanosleep().
 *
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

/********************************************************************
 *                 sleep_ew.c    for   SOLARIS                      *
 *                                                                  *
 *  Any program that uses this function must contain:		    *
 *   #include <earthworm.h> 					    *
 *  and must link with the posix4 library:			    *
 *   cc [ flag ... ] file ... -lposix4 [ library ... ]		    *
 ********************************************************************/

#include <time.h>
#include <errno.h>
#include <stdio.h>

/********************* sleep_ew for SOLARIS ******************
                    sleep alarmtime milliseconds
 **************************************************************/
void sleep_ew( unsigned alarmtime )
{
   struct timespec sleeptime;
   struct timespec timeleft;
   int err;

   sleeptime.tv_sec = (time_t) alarmtime / 1000;
   sleeptime.tv_nsec = (long) (1000000 * (alarmtime % 1000));

   while( nanosleep(&sleeptime, &timeleft) != 0 )
   {
      if ( (err = errno) == EINTR)
      {
        /*printf( "sleep_ew: interrupted by signal;" );*//*DEBUG*/ 
        /*printf( " %d msec left\n",
	        (int) (timeleft.tv_sec*1000 + timeleft.tv_nsec/1000000) );*//*DEBUG*/
	sleeptime = timeleft;
      }
      else
      {
         fprintf(stderr, "sleep_ew: error from nanosleep: %s\n",
                 strerror(err));
         fprintf(stderr,"\ttime requested = %.3f sec, time left = %.3f sec\n",
                 sleeptime.tv_sec + sleeptime.tv_nsec*1.e-9,
                 timeleft.tv_sec + timeleft.tv_nsec*1.e-9);
       }
   }

   return;
}
