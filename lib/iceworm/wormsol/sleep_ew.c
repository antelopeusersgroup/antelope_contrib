/********************************************************************
 *                 sleep_ew.c    for   SOLARIS                      *
 *                                                                  *
 *  Any program that uses this function must contain:		    *
 *   #include <earthworm.h> 					    *
 *  and must link with the posix4 library:			    *
 *   cc [ flag ... ] file ... -lposix4 [ library ... ]		    *
 ********************************************************************/

#include <time.h>

/********************* sleep_ew for SOLARIS ******************
                    sleep alarmtime milliseconds
 **************************************************************/
void sleep_ew( unsigned alarmtime )
{
   struct timespec sleeptime;
   struct timespec timeleft;

   sleeptime.tv_sec = (time_t) alarmtime / 1000;
   sleeptime.tv_nsec = (long) (1000000 * (alarmtime % 1000));

   while( nanosleep(&sleeptime, &timeleft) != 0 )
   {
        /*printf( "sleep_ew: interrupted by signal;" );*//*DEBUG*/ 
        /*printf( " %d msec left\n",
	        (int) (timeleft.tv_sec*1000 + timeleft.tv_nsec/1000000) );*//*DEBUG*/
	sleeptime = timeleft;
   }

   return;
}
