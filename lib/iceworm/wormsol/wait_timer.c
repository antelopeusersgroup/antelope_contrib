
     /********************************************************************
      *                wait_timer.c   for   Solaris                      *
      *                                                                  *
      *  These are dummy functions in Solaris.                           *
      ********************************************************************/

#include <earthworm.h>


         /**********************************************************
          *                    init_wait_timer()                   *
          *                Create a new timer object               *
          **********************************************************/

int init_wait_timer( timer_t *timerHandle, DWORD *errorCode )
{
   return 0;
}


          /***********************************************************
           *                    start_wait_timer()                   *
           *  Start the timer.                                       *
           *  lPeriod is the repeat interval in milliseconds.        *
           ***********************************************************/

int start_wait_timer( timer_t timerHandle, LONG lPeriod, DWORD *errorCode )
{
   return 0;
}


         /**********************************************************
          *                      wait_timer()                      *
          *             Wait for the timer to complete             *
          **********************************************************/

int wait_timer( timer_t timerHandle, DWORD *errorCode )
{
   return 0;
}
