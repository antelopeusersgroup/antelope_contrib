
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
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

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
