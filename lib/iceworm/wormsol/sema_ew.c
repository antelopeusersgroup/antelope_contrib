
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2002/02/18 18:38:11  davidk
 *     initialized the dummy pointer to NULL on all of the various CreateMutex type calls
 *     that require a dummy pointer param.  The dummy pointer was previously uninitialized.
 *     It doesn't seem to have any affect since all documentation says the dummy pointer
 *     is ignored, but purify complained, so I fixed it.
 *
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

/*
 * sema_ew.c  Solaris version
 *
 * This source file contains system-dependent functions for
 * handling event semaphores and mutexes
 *
 */

#include <stdio.h>
#include <synch.h>

static sema_t   semaphore;  /* Event Semaphore                        */
static mutex_t  mutex;      /* Mutual Exclusion semaphore lock        */


/************************ CreateSemaphore_ew ************************
    Create a semaphore which is posted when some event is complete.
*********************************************************************/
void CreateSemaphore_ew( void )
{
   static unsigned int count = 0;
   void *dummy = NULL;
   int   rc;

   rc = sema_init( &semaphore, count, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "CreateSemaphore_ew: Error from sema_init: %d; Exiting.\n", rc );
      exit( -1 );
   }
   return;
}

/**************************** PostSemaphore ****************************
             Let another thread know that a message is ready.
************************************************************************/
void PostSemaphore( void )
{
   int rc;

   rc = sema_post( &semaphore );
   if ( rc != 0 )
      fprintf( stderr,
              "PostSemaphore: Error from sema_post: %d\n", rc );
   return;
}

/**************************** WaitSemPost ***************************
      Wait for the event semaphore to be posted by another thread.
*********************************************************************/
void WaitSemPost( void )
{
   int rc;

   rc = sema_wait( &semaphore );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "WaitSemPost: Error from sema_wait: %d; Exiting.\n", rc );
      exit( -1 );
   }
   return;
}

/**************************** DestroySemaphore *************************
                         Kill the event semaphore.
************************************************************************/
void DestroySemaphore( void )
{
   sema_destroy( &semaphore );
   return;
}


/************************** CreateMutex_ew *************************
     Set up mutex semaphore to arbitrate the use of some variable
        by different threads.
     Mutex is created "private" and "unowned"
********************************************************************/
void CreateMutex_ew( void )
{
   void *dummy = NULL;
   int   rc;

   rc = mutex_init( &mutex, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "CreateMutex_ew: Error from mutex_init: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/*************************** RequestMutex **************************
                       Grab the mutex semaphore
********************************************************************/
void RequestMutex( void )
{
   int   rc;

   rc = mutex_lock( &mutex );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "RequestMutex: Error from mutex_lock: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/************************ ReleaseMutex_ew **************************
                    Release the mutex semaphore
********************************************************************/
void ReleaseMutex_ew( void )
{
   int   rc;

   rc = mutex_unlock( &mutex );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "ReleaseMutex_ew: Error from mutex_unlock: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/***************************** CloseMutex **************************
                        Destroy mutex semaphore
********************************************************************/
void CloseMutex( void )
{
   int   rc;

   rc = mutex_destroy( &mutex );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "CloseMutex: Error from mutex_destroy: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}

/************************ CreateSpecificMutex **********************
        As Above, but allows many to be created. Story: The original
        routines were written presuming that only one mutex would
        be used. These routines have become imbedded in much code.
        The ...Specific... versions are created for wave_server2.
        Alex 1/19/97
********************************************************************/
void CreateSpecificMutex( mutex_t* mp )
{
   void *dummy = NULL;
   int   rc;

   rc = mutex_init( mp, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "CreateSpecificMutex_ew: Error from mutex_init: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/********************* RequestSpecificMutex ************************
                     Grab the mutex semaphore
********************************************************************/
void RequestSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_lock( mp );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "RequestSpecificMutex: Error from mutex_lock: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/******************** ReleaseSpecificMutex *************************
                  Release the mutex semaphore
********************************************************************/
void ReleaseSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_unlock( mp );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "ReleaseSpecificMutex: Error from mutex_unlock: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}


/********************** CloseSpecificMutex *************************
                      Destroy mutex semaphore
********************************************************************/
void CloseSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_destroy( mp );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "CloseSpecificMutex: Error from mutex_destroy: %d; Exiting.\n",
               rc );
      exit( -1 );
   }
   return;
}

