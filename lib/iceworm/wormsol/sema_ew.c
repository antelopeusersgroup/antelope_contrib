/*
 * sema_ew.c  Solaris version
 *
 * This source file contains system-dependent functions for
 * handling event semaphores and mutex semaphores
 *
 */

#include <stdio.h>
#include <synch.h>

static sema_t   semaphore;  /* Event Semaphore                        */
static mutex_t  mutex;      /* Mutual Exclusion semaphore lock        */
static char    *ProgName;   /* label all error msgs with program name */


/************************ CreateSemaphore_ew ************************
    Create a semaphore which is posted when some event is complete.
*********************************************************************/
void CreateSemaphore_ew( char *prog )
{
   static unsigned int count = 0;
   void *dummy;
   int   rc;

   ProgName = prog;

   rc = sema_init( &semaphore, count, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      fprintf( stderr, 
              "%s/CreateSemaphore_ew: Error from sema_init: %d; exitting!\n", 
               ProgName, rc );
      exit( -1 ); 
   }
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
              "%s/PostSemaphore: Error from sema_post: %d\n", 
               ProgName, rc );
}

/**************************** WaitSemPost ***************************
      Wait for the event semaphore to be posted by another thread.
*********************************************************************/
void WaitSemPost( unsigned long *nPost )
{
   int rc;

   rc = sema_wait( &semaphore );
   if ( rc != 0 )
   {
      fprintf( stderr,
              "%s/WaitSemPost: Error from sema_wait: %d; exitting!\n", 
               ProgName, rc );
      exit( -1 ); 
   }
   *nPost = 1;
}

/**************************** DestroySemaphore *************************
                         Kill the event semaphore.
************************************************************************/
void DestroySemaphore( void )
{
   sema_destroy( &semaphore );
}


/************************** CreateMutex_ew *************************
     Set up mutex semaphore to arbitrate the use of some variable 
        by different threads.
     Mutex is created "private" and "unowned"
********************************************************************/
/*void CreateMutex_ew( char *prog )*/
void CreateMutex_ew( void )
{
   void *dummy;
   int   rc;

   /*ProgName = prog;*/

   rc = mutex_init( &mutex, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/CreateMutex_ew: Error from mutex_init: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "CreateMutex_ew: Error from mutex_init: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
}


/*************************** RequestMutex **************************
                    Grab the muxtex semaphore
********************************************************************/
void RequestMutex( void )
{
   int   rc;

   rc = mutex_lock( &mutex );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/RequestMutex: Error from mutex_lock: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "RequestMutex: Error from mutex_lock: %d; exitting!\n", 
               rc );
      exit( -1 );
   }
}


/************************ ReleaseMutex_ew **************************
                  Release the muxtex semaphore
********************************************************************/
void ReleaseMutex_ew( void )
{
   int   rc;

   rc = mutex_unlock( &mutex );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/ReleaseMutex_ew: Error from mutex_unlock: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "ReleaseMutex_ew: Error from mutex_unlock: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
}


/***************************** CloseMutex **************************
                     Destroy muxtex semaphore
********************************************************************/
void CloseMutex( void )
{
   int   rc;

   rc = mutex_destroy( &mutex );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/CloseMutex: Error from mutex_destroy: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "CloseMutex: Error from mutex_destroy: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
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
   void *dummy;
   int   rc;

   /*ProgName = prog;*/

   rc = mutex_init( mp, USYNC_THREAD, dummy );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/CreateSpecificMutex_ew: Error from mutex_init: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "CreateSpecificMutex_ew: Error from mutex_init: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
}


/********************* RequestSpecificMutex ************************
                    Grab the muxtex semaphore
********************************************************************/
void RequestSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_lock( mp );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/RequestSpecificMutex: Error from mutex_lock: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "RequestSpecificMutex: Error from mutex_lock: %d; exitting!\n", 
               rc );
      exit( -1 );
   }
}


/******************** ReleaseSpecificMutex *************************
                  Release the muxtex semaphore
********************************************************************/
void ReleaseSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_unlock( mp );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/ReleaseSpecificMutex: Error from mutex_unlock: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "ReleaseSpecificMutex: Error from mutex_unlock: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
}


/********************** CloseSpecificMutex *************************
                     Destroy muxtex semaphore
********************************************************************/
void CloseSpecificMutex( mutex_t* mp )
{
   int   rc;

   rc = mutex_destroy( mp );
   if ( rc != 0 )
   {
      /*fprintf( stderr,
              "%s/CloseSpecificMutex: Error from mutex_destroy: %d; exitting!\n", 
               ProgName, rc );*/
      fprintf( stderr,
              "CloseSpecificMutex: Error from mutex_destroy: %d; exitting!\n", 
               rc );
      exit( -1 ); 
   }
}

