
              /***********************************************
               *                threads_ew.c                 *
               *              Solaris version                *
               *                                             *
               *  This file contains functions StartThread,  *
               *  WaitThread, and KillThread                 *
               ***********************************************/

#include <stdio.h>
#include <sys/types.h>
#include <thread.h>
#include <signal.h>

void SignalHandle( int );


   /********************************************************************
    *                           StartThread                            *
    *                                                                  *
    * Arguments:                                                       *
    *     fun:        Name of thread function. Must take (void *)      *
    *                 as an argument and return void                   *
    *     stack_size: Stack size of new thread in bytes                *
    *                 In OS2, if zero the stack size is set to 8192    *
    *                 In SOLARIS, this argument is ignored             *
    *     thread_id:  Thread identification number returned to         *
    *                 calling program.                                 *
    * Returns:                                                         *
    *    -1 if error                                                   *
    *     0 if ok                                                      *
    ********************************************************************/

int StartThread( void *fun(void *), unsigned stack_size, unsigned *thread_id )
{
   int rc;                       /* Function return code */
   thread_t tid;                 /* SendMsg thread id */
   size_t stackSize = 0;

/* Set up a signal-handling function to be inherited by threads
   ************************************************************/
   sigset( SIGUSR1, &SignalHandle ); 
 
/* Start the thread
   ****************/
   /* Note: THR_DETACHED is required for thr_exit to work. That is,
      a detached thread can truly kill itself without lingering in
      some afterlife, waiting for some other thread to pick up it's exit
      status before it can truly cease to be...*/
   rc = thr_create( (void *)0, stackSize, fun, (void *)0,
                    THR_DETACHED|THR_NEW_LWP, &tid );
   if ( rc != 0 )
      return( -1 );

   *thread_id = (unsigned)tid;
   return( 0 );
}


   /********************************************************************
    *                        StartThreadWithArg                        *
    *                                                                  *
    * Arguments:                                                       *
    *     fun:        Name of thread function. Must take (void *)      *
    *                 as an argument and return void.                  *
    *	  arg:	      an unsigned long (void*), passed to the thread   *
    *     stack_size: Stack size of new thread in bytes                *
    *                 In OS2, if zero the stack size is set to 8192    *
    *                 In SOLARIS, this argument is ignored             *
    *     thread_id:  Thread identification number returned to         *
    *                 calling program.                                 *
    * Returns:                                                         *
    *    -1 if error                                                   *
    *     0 if ok                                                      *
    ********************************************************************/

int StartThreadWithArg( void *fun(void *),void* arg, unsigned stack_size, 
			unsigned *thread_id )
{
   int rc;                       /* Function return code */
   thread_t tid;                 /* SendMsg thread id */
   size_t stackSize = 0;

/* Set up a signal-handling function to be inherited by threads
   ************************************************************/
   sigset( SIGUSR1, &SignalHandle ); 
 
/* Start the thread
   ****************/
   /* Note: THR_DETACHED is requrired for thr_exit to work. That is,
      a detached thread can truly kill itsself without lingering in
      some afterlife, waiting for some other thread to pick up it's exit
      status before it can truly cease to be...*/
   rc = thr_create( (void *)0, stackSize, fun, (void *)arg,
                    THR_DETACHED|THR_NEW_LWP, &tid );
   if ( rc != 0 )
      return( -1 );

   *thread_id = (unsigned)tid;
   return( 0 );
}


  /*************************************************************
   *                          WaitThread                       *
   *                    Wait for thread to die.                *
   *                                                           *
   *             This is a dummy function in Solaris.          *
   *                                                           *
   * Argument:                                                 *
   *    thread_id = Pointer to thread id                       *
   *************************************************************/

void WaitThread( unsigned *thread_id )
{
}


   /*************************************************************
    *                        KillThread                         *
    *                Force a thread to exit now.                *
    *                                                           *
    * Argument:                                                 *
    *    tid = id of thread to kill                             *
    *                                                           *
    * Returns:                                                  *
    *     0 if ok                                               *
    *     non-zero value indicates an error                     *
    *************************************************************/

int KillThread( unsigned int tid )
{
   return( thr_kill( (thread_t) tid, SIGUSR1 ) );
}



   /***************************************************************
    *                         KillSelfThread                      *
    *     For a thread exit without affecting other threads       *
    *                                                             *
    *      Thread must have been created with the THR_DETACHED    *
    *      bit set; else a zombie lingers, waiting for someone    * 
    *      to pick up it's exit status                            *
    ***************************************************************/

int KillSelfThread( )
{
   thr_exit( (void *)NULL );
   return 0;                     /* well, not really */
}



   /*************************************************************
    *                        SignalHandle                       *
    *         Decide what to do when a signal is caught         *  
    *  Added for use with the KillThread function so that       *
    *  killed threads will exit gracefully                      *
    *************************************************************/

void SignalHandle( int sig )
{
   void *status;
 
   switch (sig)
   {
   case SIGUSR1:
        /*printf( "thread:%d caught SIGUSR1; calling thr_exit()\n",
                 (int) thr_self() );*/ /*DEBUG*/
        thr_exit( status );
   }
}
    
