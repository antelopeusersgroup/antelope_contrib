/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.1  2003/06/01 08:25:39  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.1  2001/04/06 21:03:30  davidk
 *     Initial revision
 *
 *
 ************************************************************/

#ifndef EARTHWORM_COMPLEX_FUNCS_H
# define EARTHWORM_COMPLEX_FUNCS_H

/* This file contains prototypes for earthworm libsrc
   functions that require special type definitions, such as
   (semaphores, threads, mutexes, sockets, etc.).

   If you have functions that only use primitive types and you
   do not need any extra header files for them to compile, then
   you can put them into earthworm_simple_funcs.h.

   Note, please try to keep functions from the same object
   together in one section of one file.  So all of the sema_ew.c
   stuff should go together.  Thank You!
   Davidk 2001/04/06
*************************************************************/

/* System-dependent stuff goes here
   ********************************/
#include <platform.h>

void CreateSemaphore_ew( void );            /* sema_ew.c    system-dependent */
void PostSemaphore   ( void );              /* sema_ew.c    system-dependent */
void WaitSemPost     ( void );              /* sema_ew.c    system-dependent */
void DestroySemaphore( void );              /* sema_ew.c    system-dependent */
void CreateMutex_ew  ( void );              /* sema_ew.c    system-dependent */
void RequestMutex( void );                  /* sema_ew.c    system-dependent */
void ReleaseMutex_ew( void );               /* sema_ew.c    system-dependent */
void CloseMutex( void );                    /* sema_ew.c    system-dependent */
void CreateSpecificMutex( mutex_t * );
void CloseSpecificMutex( mutex_t * );
void RequestSpecificMutex( mutex_t * );
void ReleaseSpecificMutex( mutex_t * );

                                            /* sendmail.c   system-dependent */
void SocketSysInit( void   );               /* socket_ew.c  system-dependent */
void SocketClose  ( int    );               /* socket_ew.c  system-dependent */
void SocketPerror ( char * );               /* socket_ew.c  system-dependent */
int sendall( int, const char *, long, int );/* socket_ew.c  system-dependent */

int  WaitThread( unsigned * );              /* threads_ew.c system-dependent */
int  KillThread( unsigned int );            /* threads_ew.c system-dependent */
int  KillSelfThread( void );                /* threads_ew.c system-dependent */
int  StartThread( thr_ret (void *), unsigned, unsigned * );
int  StartThreadWithArg( thr_ret (void *), void *, unsigned, unsigned * );

#endif /* EARTHWORM_COMPLEX_FUNCS_H */
