
              /*************************************************
               *                   platform.h                  *
               *                                               *
               *  System-dependent stuff.                      *
               *  This file is included by earthworm.h         *
               *************************************************/

#ifndef PLATFORM_H
#define PLATFORM_H

#ifdef _WINNT
#include <windows.h>
#include <winsock.h>               /* Socket stuff */
#include <process.h>               /* Required for getpid() */
#include <sys\types.h>
#define thr_ret void               /* Thread functions return this */
#define getpid _getpid
typedef int    pid_t;
typedef HANDLE sema_t;
typedef HANDLE mutex_t;
typedef HANDLE timer_t;
#endif

#ifdef _OS2
#define INCL_DOSPROCESS
#define INCL_DOSMEMMGR
#define INCL_DOSSEMAPHORES
#define INCL_DOSFILEMGR
#include <os2.h>
#include <netinet\in.h>       /* contains typedef of struct sockaddr_in */
#include <process.h>               /* Required for getpid() */
#include <types.h>
#include <nerrno.h>
#include <sys\socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */
typedef void thr_ret;              /* Thread functions return this */
typedef int  pid_t;
typedef HEV  sema_t;
typedef HMTX mutex_t;
typedef long timer_t;

typedef long DWORD;
#endif

#ifdef _SOLARIS
#include <sys/types.h>
#include <netinet/in.h>            /* Socket stuff */
#include <arpa/inet.h>             /* Socket stuff */
#include <signal.h>
#include <synch.h>                 /* for mutex's */
#include <sys/ipc.h>
#include <sys/shm.h>
#include <wait.h>
#include <thread.h>
#include <unistd.h>
#include <sys/socket.h>            /* Socket stuff */
#include <netdb.h>                 /* Socket stuff */
#define thr_ret void*              /* Thread functions return this */

typedef long LONG;
typedef long DWORD;
#endif

#endif
