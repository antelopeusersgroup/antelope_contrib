
    /********************************************************************/
    /*                                                                  */
    /*                          transport.h                             */
    /*                                                                  */
    /*             Include file for transport functions                 */
    /*                to access shared memory regions.                  */
    /*                                                                  */
    /* This include file requires that _SOLARIS, _WIN32, or _OS2 be     */
    /* defined in the makefile.                                         */
    /*                                                                  */
    /********************************************************************/

#ifndef TRANSPORT_H
#define TRANSPORT_H

#include <platform.h>

/* Structure types used in transport.c */

typedef struct {                   /********** shared memory header *********/
        long             nbytes;   /* total size of shared memory region    */
        unsigned long    keymax;   /* # usable bytes (nbytes - SHM_HEAD)    */
        unsigned long    keyin;    /* index of next available byte          */
        unsigned long    keyold;   /* index of oldest complete message      */
        short            flag;     /* flag watched by attached programs     */
} SHM_HEAD;                        /*****************************************/


typedef struct {                   /******* shared memory information *******/
        SHM_HEAD *       addr;     /* pointer to beginning of shred mem reg */
        long             key;      /* key to shared memory region           */
                                   /*                                       */
#ifdef _SOLARIS                    /* SOLARIS ONLY:                         */
        long             mid;      /* shared memory region identifier       */
        long             sid;      /* associated semaphore identifier       */
#endif                             /*                                       */
#ifdef _OS2                        /* OS2 ONLY:                             */
        PVOID            objAlloc; /* pointer to memory object              */
        HMTX             hmtx;     /* mutex semaphore handle                */
#endif                             /*                                       */
#ifdef  _WINNT                     /* WIN NT or 95 ONLY                     */
        HANDLE           hShare;   /* shared memory region handle           */
        HANDLE           hMutex;   /* mutex handle                          */
#endif                             /*                                       */
} SHM_INFO;                        /*****************************************/


typedef struct {                   /******** description of message *********/
        unsigned char    type;     /* message is of this type               */
        unsigned char    mod;      /* was created by this module id         */
        unsigned char    instid;   /* at this installation                  */
} MSG_LOGO;                        /*****************************************/


typedef struct {                     /******** transport layer header *********/
        char             start;      /* byte to flag beginning of this msg    */
        long             size;       /* size of msg (not including TPORT_HEAD)*/
        MSG_LOGO         logo;       /* description of message source         */
        unsigned char    seq;        /* sequence number of message            */
} TPORT_HEAD;                        /*****************************************/


typedef struct {                     /***** sequence #, outpointer tracker ****/
        long             memkey;     /* key to memory region being accessed   */
        MSG_LOGO         logo;       /* description of message source         */
        unsigned char    seq;        /* sequence number of message            */
        unsigned long    keyout;     /* points to msg after last one "got"    */
        unsigned char    active;     /* 0 until msg of logo is found in memkey*/
} MSG_TRACK;                         /*****************************************/


/* Definitions for tracking message logos (type,module,class) */
#define WILD          0   /* wildcard for message descriptor       */
#define NTRACK_PUT   20   /* max # message trackers for a "putter" */
#define NTRACK_GET   50   /* max # message trackers for a "getter" */
#define FIRST_BYTE  111   /* byte-value to flag beginning of msg   */
                          /* Note: To work on both Solaris & OS2,  */
                          /*       FIRST_BYTE must be from 0-127   */

/* Definitions of return values for tport_putmsg() and/or tport_copyto() */
#define PUT_OK        1   /* put the message in memory, no problems           */
#define PUT_NOTRACK  -1   /* NTRACK_PUT exceeded; msg not sent [tport_putmsg] */
#define PUT_TOOBIG   -2   /* message is too big for shared memory             */

/* Definitions of return values for tport_getmsg() and/or tport_copyfrom() */
#define GET_OK           1  /* got a requested message (modid,type,class)     */
#define GET_NONE         0  /* no messages of requested logo(s) in memory     */
#define GET_MISS        -1  /* got a message, but missed some [tport_getmsg]  */
#define GET_NOTRACK     -2  /* got a message, but NTRACK_GET was exceeded     */
#define GET_TOOBIG      -3  /* next message of requested logo(s) is too       */
                            /* long for caller's buffer; no msg retrieved     */
#define GET_MISS_LAPPED -4  /* got a message, but some were overwritten       */
                            /* before we saw to them [tport_copyfrom]         */
#define GET_MISS_SEQGAP -5  /* got a message, but there was a gap in seq #'s; */
                            /* missed msgs were either never in the ring, or  */
                            /* were previously found "toobig" [tport_copyfrom]*/

/* Definitions for semaphore operations */
#define SHM_INUSE    -1   /* add to semval to flag that memory is in use */
#define SHM_FREE      1   /* add to semval to flag that memory is free   */

/* Definitions for shared memory header flag */
#define TERMINATE  -999   /* tells attached programs to detach & terminate */


/* These functions are in transport.c */

void  tport_create( SHM_INFO *, long, long );
void  tport_destroy( SHM_INFO * );
void  tport_attach( SHM_INFO *, long );
void  tport_detach( SHM_INFO * );
int   tport_putmsg( SHM_INFO *, MSG_LOGO *, long, char * );
int   tport_getmsg( SHM_INFO *, MSG_LOGO *, short, MSG_LOGO *,
                    long *, char *, long );
void  tport_putflag( SHM_INFO *, short );
int   tport_getflag( SHM_INFO * );
int   tport_buffer  ( SHM_INFO *, SHM_INFO *, MSG_LOGO *, short, unsigned,
                      unsigned char, unsigned char );
int   tport_copyto  ( SHM_INFO *, MSG_LOGO *, long, char *, unsigned char );
int   tport_copyfrom( SHM_INFO *, MSG_LOGO *, short, MSG_LOGO *,
                      long *, char *, long, unsigned char * );

/* SOLARIS ONLY: */
/* This union definition is in sys/sem.h in SunOS, but not in Solaris */
/* It's an argument template for semctl system calls on semaphores    */
#ifdef _SOLARIS
union semun
{
        int              val;   /* value for SETVAL */
        struct semid_ds *buf;   /* buffer for IPC_STAT & IPC_SET */
        unsigned short  *array; /* array for GETALL & SETALL */
};
#endif

#endif
