
          /***************************************************
           *                  earthworm.h                    *
           *                                                 *
           *         Earthworm administrative setup:         *
           *        global info for all installations        *
           *                                                 *
           ***************************************************/

#ifndef EARTHWORM_H
#define EARTHWORM_H

/* System-dependent stuff goes here
   ********************************/
#include "platform.h"

/* Define unique port numbers for interprocess communications
 ************************************************************/
#define WAVE_SERVER_PORT  16022    /* for requesting/receiving trace data */


/* Define unique keys for message queues
 ***************************************/
#define MQKEY_1         1010       /* key for status message queue   */


/* Define all possible installation name/installation id pairs:
 *************************************************************/
static const int Max_Installation = 21;

static struct {
   char           *name;
   unsigned char   id;    /* (0-255 are valid id #'s) */
} EW_Installation[] = {
                        { "INST_WILDCARD",   0 },
                        { "INST_FAIRBANKS",  1 },
                        { "INST_UW",         2 },
                        { "INST_MENLO",      3 },
                        { "INST_CIT",        4 },
                        { "INST_UTAH",       5 },
                        { "INST_MEMPHIS",    6 },
                        { "INST_UNR",        7 },
                        { "INST_UCB",        8 },
                        { "INST_PTWC",       9 },
                        { "INST_IDA",       10 },
                        { "INST_NC",        11 },
                        { "INST_VT",        12 },
                        { "INST_USNSN",     13 },
                        { "INST_RICKS",     14 },
                        { "INST_HVO",       15 },
                        { "INST_ATWC",      16 },
                        { "INST_PGE",       17 },
			{ "INST_PNNL",      18 },
			{ "INST_PGC",       19 },
			{ "INST_AVO",       20 }

                      };


/* Define error words (2-bytes; >9999) global to earthworm modules
   Values 0-9999 are available for private error definitions within modules
 **************************************************************************/
#define ERR_LAPPED      10000    /* data loss; overwritten in transport ring */
#define ERR_SEQGAP      10001    /* data loss; sequence gap in msgs received */
#define ERR_OVERFLOW    10002    /* data transfer failed; allocated space at */
                                 /* target address exceeded                  */
#define ERR_UNTRACKED   10003    /* transport.h tracking limit exceeded      */


/* Set limits on certain things
 ******************************/
#define MAX_PHS_PER_EQ    250    /* set the maximum #phases to include when  */
                                 /* processing an earthquake                 */
#define MAX_BYTES_PER_EQ  (450+225*(MAX_PHS_PER_EQ))
                                 /* generous maximum size of a Hypoinverse   */
                                 /* archive message based on Fred Klein's    */
                                 /* "shadow.doc" file dated March 12, 1997   */ 
#define MAX_PIN_NUM       500    /* Max pin number possible at this          */
				 /* installation                             */

/* Prototypes for Earthworm utility functions
 ********************************************/
int  copyfile( char *, char *, char *, char *, char *, char *, char * );
                                            /* copyfile.c   system-dependent */

int  chdir_ew( char * );                    /* dirops_ew.c  system-dependent */

int  GetDiskAvail( char *, unsigned * );    /* getavail.c   system-dependent */

long GetKey  ( char * );                    /* getutil.c    sys-independent  */
int  GetInst ( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetModId( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetType ( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetLocalInst( unsigned char * );       /* getutil.c    sys-independent  */
void GetUtil_LoadTable( void );             /* getutil.c    sys-independent  */

int  getsysname_ew( char *, int );          /* getsysname_ew.c sys-dependent */

void logit_init( char *, short, int, int ); /* logit.c      sys-independent  */
void logit( char *, char *, ... );          /* logit.c      sys-independent  */

int  pipe_init ( char *, unsigned long );   /* pipe.c       system-dependent */
int  pipe_put  ( char *, int );             /* pipe.c       system-dependent */
int  pipe_get  ( char *, int, int * );      /* pipe.c       system-dependent */
void pipe_close( void );                    /* pipe.c       system-dependent */

void CreateSemaphore_ew( char * );          /* sema_ew.c    system-dependent */
void PostSemaphore   ( void );              /* sema_ew.c    system-dependent */
void WaitSemPost     ( unsigned long * );   /* sema_ew.c    system-dependent */
void DestroySemaphore( void );              /* sema_ew.c    system-dependent */
void CreateMutex_ew  ( void );              /* sema_ew.c    system-dependent */
void RequestMutex( void );                  /* sema_ew.c    system-dependent */
void ReleaseMutex_ew( void );               /* sema_ew.c    system-dependent */
void CloseMutex( void );                    /* sema_ew.c    system-dependent */
void CreateSpecificMutex( mutex_t * );
void RequestSpecificMutex( mutex_t * );
void ReleaseSpecificMutex( mutex_t * );

int SendMail( char [][60], int, char *, char * );   /* sendmail.c   system-dependent */
int SendPage( char * );                     /* sendpage.c   system-dependent */

void sleep_ew( unsigned );                  /* sleep_ew.c   system-dependent */

void SocketSysInit( void   );               /* socket_ew.c  system-dependent */
void SocketClose  ( int    );               /* socket_ew.c  system-dependent */
void SocketPerror ( char * );               /* socket_ew.c  system-dependent */
int sendall( int, const char *, long, int );/* socket_ew.c  system-dependent */

int  WaitThread( unsigned * );              /* threads_ew.c system-dependent */
int  KillThread( unsigned int );            /* threads_ew.c system-dependent */
int  KillSelfThread( void );                /* threads_ew.c system-dependent */
int  StartThread( thr_ret (void *), unsigned, unsigned * );
int  StartThreadWithArg( thr_ret (void *), void *, unsigned, unsigned * );

#endif
