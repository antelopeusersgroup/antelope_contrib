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
 *     Revision 1.3  2002/06/06 19:34:50  lucky
 *     Added geo_to_km_deg
 *
 *     Revision 1.2  2001/07/01 21:59:42  davidk
 *     Added prototype for geo_to_km() from libsrc/util/geo_to_km.c
 *
 *     Revision 1.1  2001/04/06 21:03:30  davidk
 *     Initial revision
 *
 *
 ************************************************************/

#ifndef EARTHWORM_SIMPLE_FUNCS_H
# define EARTHWORM_SIMPLE_FUNCS_H

/* DO NOT PUT ANY #includes in this file!!!! */

/* This file contains prototypes for earthworm libsrc
   functions that are simple and require no special
   type definitions.  If you have more complex functions
   (semaphores, threads, mutexes, sockets, etc.) you should
   put them in earthworm_complex_funcs.h

   Note, please try to keep functions from the same object
   together in one section of one file.  So all of the logit.c
   stuff should go together.
   Davidk 2001/04/06
*************************************************************/

/* Prototypes for Earthworm utility functions
 ********************************************/
long GetKey  ( char * );                    /* getutil.c    sys-independent  */
int  GetInst ( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetModId( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetType ( char *, unsigned char * );   /* getutil.c    sys-independent  */
int  GetLocalInst( unsigned char * );       /* getutil.c    sys-independent  */
void GetUtil_LoadTable( void );             /* getutil.c    sys-independent  */

void logit_init( char *, short, int, int ); /* logit.c      sys-independent  */
void html_logit( char *, char *, ... );     /* logit.c      sys-independent  */
void logit( char *, char *, ... );          /* logit.c      sys-independent  */
int  get_prog_name( char *, char * );       /* logit.c      sys-independent  */

int SendMail( char [][60], int, char *, char *, 
                     char *, char *, char *, char * );   

/* System-dependent stuff goes here
   ********************************/

int  copyfile( char *, char *, char *, char *, char *, char *, char * );
                                            /* copyfile.c   system-dependent */

int  chdir_ew( char * );                    /* dirops_ew.c  system-dependent */
int  CreateDir( char * );                   /* dirops_ew.c  system-dependent */
int  RecursiveCreateDir( char * );          /* dirops_ew.c  system-dependent */
int  GetFileName( char * );                 /* dirops_ew.c  system-dependent */
int  rename_ew( char *, char * );           /* dirops_ew.c  system-dependent */

int  GetDiskAvail( unsigned * );            /* getavail.c   system-dependent */

int  getsysname_ew( char *, int );          /* getsysname_ew.c sys-dependent */

int SendPage( char * );                     /* sendpage.c   system-dependent */

void sleep_ew( unsigned );                  /* sleep_ew.c   system-dependent */


int  pipe_init ( char *, unsigned long );   /* pipe.c       system-dependent */
int  pipe_put  ( char *, int );             /* pipe.c       system-dependent */
int  pipe_get  ( char *, int, int * );      /* pipe.c       system-dependent */
void pipe_close( void );                    /* pipe.c       system-dependent */

/* from geo_to_km.c */
int geo_to_km (double lat1, double lon1, double lat2, double lon2,
              double* dist, double* azm);
int geo_to_km_deg (double lat1, double lon1, double lat2, double lon2,
              double* dist, double *xdeg, double* azm);

#endif /* EARTHWORM_SIMPLE_FUNCS_H */
