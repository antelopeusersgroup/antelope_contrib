
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.2  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.2  2001/07/01 22:09:26  davidk
 *     Added prototype for datestr23_local().
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */


     /********************************************************
      *                      time_ew.h                       *
      *                                                      *
      *  Include file for the earthworm multi-thread safe    *
      *  versions of time routines.                          *
      *  Note: #include <time.h>    must be placed before    *
      *        #include <time_ew.h> in each source file      *
      *  using the earthworm time functions.                 *
      ********************************************************/

#ifndef TIME_EW_H
#define TIME_EW_H

#include <time.h>

/* Function prototypes
 *********************/
struct tm *gmtime_ew   ( const time_t *, struct tm * );
time_t     timegm_ew   ( struct tm * );
struct tm *localtime_ew( const time_t *, struct tm * );
char      *ctime_ew    ( const time_t *,    char *, int );
char      *asctime_ew  ( const struct tm *, char *, int );
double     hrtime_ew   ( double * );
char      *datestr23   ( double, char *, int );
char *datestr23_local( double t, char *pbuf, int len );


#define DATESTR23  23   /* length of string required by datestr23() */


#endif
