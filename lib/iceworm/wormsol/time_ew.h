
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

#define DATESTR23  23   /* length of string required by datestr23() */


#endif
