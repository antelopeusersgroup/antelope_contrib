
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.4  2003/06/01 08:25:39  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.1  2000/02/14 20:05:54  lucky
 *     Initial revision
 *
 *
 */

/*
 *  chron3.h   include file for time-conversion routines
 */

#ifndef CHRON3_H
#define CHRON3_H

#include <time.h>

#define GSEC1970 11676096000.00 /* Gregorian seconds equivalent for 19700101 */
                                /* # seconds between Carl Johnson's chron3   */
                                /* time 0 and 1970-01-01 00:00:00.0 GMT      */

struct Greg {
        int year;
        int month;
        int day;
        int hour;
        int minute;
        float second;
};

/*  Function prototypes  */

void date20( double, char * );
void date17( double, char * );

struct Greg *datime( double, struct Greg * );  /*changed to be MT-safe */
struct Greg *gregor( long, struct Greg * );    /*changed to be MT-safe */
struct Greg *grg( long, struct Greg * );       /*changed to be MT-safe */

long julian( struct Greg * );
long julmin( struct Greg * );

double julsec17( char * );
int    epochsec17( double *, char * );
time_t timegm( struct tm * );

double tnow( void );

#endif

