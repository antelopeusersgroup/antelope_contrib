/*
 *  chron3.h   include file for time-conversion routines
 */

#ifndef CHRON3_H
#define CHRON3_H

#define CENTURY 1900

#include <time.h>

struct Greg {
        int year;
        int month;
        int day;
        int hour;
        int minute;
        float second;
};

/*  Function prototypes  */

void date18( double, char * );
void date15( double, char * );

struct Greg *datime( double );
struct Greg *gregor( long );
struct Greg *grg( long );

long julian( struct Greg * );
long julmin( struct Greg * );

double julsec15( char * );
int    epochsec15( double *, char * );
time_t timegm( struct tm * );

double tnow( void );

#endif

