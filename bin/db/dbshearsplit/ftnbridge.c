#include "tr.h"

void pfkeys_( int * pf, int * tp)
{
    Tbl * t;
    t = ( pfkeys( (Pf*)(*pf) ) );
    *tp = (int) t;
}


int intbl_( int * tb, char * str, int * numchars )
{
    Tbl * results;
    int i;
    int numrecs = maxtbl( (Tbl*)(*tb) );
    char * buffer = malloc( (*numchars + 1) * sizeof(char));
    char * nextel;

    strncpy( buffer, str, *numchars );
    buffer[*numchars] = '\0';

    for ( i = 0; i < numrecs; i++ )
    {
        nextel = strdup( gettbl( (Tbl*)(*tb),i ) );
        if ( strcmp( nextel, buffer ) == 0 )
        {
            free( nextel );
            return 1;
        }
        free( nextel );
    }

    return 0;
}

void trfilter_( Dbptr * tr, char * filter_string)
{
    trfilter( *tr, filter_string );
}


void trfilter_segs_(int * nsegs,int * nsamps, double * dt, float ** data,
                             char * filter_string)
{
    trfilter_segs( *nsegs, nsamps, dt, data, filter_string );
}

void trfilter_seg_( int * nsamps, double * db, float * data, char * filter_string )
{
    int nsegs = 1;
    float * dp[1];
    dp[0] = data;

    trfilter_segs( nsegs, nsamps, db, dp, filter_string );
}
