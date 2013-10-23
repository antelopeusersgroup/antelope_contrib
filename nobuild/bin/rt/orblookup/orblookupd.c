#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>
#include <ctype.h>
#include <pwd.h>
#include <float.h>
#include <errno.h>
#ifndef MAXINT
#include <limits.h>
#define MAXINT INT_MAX
#endif
#include "coords.h"
#include "orb.h"
#include "Pkt.h"
#include "db.h"
#include "orblookupd.h"


/* Simplifies the task of grabbing data stored in a pf object.  Returns
 * either an Arr*, a Tbl*, or a char*.  Caller is expected to know what is
 * being returned. */
 
void *
getfrom_pfstruct( Pf * pf )
{
    if ( pf == NULL ) return NULL;
    switch( pf->type )
    {
        case PFARR:     return pf->value.arr;
        case PFTBL:     return pf->value.tbl;
        default:        return pf->value.s;
    }
}

/* Shorthand for accessing the pf-nested contents of an array. */
void *
getpf( Arr * arr, char * index )
{
    if ( arr == NULL ) return NULL;
    return ( getfrom_pfstruct( (Pf*) getarr( arr, index ) ) );
}

/* Recursively removes pf instances from the pf object and returns the
 * objects' contents in an entirely new structure so that no memory is reused.
 * Requires the caller have some knowledge about the structure of the data. */
void *
pf_collapse( Pf * pf )
{
    Arr * currarr;
    Tbl * currtbl;
    char * key;
    void * value;
    int i;
    int size;
    
    if ( pf == NULL ) return NULL;
    switch( pf->type )
    {
        case PFARR:     currtbl = keysarr( pf->value.arr );
                        size = maxtbl( currtbl );
                        /* currarr = newarr( pf->value.arr->rb_comp ); */
                        currarr = newarr( strcmp );
                        for ( i = 0; i < size; i++ )
                        {
                            key = poptbl( currtbl );
                            value = pf_collapse( getarr( pf->value.arr, key ) );
                            setarr( currarr, key, value );
                        }
                        freetbl( currtbl, 0 );
                        return currarr;

        case PFTBL:     size = maxtbl( pf->value.tbl );
                        currtbl = newtbl( size );
                        for ( i = 0; i < size; i++ )
                            settbl( currtbl, i,
                                    pf_collapse( gettbl( pf->value.tbl, i ) ) );
                        return currtbl;

        default:        return strdup( pf->value.s );
    }
}

Pf *
create_pf ( void * value, int type )
{
    Pf * pf = pfnew( type );

    switch ( type )
    {
        case PFARR :  pf->value.arr = (Arr*) value;
                      break;
        case PFTBL :  pf->value.tbl = (Tbl*) value;
                      break;
        default:      pf->value.s = value;
    }
    return pf;
}

int
clrarr( Arr * arr, void (*free_value )() )
{
    Tbl * tbl;
    char * key;
    void * oldval;
    int size;
    int i;

    tbl = keysarr( arr );
    size = maxtbl( tbl );
    for ( i = 0; i < size; i++ )
    {
        key = poptbl( tbl );
        oldval = delarr( arr, key );
        if ( oldval ) free_value( oldval );
    }
    freetbl( tbl, 0 );

    return 1;
}

/* Recursively traverses a Pf object and frees its nested pf-consistent
 * members. */
void recurse_free_pf ( Pf * pf )
{
    Arr * currarr;
    Tbl * currtbl;
    void * key;
    void * value;
    int i;
    int size;


    if ( pf == NULL ) return;

    switch ( pf->type )
    {
        case PFARR:     clrarr( pf->value.arr, recurse_free_pf );
                        break;

        case PFTBL:     clrtbl( pf->value.tbl, recurse_free_pf );
                        break;

        default:        free( pf->value.s );
                        break;
    }

    pffree( pf );
    pf = NULL;
}


/* converts a floating-point number into a string with the given number of
 * decimal places.  Caller must free the allocated string. */
char *
ftoa ( double d, int precision )
{
    char todec[21];
    char num[20];
    sprintf ( todec, "%%.%df", precision );    
    sprintf ( num, todec, d );
    return ( strdup( num ) );
}

/* converts an integer into a string.  Caller must free the allocated
 * string.*/
char *
itoa ( int i )
{
    return ftoa( (double)i, 0 );
}

/* creates a new string by concatenating two strings s1 and s2 together.
 * Unlike strcat, does not alter the original string.  Returns a
 * freshly-allocated char* that must be freed by the caller. */
char *
stradd( char * s1, char * s2 )
{
    char * ns;
    int len;

    if ( !s1 || !s2 ) return NULL;

    len = strlen( s1 ) + strlen( s2 ) + 1;
    ns = (char*) malloc ( len * sizeof(char) );
    sprintf( ns, "%s%s", s1, s2 );

    return strdup(ns);
}

char *
get_station_field( Arr * sinfo, char * name )
{
    return ( (char*) getpf( getpf( sinfo, FIELDS ), name ) );
}

int
set_station_field( Arr * sinfo, char * name, char * value )
{
    Pf * oldval;
    Pf * pf;

    if ( !sinfo || !name || !value  ) return 0;

    pf = create_pf ( value , PFSTRING );
    oldval = setarr( getpf( sinfo, FIELDS ), name, pf );
    /* if ( oldval ) recurse_free_pf( oldval ); */

    return 1;
}

int
sinfo_update ( Arr * sold, Arr * snew )
{
   int i;
   int n1, n2;
   int numfields;
   char * field;
   char * value;
   char * newval;
   char * oldval;
   int newp, oldp;
   Tbl * fields;

   fields = keysarr( getpf( snew, FIELDS ) );

   numfields = maxtbl( fields );
   for ( i = 0; i < numfields; i++ )
   {
       field = (char*) poptbl( fields );
       value = get_station_field( snew, field );
       oldval = get_station_field( sold, field );
       if ( strcmp( field, LATENCY ) == 0 )
       {
           newp = atoi( getpf( snew, LATENCY_ORDER ) );
           oldp = atoi( getpf( sold, LATENCY_ORDER ) );
           if ( ( !oldp  && newp ) || ( newp < oldp ) )
           {
               set_station_field( sold, field, strdup(value) );
               setarr( sold, LATENCY_ORDER, create_pf( itoa(newp), PFSTRING ) );
           }
           else if ( ( ( newp == oldp ) || ( !newp && !oldp ) ) && 
                     ( atoi( value ) < atoi( oldval ) ) )
           {
               set_station_field( sold, field, strdup(value) );
               setarr( sold, LATENCY_ORDER, create_pf( itoa(newp), PFSTRING ) );
           }
       }
       else
           /* If this is a channels value, we want to merge the values */
           if ( strcmp( field, NUMCHANS ) == 0 )
           {
               (!value)? (n1 = 0): (n1 = atoi( value ));
               (!oldval)? (n2 = 0): (n2 = atoi( oldval ));
               set_station_field( sold, field, itoa( n1 + n2 ) );
           }
           else
               if ( value != NULL )
                   set_station_field( sold, field, strdup(value) );
       
   }
   freetbl( fields, 0 );

   return 1;
}

int
get_priority( char * alert_name, orbinfo * oi )
{
    int i;
    int size;
    Arr * level;
    Tbl * alerttbl;

    alerttbl = valsarr( oi->alert_stages );
    size = maxtbl( alerttbl );
    for ( i = 0; i < size; i++ )
    {
        level = (Arr*) poptbl( alerttbl );
        if ( strcmp( getarr( level, NAME ), alert_name ) == 0 )
        {
            freetbl( alerttbl, 0 );
            return atoi( getarr( level, PRIORITY ));
        }
    }
    freetbl( alerttbl, 0 );
    return -1;
}

int
is_station_new( Arr * sinfo )
{
    char * isnew = getpf( sinfo, ISNEW );
    if ( !strcmp( isnew, "1" ) ) return 1;
    return 0;
}

Arr *
get_network( Arr * networks, char * net ) 
{
    return ( getpf( networks, net ) );
}

Arr *
get_stations ( Arr * networks, char * net )
{
    return ( getpf( get_network( networks, net ), STALIST ) );
}

int
add_network ( Arr * networks, char * net )
{
    Arr * network;
    Arr * oldnet;

    if ( get_stations( networks, net ) ) return 0;

    network = newarr( strcmp );

    setarr( networks, net, create_pf( network, PFARR ) );
    setarr( network, STALIST, create_pf( newarr( strcmp ), PFARR) );
    
    return 1;
}

Arr *
get_station ( Arr * networks, char * net, char * sta )
{
    return ( getpf( get_stations( networks, net ), sta ) );
}

int
add_station ( Arr * networks, char * net, char * sta, Arr * sinfo )
{
    Arr * stations;
    Arr * sold;
    char * latency;

    if ( networks == NULL )
        networks = newarr( strcmp );

    if ( sold = get_station( networks, net, sta ) )
    {
        sinfo_update( sold, sinfo );
        clrarr( sinfo, pffree );
    }
    else
    {
        if ( get_stations( networks, net ) == NULL ) 
            add_network( networks, net );

        if ( !(stations = get_stations( networks, net ) ) ) return 0;
        setarr( stations, sta, create_pf( sinfo, PFARR ) );
    }

    return 1;
}

int
set_fullnetname( Arr * networks, char * net, char * name )
{
   Arr * network = get_network( networks, net );
   Pf * oldval;
   oldval = setarr( network, FULLNET, create_pf( name, PFSTRING ) );
   if ( oldval ) recurse_free_pf( oldval );
   return 1;
}

char *
calc_latency ( double time1, double time2 )
{
    return ftoa( time1 - time2, 0 );
}

int
add_latencies ( Arr * networks )
{
    char * net;
    char * sta;
    char * statime;
    Arr * stations;
    Arr * sinfo;
    Arr * network;
    Tbl * stas;
    Tbl * nets;
    Pf * oldval;
    int numstas;
    int numnets;
    int i, j;
    int min = MAXINT;
    int max = 0;

    nets = keysarr( networks );
    numnets = maxtbl( nets );
    for ( i = 0; i < numnets; i++ )
    {
        net = gettbl( nets, i );
        network = getpf( networks, net );
        stations = getpf( network, STALIST );
        stas = keysarr( stations );
        numstas = maxtbl( stas );

        for ( j = 0, min=MAXINT, max=0; j < numstas; j++ )
        {
            sta = gettbl( stas, j );
            sinfo = getpf( stations, sta );
            statime = get_station_field( sinfo, LATENCY );
            if ( statime && ( atoi( statime ) < min ) )
                min = atoi( statime );
            if ( statime && ( atoi( statime ) > max ) )
                max = atoi( statime );
        }
        freetbl( stas, 0 );
        oldval = setarr( network, MINNETLATENCY, 
                         create_pf( itoa(min), PFSTRING ) ); 
        if ( oldval ) recurse_free_pf( oldval );
        oldval = setarr( network, MAXNETLATENCY, 
                         create_pf( itoa(max), PFSTRING ) ); 
        if ( oldval ) recurse_free_pf( oldval );
    }
    freetbl( nets, 0 );

    return 1;
}

int
set_sinfo_limits( Arr * sinfo, orbinfo * oi, char * pkttype )
{
    int i, j;
    int size, size2;
    int numalerts;
    Arr * alert_ranges;
    Arr* ranges; 
    Arr * oldval;
    Pf * newfieldlist;
    Arr * field;
    Pf * thresholds;
    Pf * thresh;
    double high;
    double low;
    double * val;
    Tbl* levels;
    Tbl* byfield;
    Tbl* fields;
    char* level;
    char * name;
    int priority = 0;

    /* Find the alert thresholds applicable to this packet type. If packet
       type not mentioned by name, use the default values. */
       
    if ( !( ranges = getarr( oi->alert_ranges, pkttype) ) ) 
       if ( !( ranges = getarr( oi->alert_ranges, "all") ) ) return 0;

    thresholds = pfnew( PFARR );
    thresholds->value.arr = newarr( strcmp );
    oldval = setarr( sinfo, THRESHOLD, thresholds );
    /* if ( oldval ) freearr( oldval, recurse_free_pf ); */
    
    /* First grab the list of alert levels for this packet type. */
    levels = keysarr( ranges );
    numalerts = maxtbl( levels );
    for ( i =0; i < numalerts; i++ )
    {
        level = poptbl( levels );
        byfield = (Tbl*) getarr( ranges, level );
        
        newfieldlist = create_pf ( newarr( strcmp ), PFARR );
        setarr( thresholds->value.arr, level, newfieldlist );
        priority = get_priority( level, oi );

        /* Next, get the thresholds for this current alert level. */
        size = maxtbl( byfield );
        for ( j = 0; j < size; j++ )
        {
            /* From this, examine each field. */
            field = (Arr*) gettbl( byfield, j );

            thresh = create_pf( newarr( strcmp ), PFARR );

            setarr ( thresh->value.arr, HIGH, 
                     create_pf( strdup( getarr( field, HIGH ) ), PFSTRING) );
            setarr ( thresh->value.arr, LOW, 
                     create_pf( strdup( getarr( field, LOW ) ), PFSTRING) );
            setarr ( thresh->value.arr, PRIORITY, 
                     create_pf( itoa( priority ), PFSTRING));

            name = getarr( field, NAME );
            if ( name == NULL )
                continue;

            oldval = setarr( newfieldlist->value.arr, name, thresh );
            if ( oldval ) free( oldval );
        }
    }

    freetbl( levels, 0 );
    return 1; 
}

char * 
extract_channame( srcname )
    char * srcname;
{
    char * s;
    char * chan;
    char * first;
    char * second;
    char * end;

    s = strdup( srcname );
    first = strchr( s, '_' );
    if ( !first ) return NULL;
    second = strchr( first + 1, '_' );

    if ( !second ) return NULL;
    end = strchr( second, '/' ); 
    chan = second + 1;
    if ( end ) *end = '\0';
    chan = strdup ( chan );
    free( s );

    return chan;
}



char *
extract_sta ( char * srcname )
{
    char * sta;
    char * s = NULL;
    int s1 = -1;
    int s2 = -1;
    int s3 = -1;

    s = strdup( srcname );
    s3 = strcspn( s, "/" );


    /* Look for the first slash.  If it comes at the very beginning, this
       source is useless to us. */
    if ( s3 > 0 )
    {
        s[s3] = '\0';

        /* Now, find the first underscore. */
        s1 = strcspn( s, "_" );

        if ( s1 > -1 )
        {
            /* Find the second underscore. */
            s2 = strcspn( &s[++s1], "_" );

            if ( s2 > -1 )
            {
                /* Mark the second underscore, if it exists, as the end. */
                s[s1+s2] = '\0';
                sta = strdup( &s[s1] );
                s[s1+s2] = '_';
                s[s3] = '/';
                
                free(s);
                return sta;
            }
            else
            {
                sta = strdup( &s[s1] );
                s[s3] = '/';

                free( s );
                return sta;
            }
        }
    }

    if ( s1 > 0 ) s1 = '_';
    if ( s2 > 0 ) s2 = '_';
    if ( s3 > 0 ) s3 = '/';
    if ( s ) free( s );

    return NULL;
}


char *
extract_net ( char * srcname )
{
    char * net, *s = NULL;
    int len;

    s = strdup( srcname );
    len = strcspn( s, "/" );

    /* Look for the first slash.  If it comes at the very beginning, this
       source is useless to us. */
    if ( len > 0 )
    {
        /* Mark the first slash as the end of the string. */
        s[len] = '\0';

        /* Find the first underscore. */
        len = strcspn( s, "_" );
        if ( len > 0 )
        {
            /* Mark the underscore as the string end. */
            net = (char*) malloc( ( len + 1 ) * sizeof( char ) );
            strncpy( net, s, len );
            net[len] = '\0';
            free(s);

            return net;
        }
    }

    free( s );
    return NULL;
}


char *
extract_pkttype ( char * srcname )
{
    char * first_slash;
    char * last_slash;
    char * s = strdup( "" );

    first_slash = strchr( srcname, '/' );
    last_slash  = strrchr( srcname, '/' );

    /* first, ensure that there is at least two forward slashes in the string */
    if ( first_slash == last_slash ) return s;

    /* next, make sure there are no more than two forward slashes. */
    first_slash = strchr( first_slash+1 , '/' );
    if ( first_slash != last_slash ) return s;

    /* finally, extract the packet type from the last name segment. */
    free(s);
    s = strdup( last_slash + 1 );

    return s;
}

Arr *
create_new_sinfo( char * name )
{
   Arr * sinfo = newarr( strcmp );
   Pf * fields = pfnew( PFARR );
   
   fields->value.arr = newarr( strcmp );
   setarr( sinfo, ISNEW, create_pf( itoa(0), PFSTRING ) );
   setarr( sinfo, NAME, create_pf( strdup( name ), PFSTRING ) ); 
   setarr( sinfo, FIELDS, create_pf( newarr(strcmp), PFARR ) );

   return sinfo;
}

Arr *
sinfo_init ( char * pkttype, char * name, orbinfo * oi, char * priority )
{
    int i;
    Arr * sinfo = create_new_sinfo( name );
    set_sinfo_limits ( sinfo, oi, pkttype );
    if ( !priority )
        setarr( sinfo, LATENCY_ORDER, create_pf( itoa(0), PFSTRING ) );
    else
        setarr( sinfo, LATENCY_ORDER, create_pf( strdup(priority), PFSTRING ) );

    return sinfo;
}


Arr *
get_network_names ( Arr * networks, orbinfo * oi )
{
    int i;
    int size;
    Dbptr db;
    Dbptr dbs;
    char selectstr[30] = "net==\"";
    char * net;
    char * name;
    Arr* val;
    Tbl * nets;
    Dbvalue v1;
    char * netdbname = stradd( oi->dbname, ".network" );

    dbopen_table( netdbname, "r", &db );

    nets = keysarr( networks );
    size = maxtbl( nets );
    for ( i = 0; i < size; i++ )
    {
        net = poptbl( nets );

        /* We need to create the string for the select query from the net. */
        selectstr[6]='\0';
        strcat ( selectstr, net );
        strcat ( selectstr, "\"" ) ;
        selectstr[29]='\0';

        dbs = dbsubset( db, selectstr, "" );
        dbquery ( dbs, dbRECORD_COUNT, &v1 );
        if ( v1.i != 1 )
        {
            name = stradd( net, " network" );
        }
        else
        {
            dbs.record=0;
            dbgetv ( dbs, "", "netname",  &v1, 0 );
            name = strdup( v1.s );
        }

        set_fullnetname( networks, net, name );
    }

    freetbl( nets, 0 );
    dbclose ( db );
    free( netdbname );
    return networks;
}

Arr *
check_station_db ( Arr * networks, orbinfo * oi )
{
    char select[30];
    char * net;
    char * sta;
    char s[10] = "sta=="; 
    char * nchans;
    char * dbname;
    Arr* network;
    Arr* stations;
    Arr* station;
    Tbl * nets;
    Tbl * stas;
    Tbl * sortby;
    Dbvalue v1;
    Dbvalue v2;
    Pf * oldval;
    int numnets;
    int numstas;
    int size;
    int i, j;
    int totchans;
    Dbptr db;
    Dbptr dbs;
    Dbptr dbnew;
    Dbptr dbint;
    char year[5];
    char day[4];
    char * curryear;
    char * currday;
    int expected;
    char * yrday;
    char * numexp;
    Hook * hook = NULL;

    dbname = stradd( oi->dbname, ".affiliation" );
    dbopen_table( dbname, "r", &db );
    free( dbname );

    curryear = epoch2str( oi->lasttime, "%Y" );
    currday = epoch2str( oi->lasttime, "%j" );

    /* First, check that each station that has reported is stored in the
     * database.  If not, flag it as a new station. */
    nets = keysarr( networks );
    numnets = maxtbl( nets );
    for ( i = 0; i < numnets; i++ )
    {
        expected = 0;
        totchans = 0;
        net = (char*) poptbl( nets );

        stations = ( Arr* ) get_stations( networks, net );
        stas = keysarr( stations );
        numstas = maxtbl( stas );
        for ( j = 0; j < numstas ; j++ )
        {
            sta = poptbl( stas );            
            station = (Arr*) get_station ( networks, net, sta );
            /* While we're here, we'll total up the number of channels.
             * Not the best place to do it, but more efficient. */
            if ( nchans = get_station_field( station, NUMCHANS ) ) 
               totchans += atoi( nchans ); 

            sprintf ( select, "net==\"%s\"&&sta==\"%s\"", net, sta );

            dbs = dbsubset( db, select, "" );
            dbquery ( dbs, dbRECORD_COUNT, &v1 );
            if ( v1.i < 1 )
                setarr( station, ISNEW, 
                        create_pf( itoa(1), PFSTRING ));
            else
                expected++;
            dbfree( dbs );
        }
        freetbl( stas, 0 );

        /* Set the number of expected stations and number of channels for
         * the network. */
        network = get_network( networks, net );
        oldval = setarr( network, NUMSTASEXP, 
                         create_pf( itoa(expected), PFSTRING ) );
        /* if ( oldval ) recurse_free_pf( oldval ); */

        oldval = setarr( network, NUMCHANS, 
                         create_pf( itoa( totchans ), PFSTRING ) );
        /* if ( oldval ) recurse_free_pf( oldval ); */
    }
    freetbl( nets, 0 );

    sortby = newtbl(1);
    pushtbl( sortby, "ondate" );

    /* Open the site database table so we can determine which stations are
     * registered in the database. */
    dbname = stradd( oi->dbname, ".site" );
    dbopen_table( dbname, "r", &dbs );
    free( dbname );

    dbquery ( db, dbRECORD_COUNT, &v1 );
    size =  v1.i;

    /* Now, check that each station in the database has reported. */
    for ( i = 0; i < size; i++ )
    {
        db.record = i;
        if ( dbgetv( db, NULL, "net", &v1, "sta", &v2, 0 ) == dbINVALID ) 
            continue;

        net = strdup(v1.s);
        sta = strdup(v2.s);

        if ( get_station( networks, net, sta) == NULL )
        {
            /* We haven't found the db station in our station list. */
            sprintf ( select, "sta==\"%s\"", sta );
            dbint = dbsubset( dbs, select, "" );
            dbnew = dbsort( dbint, sortby, dbSORT_REVERSE, "" );
            dbfree( dbint );
            dbnew.record = 0;

            if ( dbgetv( dbnew, "", "offdate", &v1, 0 ) == dbINVALID ) 
                /* if station from db has not reported, add it as 
                 * unresponsive */
            {
                station = create_new_sinfo( sta );
                add_station( networks, net, sta, station );
            }
            else
            {
                /* if station has no offdate, it must AWOL. */
                if ( v1.i == -1 ) 
                {
                    station = create_new_sinfo( sta );
                    add_station( networks, net, sta, station );

                    /* increment the number of expected stations by one */
                    network = get_network( networks, net );
                    if ( !(numexp = (char*) getpf( network, NUMSTASEXP) ) )
                        setarr( network, NUMSTASEXP, 
                                create_pf(itoa(1), PFSTRING ) );
                    else
                    {
                        expected = atoi( numexp ) + 1;
                        free( numexp );
                        setarr( network, NUMSTASEXP, 
                                create_pf(itoa(expected), PFSTRING ) );
                    }
                } 
                else
                {
                    yrday = itoa( v1.i );

                    strncpy ( year, yrday, 4 );
                    strcpy ( day, &yrday[4] );

                    if ( ( atoi(year) > atoi(curryear) ) || 
                         ( ( atoi(year) == atoi(curryear) ) && 
                           ( atoi(day) > atoi(currday) ) ) )
                    {
                        station = create_new_sinfo( sta );
                        add_station( networks, net, sta, station );

                        /* increment the number of expected stations by one */
                        network = get_network( networks, net );
                        if ( !(numexp = (char*) getpf( network, NUMSTASEXP) ) )
                            setarr( network, NUMSTASEXP,
                                create_pf(itoa(1), PFSTRING ) );
                        else
                        {
                            expected = atoi( numexp ) + 1;
                            free( numexp );
                            setarr( network, NUMSTASEXP,
                                create_pf(itoa(expected),PFSTRING ) );
                        }
                    }
                    free( yrday );
                }

            }
            if ( dbnew.database != dbINVALID ) dbfree( dbnew );
        }
        free(net);
        free(sta);
    }

    freetbl( sortby, 0 );
    free( curryear );
    free( currday );
    dbclose( db );
    dbclose( dbs );
    return networks;
}

Arr *
get_orbnames()
{
    char pfpath[256];
    FILE * infile;
    Pf * pf;
    Arr * orbnames;

    pfread ( "orblookup.pf", &pf );
    pf->type = PFARR;
    orbnames = pf_collapse( getarr(  pf->value.arr, ORBLIST ) );
    /* recurse_free_pf( pf ); */

    return orbnames;
}

int
extract_pktinfo ( Arr * sinfo, Packet *pkt, char * pkttype, Arr * fieldsarr, 
                  Arr * disabled_fields, int numfields )
{
    int i, j, samp;
    int sum;
    int digits;

    double divby;
    double offset;
    double total;

    PktChannel * chan;
    Tbl * fields;
    Arr * entry;
    Arr * df;
    
    char * channame;
    char * chantype;
    char * ent;
    char * label;
    char scr[50];
    char result[50];

    for ( i = 0; i < pkt->nchannels; i++ )
    {
        chan = (PktChannel*) gettbl( pkt->channels, i );

        fields = (Tbl*) keysarr ( fieldsarr );
        for ( j = 0; j < numfields ; j++ )
        {
            /* Examine each field in the list. */
            label = poptbl( fields ); 

            /* Skip if this field is in the disabled list for this
               packet type. */
            if ( ( ( df = getarr( disabled_fields, pkttype ) ) != NULL ) &&
                 ( getarr( df, label ) != NULL ) ) 
                continue;

            entry = (Arr*) getarr( fieldsarr, label );
            channame = getarr( entry, CHANNAME );
            chantype = getarr( entry, CHANTYPE );

            if ( !strcmp ( channame, chan->chan ) )
            {
                sum = 0;
                for ( samp = 0; samp < chan->nsamp; samp++ )
                {
                    if ( !strcmp(chantype, ALERTCHAN) )
                        sum &= (chan->data[samp]);
                    else
                        sum += (chan->data[samp]);
                }

                ent = getarr( entry, NUMDIGITS );
                (ent!=NULL)?(digits=(int)atof(ent)):(digits=0);

                ent = getarr( entry, DIVFACTOR );
                (ent!=NULL)?(divby=atof(ent)):(divby=1);

                ent = getarr( entry, OFFSET );
                (ent!=NULL)?(offset=atof(ent)):(offset=0);

                total = (((double)sum) / chan->nsamp) / divby ;
                total += offset;

                set_station_field( sinfo, label, ftoa( total, digits ) );
            }
        }
        freetbl( fields, 0 );
    }
 
    return 1;
}

int
cache_results( char * cachefile, Arr * networks, char * orbfilename, 
               double cachetime )
{
    Pf * pf = create_pf( newarr( strcmp ), PFARR );
 
    setarr ( pf->value.arr, NETWORKS, create_pf( networks, PFARR ) );
    setarr ( pf->value.arr, TIME, create_pf( ftoa( cachetime, 3 ), 
                                                  PFSTRING ) );

    pfwrite( cachefile, pf ); 
 
    return 1;
}

Arr *
get_orbdata ( orbinfo * oi )
{
    int i, j;
    int orb = -1;
    Orbsrc * sources = NULL;
    int nsource;
    int slatest;
    int nbytes = 0;
    int bufsize = 0;
    int n = 0;
    double when;
    char * srcname;
    char * net;
    char * sta;
    int pktid = 0;
    char * pkttype;
    char * packet = (char*)malloc( PACKET_SIZE * sizeof(char) );
    char * priority;
    char * channame;
    Packet * pkt;
    PktChannel * chan;
    Arr * sinfo;
    Arr * slist = NULL;
    Arr * supported_packets; 
    Arr * fields;
    Arr * disabled;
    int numfields = cntarr( oi->fields );

    if ( (orb = orbopen( oi->name, "r" )) < 0 ) 
    {
        /* We need a more graceful, meaningful exit here. */
        exit(-1);
    }

    oi->lasttime = now();
    orbsources( orb, &when, &sources, &nsource );

    slist = newarr( strcmp );

    for ( i = 0; i < nsource; i++ ) {
        slatest = sources[i].slatest;
        srcname = sources[i].srcname;
        if ( srcname == NULL ) continue;
        
        /* Obtain the net and station from the srcname */
        if ( ( (net = extract_net( srcname )) == NULL) ||
             ( (sta = extract_sta( srcname )) == NULL ) )
            continue;

        orbget ( orb, slatest, &pktid, srcname, &when, &packet,
                 &nbytes, &bufsize );

        if ((pkt = newPkt() ) == NULL ) continue;

        unstuffPkt( srcname, when, packet, nbytes, &pkt );
        chan = (PktChannel*) gettbl( pkt->channels, 0 );
        
        if ( chan == NULL ) continue;
        pkttype = extract_pkttype( srcname );
        channame = extract_channame( srcname );
        if ( !oi->latency_order || !channame ) priority = NULL;
        else priority = getarr( oi->latency_order, channame );

        sinfo = sinfo_init( pkttype, sta, oi, priority );
        if ( getarr( oi->supported_pkts, pkttype ) != NULL )
        {
            extract_pktinfo( sinfo, pkt, pkttype, oi->fields, 
                             oi->disabled_fields, cntarr( oi->fields ) );
            set_station_field( sinfo, NUMCHANS, itoa(0) );
        }
        else
            set_station_field( sinfo, NUMCHANS, itoa(pkt->nchannels) );

        /* Set the latency.  Latency is a special field that is calculated
           from the arrival time instead of being extracted from the packet. */

        set_station_field( sinfo, LATENCY, 
                           calc_latency( oi->lasttime, chan->time ) );
        
        /* free( pkttype ); */
        /* freePkt( pkt ); */

        /* add the station to the list, and add the latency of the network. */
        /* if this is the first time we've seen this station, compare 
         * latency values against net min and max as well. */
        add_station ( slist, net, sta, sinfo );

        if ( cntarr( sinfo ) == 0 ) freearr( sinfo, 0 );

        free( net );
        free( sta );
        free( pkttype );
    }

    free( packet );

    add_latencies( slist );
    slist = get_network_names ( slist, oi );
    slist = check_station_db ( slist, oi );
    orbclose( orb );

    return slist;
}

void
orbthread( orbinfo * oi )
{
    double runtime;
    char * orbfile;
    Arr * slist;

    if ( verbosity > 0 )
        printf( "[%s] Retrieving data from orb.\n", oi->name );
    slist = get_orbdata( oi );
    if ( verbosity > 1 )
        printf( "[%s] Finished retrieving data.\n", oi->name );

    if ( verbosity > 0 )
        printf( "[%s] Storing orb data.\n", oi->name );

    cache_results( oi->cachefile, slist, oi->file, oi->lasttime );    
    freearr( slist, recurse_free_pf );

    if ( verbosity > 1 )
        printf( "[%s] Completed storing orb data.\n", oi->name );
}


/*  This function is being rewritten, since pf libraries seem to have problems
 *  with multithreading.  This function now performs the task of timing
 *  without multithreading. */
void
orbtimer( orbinfo * oi )
{
    /* thread_t t; */
    double starttime = now();
    double time_remaining;
    double minutes_in_epoch = oi->update_mins * 60 ; 

    /* thr_create( NULL, NULL, orbthread, oi, THR_BOUND, &t ); */

    while( 1 )
    {
        starttime = now();
        orbthread( oi );
        /*
        printf( "Orb %s will launch in another %d minutes.\n", oi->name, 
                oi->update_mins );
        sleep( oi->update_mins * 60 );

        if ( thr_kill( t, 0 ) == 0 )
        {
            fprintf ( stderr, "orblookup on %s is still ", oi->name );
            fprintf( stderr, "running after %s minutes.\n", oi->update_mins );
        }
        else
        {
        
            thr_create( NULL, NULL, orbthread, oi, THR_BOUND, &t ); 
        }
        */

        time_remaining = minutes_in_epoch - ( now() - starttime );
        if ( time_remaining > 0 )
        {
            if ( verbosity > 0 )
                printf( "Sleeping for %.0f seconds.\n", time_remaining );
            usleep( time_remaining * pow( 10, 6 ) );
        }
    }
}

int
main( int argc, char ** argv )
{
    Arr * orbnames;
    Arr * oiarr;
    orbinfo * oi;
    Arr * slist;
    Tbl * orblist;
    void * status;
    char * key;
    char * value;
    int numorbs;
    int i;

    /* From the command line, we need to get the level of verbosity to
     * use while running. */
    if ( argc != 2 )
    {
        fprintf( stderr, "usage: %s [orbname]\n", argv[0] );
        exit(-1);
    }

    /*
    if ( verbosity > 3 ) verbosity = 3;
    if ( verbosity < 0 ) verbosity = 0;
    */

    verbosity = 3;

    if ( verbosity > 1 )
        printf( "%s process inititated at %s for %s.\n", argv[0],
                epoch2str( now(), "%D %T" ), argv[1] );


    /* We'll set up a signal catching thread.  The purpose of this will be to
     * monitor for any external signals sent to the program.  This allows us
     * to shut down more cleanly. */
    /* ... To be implemented ... */

    /* Now, for the orbs. */
    /* First, we need to get the list of orbs we'll need to update. */
    orbnames = get_orbnames();


    /* 
    orblist = keysarr( orbnames );
    numorbs = maxtbl( orblist );
    */

    
    /*
    orbs = ( thread_t*) malloc( numorbs * sizeof( thread_t ) );
    */

    /* Now, for each orb, we need to start a thread that will launch a data
     * update for that orb on a regular interval. */
    /*
    for ( i = 0; i < numorbs; i++ )
    {
    */

    oi = (orbinfo*)malloc( sizeof( orbinfo ) );
    oi->file = strdup(argv[1]);
    oiarr = (Arr*) getarr( orbnames, oi->file );
    oi->name = getarr( oiarr, ORBNAME );
    oi->latency_order = getarr( oiarr, LATENCY_ORDER ); 

    if ( verbosity > 2 )
        printf( "[%s] Retrieved orb data from pf file.\n", oi->name);

    oi->supported_pkts = getarr( oiarr, PACKETS );
    oi->fields = (Arr*) getarr( oiarr, FIELDS );
    oi->disabled_fields = (Arr*) getarr( oiarr, DISABLED_FIELDS );
    oi->alert_stages = (Arr*) getarr( oiarr, ALERT_STAGES );
    oi->alert_ranges = (Arr*) getarr( oiarr, ALERT_RANGES );
    oi->dbname = getarr( oiarr, DBNAME );
    oi->cachefile = getarr( oiarr, "cachefile" );

    orbthread( oi );

    /*
    }

    for ( i = 0; i < numorbs; i++ )
        thr_join( orbs[i], NULL, &status );
    */

    return 0;
}
