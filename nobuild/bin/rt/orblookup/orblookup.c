#include <locale.h>
#include <ctype.h>
#include <stdlib.h>
#include <pwd.h>
#include <errno.h>
#include "coords.h"
#include "orb.h"
#include "Pkt.h"
#include "db.h"
#include "orblookup.h"


/* Simplifies the task of grabbing data stored in a pf object.  Returns
 * either an Arr*, a Tbl*, or a char*.  Caller is expected to know what is
 * being returned. */
 
void * getfrom_pfstruct( Pf * pf )
{
    if ( pf == NULL ) return NULL;
    switch( pf->type )
    {
        case PFARR:     return pf->value.arr;
        case PFTBL:     return pf->value.tbl;
        default:        return pf->value.s;
    }
}

void * getpf( Arr * arr, char * index )
{
    return ( getfrom_pfstruct( (Pf*) getarr( arr, index ) ) );
}

Arr * get_station_fields( Arr * sinfo )
{
    Pf * fields = (Pf*) getarr( sinfo, FIELDS );
    return ( fields->value.arr );
}

/* converts a floating-point number into a string with the given number of
 * decimal places.  Caller must free the allocated string. */
char * ftoa ( double d, int precision )
{
    char todec[21];
    char num[20];
    sprintf ( todec, "%%.%df", precision );    
    sprintf ( num, todec, d );
    return ( strdup( num ) );
}

/* converts an integer into a string.  Caller must free the allocated
 * string.*/
char * itoa ( int i )
{
    return ftoa( (double)i, 0 );
}

/* creates a new string by concatenating two strings s1 and s2 together.
 * Unlike strcat, does not alter the original string.  Returns a
 * freshly-allocated char* that must be freed by the caller. */
char * stradd( char * s1, char * s2 )
{
    char * ns;
    int len;

    if ( !s1 || !s2 ) return NULL;
    len = strlen( s1 ) + strlen( s2 ) + 1;
    ns = (char*) malloc ( len * sizeof(char) );
    sprintf( ns, "%s%s", s1, s2 );
    return strdup(ns);
}

char * get_station_field( Arr * sinfo, char * name )
{
    Pf * pf;
    if ( !(pf = (Pf*) getarr( get_station_fields( sinfo ), name ) ) )
        return NULL;
    return ( strdup(pf->value.s) );
}

int set_station_field( Arr * sinfo, char * name, char * value )
{
    Pf * oldval;
    Pf * pf = pfnew( PFSTRING );
    pf->value.s = value;

    oldval = setarr( get_station_fields( sinfo ), name, pf );
    /*    if ( oldval != NULL ) pffree( oldval ); */

    return 1;
}

Pf * create_pf ( void * value, int type )
{
    Pf * pf = pfnew( type );

    switch ( type )
    {
        case PFARR :  pf->value.arr = (Arr*) value;
                      break;
        case PFTBL :  pf->value.tbl = (Tbl*) value;
                      break;
        default:        pf->value.s = strdup((char *) value);
    }
    return pf;
}

char * secs_to_readable( char * secs )
{
    long t, abst;
    char * ep;
    char buff[80];
    long divby;

    t = (int) strtol( secs, &ep, 10 );
    if ( ( t == 0 ) && ( EINVAL ) ) return "";

    abst = abs(t);
    
    if ( abst >= 86400  )
    {
        t /= 86400;
        if ( t == 1 ) sprintf ( buff, "%d day", t );
        else sprintf ( buff, "%d days", t );
    }
    else if ( abst >= 3600 )
    {
        t /= 3600;
        if ( t == 1 ) sprintf ( buff, "%d hour", t );
        else sprintf ( buff, "%d hours", t );
    }
    else if ( abst >= 60 )
    {
        t /= 60;
        if ( t == 1 ) sprintf ( buff, "%d min", t );
        else sprintf ( buff, "%d mins", t );
    }
    else
    {
        if ( t == 1 ) sprintf ( buff, "%d sec", t );
        else sprintf ( buff, "%d secs", t );
    }
    
    return ( strdup( buff ) );
}


int is_station_new( Arr * sinfo )
{
    Pf * isnew = getarr( sinfo, ISNEW );
    if ( !strcmp( isnew->value.s, ONESTR ) ) return 1;
    return 0;
}

Arr * get_network( Arr * networks, char * net ) 
{
    Pf * network;
    if ( networks == NULL ) return NULL;
    if ( !(network = getarr( networks, net ) ) )return NULL;
    return ( network->value.arr );
}

Arr * get_stations ( Arr * networks, char * net )
{
    Arr * network;
    Pf * stations;

    if ( !(network = get_network( networks, net ) ) ) return NULL;
    if ( !(stations = getarr( network, STALIST ) ) )return NULL;
    return ( stations->value.arr );
}

int add_network ( Arr * networks, char * net )
{
    Arr * network = get_stations( networks, net );
    if ( network ) return NULL;
    
    network = newarr( strcmp );
    setarr( network, STALIST, create_pf( newarr( strcmp ), PFARR) );
    setarr( networks, net, create_pf(network, PFARR) );
    
    return 1;
}

Arr * get_station ( Arr * networks, char * net, char * sta )
{
    Arr * stations;
    Pf * station;
    if ( !(stations = get_stations( networks, net ) ) ) return NULL;
    if ( !(station = (Pf*) getarr( stations, sta ) ) ) return NULL;
    return ( station->value.arr );
}

int set_fullnetname( Arr * networks, char * net, char * name )
{
   Arr * network = get_network( networks, net );
   setarr( network, FULLNET, create_pf( name, PFSTRING ) );
   return 1;
}

char * get_fullnetname ( Arr * networks, char * net )
{
   Arr * network;
   Pf * fullnet;
   if ( !(network = get_network ( networks, net ) ) ) return NBSP;
   if ( !(fullnet = getarr( network, FULLNET ) ) ) return NBSP;
   
   return ( fullnet->value.s );
}

char * get_min_net_latency ( Arr * networks, char * net )
{
    Pf * network;
    if ( !(network = getarr( networks, net ) ) ) return 0;
    return ( ((Pf*)getarr( network->value.arr, MINNETLATENCY ))->value.s );
}

int comp_double ( char ** v1, char ** v2, void * private )
{
    double d1 = atof( *v1 );
    double d2 = atof( *v2 );

    if ( d1 == d2 ) return 0;
    if ( d2 > d1 ) return 1;
    return -1;
}

char * get_median_net_latency ( Arr * networks, char * net )
{
    Pf * network;
    Pf * stations;
    Tbl * latencies;
    Tbl * stas;
    Pf * station;
    char * staname;
    char * latency;
    int numstas;
    int i;

    /* get the number of stations to calculate for. */
    if ( !(network = (Pf*) getarr( networks, net ) ) ) return 0;
    if ( !(stations = (Pf*)getarr( network->value.arr, STALIST ))) return 0;
    numstas = cntarr( stations->value.arr );
    latencies = newtbl( numstas );

    /* Calculate the median value for the given station list. */
    stas = keysarr( stations->value.arr );
    for ( i = 0; i < numstas; i++ )
    {
        staname = gettbl( stas, i );
        station = getarr( stations->value.arr, staname );
        /* if it exists, push the current latency onto the list. */
        if ( latency = get_station_field( station->value.arr, LATENCY ) )
             pushtbl( latencies, strdup( latency ) );
    }
    /* Sort the list of latencies. */
    numstas = maxtbl( latencies );
    sorttbl( latencies, comp_double, NULL );
    latency = strdup( gettbl( latencies, numstas/2 ) );
    freetbl( latencies, free );
    return (latency);
}

char * get_max_net_latency ( Arr * networks, char * net )
{
    Pf * network;
    if ( !(network = getarr( networks, net ) ) ) return 0;
    return ( ((Pf*)getarr( network->value.arr, MAXNETLATENCY ))->value.s );
}

int check_field_bounds ( double value, Pf * field_threshold )
{
   Pf * high;
   Pf * low;

   if ( value == BADVAL ) return 0;
   if ( field_threshold == NULL ) return 0;
   high = (Pf*) getarr( field_threshold->value.arr, HIGH );
   low = (Pf*) getarr( field_threshold->value.arr, LOW );
   
   if ( ( value >= atof( high->value.s ) ) || ( value <= atof( low->value.s ) ) )
       return 1;
   
   return 0;
}

/* Checks each threshold for a violation and returns the highest priority
   alert that was triggered. */

int check_field_thresholds ( Arr * sinfo, char * name )
{
    int j;
    double value;
    int max = 0;
    int curr = 0;
    int numlevels;
    Tbl * alerts;
    Pf * level;
    Pf * field;
    char * f;
    char * p;


    if (  (f = get_station_field( sinfo, name )) == NULL ) return 0;

    /* value is the value held in the named field for this station. */
    value = atof( f );

    /* alerts is the list of thresholds for this station. */
    alerts = valsarr( ((Pf*)getarr( sinfo, THRESHOLD ))->value.arr );

    /* Iterate through each alert level. */
    numlevels = maxtbl( alerts );
    for ( j = 0; j < numlevels; j++ )
    {
        level = (Pf*) gettbl( alerts, j );

        /* Check the bounds. If value lies outside and this is the highest
           priority alert broken, set this as the current alert level. */
        if ( !(field = (Pf*)getarr( level->value.arr, name ) ) ) return -1;
        if ( !(p = pfget_string( field, PRIORITY ) ) ) return -1;

        curr = atoi( p );
        if ( check_field_bounds( value, field ) )
            if ( curr > max )
                max = curr;
    }

    return max;
}


void create_alertlist ( char * key, char * value, void * private )
{
    Arr * args = (Arr*) private;
    Arr * list = (Arr*) getarr( args, "list" );
    AlertLevelFP fp = (AlertLevelFP) getarr( args, "alerttest" );
    
    if ( fp( ((Pf*)value)->value.arr ) > 0 ) setarr( list, key, value );
}

char * extract_sta ( char * srcname )
{
    char * sta, *s = NULL;
    int len;
    int end;

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
            /* Find the second underscore. */
            end = strcspn( &s[++len], "_" );

            if ( end > 0 )
            {
                /* Mark the second underscore, if it exists, as the end. */
                s[len+end] = '\0';
                sta = strdup( &s[len] );
                free(s);

                return sta;
            }
        }
    }

    free( s );
    return NULL;
}


char * extract_net ( char * srcname )
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
            s[len] = '\0';
            net = strdup( s );
            free(s);

            return net;
        }
    }

    free( s );
    return NULL;
}


void * remove_pf_instances ( Pf * pf )
{
    int i;
    int size;
    Tbl * keys;
    char * key;
    Pf * value;
    Pf * root = pf;
    
    switch ( pf->type )
    {
        case PFARR: size = cntarr( pf->value.arr );
                    keys = keysarr( pf->value.arr );
                    for ( i = 0; i < size; i++ )
                    {
                        key = gettbl( keys, i ); 
                        value = getarr( pf->value.arr, key );
                        setarr( pf->value.arr, key,
                                remove_pf_instances( value ) );
                    }
                    return (void*)(pf->value.arr);
                    
        case PFTBL: size = maxtbl( pf->value.tbl );
                    for ( i = 0; i < size; i++ )
                    {
                        value = gettbl( pf->value.tbl, i );
                        settbl( pf->value.tbl, i,
                                remove_pf_instances( value ));
                    }
                    return (void*)pf->value.tbl;
        default:    return (void*)pf->value.s;
    }
}

int read_orbinfo( char * pffile, char * orbname, char ** cachefile )
{
    Pf * pf = NULL;
    Pf * tmpl = NULL;
    char pfpath[256];
     
    pfread( pffile, &pf );
    if ( pf == NULL )
        elog_die( 1, "Could not find parameter file %s", pffile );

    pfget( pf, "html_template", (void**) &tmpl );
    html_template = (Arr*) remove_pf_instances( tmpl );
    pfget( pf, "styles", (void**) &tmpl );
    styles = tmpl->value.arr;
    orblist = getpf( pf->value.arr, ORBLIST );
    if ( !orblist ) 
        elog_die( 1, "Badly formatted parameter file %s: missing %s", 
                  pffile, ORBLIST );
    orbinfo = getpf( orblist, orbname );
    if ( !orbinfo ) 
        elog_die( 1, "Badly formatted parameter file %s: missing entry for %s", 
                  pffile, orbname );

    *cachefile = getpf( orbinfo, CACHEFILE );
    if ( *cachefile == NULL )
        elog_die( 1, "Badly formatted parameter file %s: missing %s", 
                  pffile, CACHEFILE );

    return 1;
}

int generate_system_error_page ()
{
    char title[21];
    char body[110];

    strcpy( title, "" );
    strcat ( title, "Error on Host System" );

    strcpy( body, "" );
    strcat ( body, "This system encountered techincal difficulties while " );
    strcat ( body, "processing\n your request. Please try your request again.");

    generate_message_page ( title, body );

    return 1;
}

int generate_error_page ()
{
    char header[20];
    char body[74];

    strcpy( header, "" );
    strcpy( body, "" );

    strcat ( header, "Error Accessing ORB" );

    strcat ( body, "An error occurred while trying to access " );
    strcat ( body, "the ORB. Please try again later." );

    generate_message_page ( header, body );

    return 1;
}


int generate_entry_page ()
{
    char title[1024];
    char body [4096];
    char * orbname;
    char * pffile;
    char * filename;
    char * newfile;
    char * server;
    char pfpath[256];
    Arr * pflist;
    Tbl * list;
    Pf * pf = NULL;
    FILE * infile;
    int i;
    int len = 0;
    int size;

    strcpy ( title, "ORB real-time status lookup page" );
    filename = getenv( "SCRIPT_NAME" );
    server = getenv( "SERVER_NAME" ); 

    if ( ( !filename || !server ) )
    {
        generate_system_error_page();
        return 0;
    }
    else
    {
        filename = strdup( filename );
        server = strdup( server );
    }

    len += sprintf ( &body[len], "<TABLE border=0 align=center cellpadding=5");
    len += sprintf ( &body[len], " cellspacing=0>\n");
    len += sprintf ( &body[len], "  <TR bgcolor=gray>\n" );
    len += sprintf ( &body[len], "    <TH>Active ORBs:</TH>\n" );
    len += sprintf ( &body[len], "  </TR>\n" );
    
    sprintf ( pfpath, "orblookup.pf" );
    infile = fopen( pfpath, "r" );
    if ( infile ) 
       pfin ( infile, &pf );

    if ( pf == NULL )
    {
        generate_system_error_page();
        return 0;
    }

    pflist = (Arr*) getpf( pf->value.arr, ORBLIST );
    if ( pflist == NULL )
    {
        generate_system_error_page();
        return 0;
    }

    list = keysarr( pflist );
    size = maxtbl( list );
    for ( i=0; i < size; i++ )
    {
        orbname = gettbl( list, i );
        newfile = (char*) malloc( ( strlen(server) + strlen(filename) +
                                    strlen(orbname) + 24 ) 
                                  * sizeof( char ) );
        sprintf ( newfile, "http://%s%s?orbname=%s&all=on", server, filename, 
                                                            orbname );
        len += sprintf ( &body[len], "  <TR bgcolor=silver>\n" );
        len += sprintf ( &body[len], "    <TD><A HREF=\"%s\">%s</A></TD>\n",
                        newfile, orbname );
        len += sprintf ( &body[len], "  </TR>\n" );
    }

    len += sprintf ( &body[len], "</TABLE>\n" );
    fclose( infile );

    generate_message_page( title, body );
    return 1;
}


int generate_message_page ( char * title, char * body )
{
    printf ( "Content-type: text/html\n\n" );
    printf ( "<HTML>\n" );
    printf ( "<HEAD>\n" );
    printf ( "  <TITLE>%s</TITLE>\n", title );
    printf ( "</HEAD>\n" );
    printf ( "<BODY>\n" );
    printf ( "  <H1 align=center>%s</H1>\n", title );
    printf ( "  <P>%s</P>\n</BODY>\n", body );
    printf ( "</HTML>\n" );
    return 1;
}


char * get_field_in_position ( int order )
{
    Arr * fieldarr = getpf( orbinfo, FIELDS );
    Arr * field;
    Tbl * fields;
    char * fieldname;
    char * fieldorder;
    int i = 0;
    int numfields = atoi( getpf( orbinfo, NUMFIELDS ) );
        
    fields = keysarr( fieldarr );
    for ( i = 0; i < numfields; i++ )
    {
        fieldname = gettbl( fields, i );
        field = (Arr*) getpf( fieldarr, fieldname );
        fieldorder = (char*) getpf( field, ORDER );
        if ( (fieldorder != NULL) && (order == atoi(fieldorder) ) )
             return fieldname;

    }
    return NULL;
}

int field_list_compare( void * e1, void * e2 )
{
    char * f1, *f2;
    Arr * fieldsarr = getpf( orbinfo, FIELDS );
    Arr * field;
    int i1, i2;

    if ( (e1 == NULL) || (e2 == NULL) ) return -2;

    field = (Arr*) getpf( fieldsarr, *((char**) e1) );
    if ( field == NULL ) return -2;
    f1 = getarr( field, ORDER );
    if ( f1 == NULL ) return -2;
    i1 = atoi( f1 );

    field = (Arr*) getpf( fieldsarr, *((char**) e2) );
    if ( field == NULL ) return -2;
    f2 = getarr( field, ORDER );
    if ( f2 == NULL ) return -2;
    i2 = atoi( f2 );
    
    if ( i1 == i2 ) return 0;
    if ( i1 < i2 ) return -1;

    return 1;
}

int get_comment_start( char * s )
{
   return ( (int) strstr( s, "<!--" ) - (int) s );
}

int get_comment_end( char * s )
{
   int start = get_comment_start( s );
   return ( (int) strstr( &s[(start +4)], "-->" ) - (int)s  + 3);
}


int set_varname( Arr * tinfo, char * name, char * value )
{
    Arr * values;
    Arr * vallist;
    
    if ( tinfo == NULL ) return 0;
    if ( (values = (Arr*) getarr( tinfo, VARS )) == NULL ) return 0;
    if ( (vallist = (Arr*) getarr( values, name )) == NULL ) return 0;
    setarr( vallist, NAME, value );
}


char * get_varname( Arr * tinfo, char * name )
{
    Arr * vars;
    Arr * vallist;
    
    if ( (vars = (Arr*) getarr( tinfo, VARS )) == NULL ) return NULL;
    if ( (vallist = (Arr*) getarr( vars, name )) == NULL ) return NULL;

    return ( getarr( vallist, NAME ) );
}

Arr * get_varlist ( Arr * tinfo, char * name )
{
    Arr * vars;
    Arr * blocklist;
    Arr * list;
    TagListFP fp;

    if ( !(vars = (Arr*) getarr( tinfo, VARS ) ) ) return NULL;
    if ( !(blocklist = (Arr*) getarr( vars, name ) ) ) return NULL;

    if ( !(fp = (TagListFP) getarr( blocklist, LIST ) ) ) return NULL;
    list = fp( tinfo );
    return list;
}

TagValFP * get_varfp ( Arr * tinfo, char * name )
{
    Arr * vars;
    Arr * blocklist;
    char* (*fp)();

    if ( !(vars = (Arr*) getarr( tinfo, VARS ) ) ) return NULL;
    if ( !(blocklist = (Arr*) getarr( vars, name ) ) ) return NULL;

    return ( (TagValFP *) getarr( blocklist, FP ) );
}

int print_insert_indent ( FILE * f, char * s, int indent )
{
    char * spacer = (char*) malloc ( (indent+1) * sizeof( int ) );
    char * curr;
    char * buff;
    int eol = 0;
    int start = 0;
    int size = strlen( s ); 
    int i;

    for ( i = 0; i < indent; i++ )
        spacer[i] = ' ';
    spacer[i] = '\0';
    
    /* Print out each line, but also indent the next line. */
    while( ( ( start < size ) && (eol=strcspn( &s[start], "\n" ) ) > -1 ) )
    {
        buff = (char *) malloc ( (eol+1) * sizeof( char ) );
        strncpy( buff, &s[start], eol );
        buff[eol] = '\0';

        eol += start;
        fprintf ( f, "%s", buff );
        free ( buff );
        if ( eol < size )
           fprintf ( f, "\n%s", spacer );
        start = (eol + 1);
    }

    return 1;
}

Arr* get_stalist ( Arr * tinfo ) 
{ 
    Arr * slist;
    Arr * newlist;
    Arr * args;
    char * net;
   
    if ( !(slist = (Arr*) getarr( tinfo, SLIST ) ) ) return NULL;
    if ( !(net = get_varname( tinfo, NET ) ) ) return NULL;
    slist = get_stations( slist, net );
    if (!alertmode) return ( slist );

    args = newarr( strcmp );
    newlist = newarr( strcmp ); 
    setarr( args, "list", newlist );
    setarr( args, "alerttest", (void*) get_staclasslevel );
    applyarr( slist, create_alertlist, args );

    return ( (Arr*) getarr( args, "list" ) );
}

Arr * get_orblist ( Arr * tinfo ) { return orblist; }

Arr * get_tinfo_station ( Arr * tinfo )
{
    char * sta = get_varname( tinfo, STA );
    Arr * stalist = get_varlist( tinfo, STA );
    
    if ( !sta || !stalist ) return NULL;
    return (((Pf*)getarr( stalist, sta ))->value.arr );
}

char * get_ordered_value ( Arr * tinfo )
{
    char * current;
    Arr * list;

    if ( !( current = getarr( tinfo, CURRENT ) ) ) return strdup( "" );
    if ( !( list = get_varlist( tinfo, current ) ) ) return strdup( "" );

    return ( getarr( list, get_varname( tinfo, current ) ) );
}


Arr* get_netlist ( Arr * tinfo ) 
{ 
    Arr * netlist;
    Arr * newlist;
    Arr * args;
   
    netlist = (Arr*) getarr( tinfo, SLIST );
    if (!alertmode) return ( netlist );
     
    args = newarr( strcmp );
    newlist = newarr( strcmp ); 
    setarr( args, "list", newlist );
    setarr( args, "alerttest", (void*) get_netclasslevel );
    applyarr( netlist, create_alertlist, args );

    return ( (Arr*) getarr( args, "list" ) );
}

Arr* get_used_fieldlist ( Arr * tinfo )
{
    return ( getarr( tinfo, FIELD_LIST ) );
}

Arr* get_fieldlist ( Arr * tinfo )
{
    return getpf( orbinfo, FIELDS );
}

Arr* get_alertlist ( Arr * tinfo )
{
    return getpf( orbinfo, ALERT_STAGES );
}

char* get_orblookup_orbname ( Arr * tinfo )
{
    return getpf( orbinfo, ORBNAME );
}

char * get_encoded_net ( Arr * tinfo )
{
    char * net;
    if ( !(net = get_varname ( tinfo, NET ) ) ) return strdup("");
    return ( encode_uri( net, strlen( net ) ) );
}
char * get_encoded_netsta ( Arr * tinfo )
{
    char * net;
    char * sta;
    char * netsta;
    char * encoded;

    if ( !(net = get_varname ( tinfo, NET ) ) ) return strdup("");
    if ( !(sta = get_varname ( tinfo, STA ) ) ) return strdup("");
    netsta = stradd( net, sta );
    encoded = ( encode_uri( netsta, strlen( netsta ) ) );
    free( netsta );

    return encoded;
}

char * get_row_seperator ( Arr * tinfo ) 
{ 
    char * spacer;
    char * buff;
    char * num_stations;
    int i;
    int index;
    int len;
    int indent;
    int numstas;

    if ( !(spacer = getarr( tinfo, INDENT ) ) ) spacer = "0";
    indent = atoi( spacer );
    spacer = (char*) malloc ( indent * sizeof(char) );

    for ( i = 0; i < indent; i++ )
        spacer[i] = ' ';
    spacer[i] = '\0';

    index = atoi(getarr( tinfo, INDEX ) );
   
    if ( ( (index+1) % ( get_numcols() ) ) == 0 )
    {
        buff = (char*) malloc ( (indent + 11 ) * sizeof(char) );
        buff[0] = '\0';
        sprintf ( buff, "</TR>\n%s<TR>", spacer );
        if ( !(num_stations = getarr( tinfo, MAXINDEX ) ) ) return strdup("");
        if ( (index+1) == atoi(num_stations) ) buff[5] = '\0';
        return (buff);
    }

    /* If this is the last entry, then we need to print out some blank
       cells to finish off the row. */

    numstas = cntarr( get_varlist( tinfo, STA ) );
    if ( index == ( numstas - 1 ) )
    {
        index =  get_numcols() - ( (numstas-1) % get_numcols() ) - 1;
        if ( index < 1 ) return (strdup(""));

        buff = (char*) malloc ( (1 + ((indent + 18) * index)) * sizeof(char) );
        buff[0] = '\0';
        len = 0;

        for ( i = 0; i < (index-1); i++ )
            len += sprintf( &buff[len], "  <TD>&nbsp;</TD>\n%s", spacer );
        len += sprintf( &buff[len], "  <TD>&nbsp;</TD>\n" );
        return ( buff );
    }

    return strdup (""); 
} 

char * get_staclass( Arr * tinfo )
{
    Arr * list;
    char * sta;
    char * net;
    Arr * sinfo;

    sta = get_varname ( tinfo, STA );
    net = get_varname ( tinfo, NET );
    list = get_netlist ( tinfo );

    if ( !(sinfo = get_station ( list, net, sta ) ) )
        return ( strdup( "" ) );

    return select_groupstyle( sinfo );
}

int get_netclasslevel( Arr * network )
{
    int i, j;
    int numstas;
    int numfields;
    int curr = 0;
    int max = 0;
    Tbl * stas;
    Arr * sinfo;
    Pf * stations;
    char * level;
    char * sta;

    if ( level = getarr( network, "alertlevel" ) )
        return ( atoi( level ) );

    if ( !(stations = (Pf*) getarr( network, STALIST ))) return 0;
    stas = keysarr( stations->value.arr );
    numstas = maxtbl( stas );
    for ( i = 0; i < numstas; i++ )
    {
        sta = gettbl( stas, i );
        sinfo = ((Pf*)getarr( stations->value.arr, sta ))->value.arr;
        curr = get_staclasslevel( sinfo );
        if ( curr > max ) max = curr;
    }

    setarr( network, "alertlevel", itoa( max ) );

    return ( max );
}

char * get_netclass( Arr * tinfo )
{
    Arr * netlist = get_varlist( tinfo, NET );
    char * net = get_varname( tinfo, NET );
    int level = get_netclasslevel( ((Pf*) getarr( netlist, net ))->value.arr );

    return get_alert_name( level );
}

char * get_modclass( Arr * tinfo )
{
    char * size = getarr( tinfo, INDEX );
    if ( !size ) return ( strdup("") );
    if ( atoi(size) % 2 ) return EVENROW;
    else return ODDROW;
}

char * get_script_name ( Arr * tinfo ) 
{
    return strdup( getenv("SCRIPT_NAME") );
}


char * get_fieldstyle( Arr * tinfo )
{
    Arr * sinfo;
    int field_limits = 0;

    if ( !(sinfo = get_tinfo_station( tinfo ) ) )
        return ( strdup("") );

    field_limits = check_field_thresholds( sinfo, get_ordered_value(tinfo) );

    if ( ( field_limits == 0 ) && ( is_station_new( sinfo ) ) )
        return ( getpf( orbinfo, NEWSTA ) );
    else
        return ( get_alert_name( field_limits ) );
}

char * get_fieldalign( Arr * tinfo )
{
    char * field;
    char * type;
    Arr * fieldinfo;
    Arr * fieldlist;

    fieldlist = get_varlist( tinfo, USED_FIELD );
    field = getarr( fieldlist, get_varname( tinfo, USED_FIELD ) );
    fieldinfo = (Arr*) getpf( getpf( orbinfo, FIELDS ), field );

    if ( !(type = getarr( fieldinfo, CHANTYPE ) ) )
        return strdup( "" );

    if ( !strcmp( type, ALERT ) )
        return strdup( "center" );

    return strdup( "right" );
}

char * get_fieldvalue( Arr * tinfo )
{
    Arr * fieldlist;
    char * field;
    char * value;
    char s[20];
    char * sta;
    char * units;
    Arr * finfo;
    Arr * stations;
    Arr * sinfo;

    field = getarr( get_varlist( tinfo, USED_FIELD ),
                    get_varname( tinfo, USED_FIELD ) );
    if ( !(sinfo = get_tinfo_station( tinfo ) ) )
        return NBSP;
    if ( !(value = get_station_field( sinfo, field ) ) ) 
        return NBSP;
    finfo = (Arr*) getpf( get_fieldlist(tinfo), field );

    if ( strcmp( getpf( finfo, CHANTYPE), ALERTCHAN ) == 0 )
    {
        if ( atof(value) > 0 )
            sprintf ( s, "on" );
        else
            sprintf ( s, "off" );
    }
    else
    {
        units = getpf( finfo, UNITS );
        if ( !units )
            sprintf( s, "%s", value, units );
        else
            if ( strcmp( units, "time" ) == 0 )
                sprintf ( s, "%s", secs_to_readable( value ) );
            else
                sprintf( s, "%s %s", value, units );

    }

    if ( !strcmp( s, "" ) ) return NBSP;
    return strdup(s);
}

char * get_tinfo_fullnetname ( Arr * tinfo )
{
    Arr * list = get_varlist( tinfo, NET );
    char * name = get_varname( tinfo, NET );
    return ( get_fullnetname( list, name ) );
}

char * create_url ( char * server, char * filename, Arr * query )
{
    int i;
    Tbl * list = keysarr( query );
    int size = maxtbl( list );
    char s[1024];
    int current = 0;
    char * key; 
    char * value;

    s[0] = '\0';

    current += sprintf( &s[current], "http://%s%s", server, filename );
    for( i = 0; i < size; i ++ )
    {
        key = gettbl( list, i );
        if ( i == 0 )
            current += sprintf( &s[current], "?%s", key );
        else
            current += sprintf( &s[current], "&%s", key );

        if ( value = getarr( query, key ) )
            current += sprintf( &s[current], "=%s", value );
    }
    return strdup(s);
}


Arr * fields_to_query( Arr * fieldlist )
{
    Tbl * fields = valsarr( fieldlist );
    Arr * query = newarr( strcmp );
    char * field;
    int i;
    int size = maxtbl( fields );

    for ( i = 0; i < size; i++ )
    {
        field = gettbl( fields, i );
        field = encode_uri( field, strlen(field) );
        setarr( query, field, "on" );
        /* free( field ); */
    }
    return query;
}

char * get_neturl( Arr * tinfo )
{
    char * server = getenv( "SERVER_NAME" );
    char * filename = getenv ( "SCRIPT_NAME" );
    char * net = get_varname( tinfo, NET );
    char * orb = getpf( orbinfo, ORBNAME );
    Arr * query = newarr( strcmp );

    setarr( query, "all", "on" );
    if ( orb ) setarr( query, "orbname", encode_uri( orb, strlen(orb) ) );
    if ( net ) setarr( query, "net", net );

    return ( create_url( server, filename, query ) );
}

char * get_orburl( Arr * tinfo )
{
    char * server = getenv( "SERVER_NAME" );
    char * filename = getenv ( "SCRIPT_NAME" );
    char * orb = get_varname( tinfo, ORB );

    Arr * query = newarr( strcmp );
    setarr( query, "orbname", orb );

    return ( create_url( server, filename, query ) );
}

char * get_alertmode( Arr * tinfo )
{
    if ( alertmode ) return ( " CHECKED" );
    return ("");
}

char * get_tinfo_numcols( Arr * tinfo )
{
    return ( (char*) getpf( orbinfo, NUMCOLS ) );
}

int get_numcols( )
{
    return atoi( get_tinfo_numcols( NULL ) );
}

char * get_num_used_fields ( Arr * tinfo )
{
    Arr * field_list = (Arr*) getarr( tinfo, FIELD_LIST );
    return ( itoa( cntarr( field_list ) + 1 ) );
}

char * get_num_fields ( Arr * tinfo )
{
    return ( getpf( orbinfo, NUMFIELDS ) );
}

char * get_run_time ( Arr * tinfo )
{
   return ( epoch2str( cachetime, "%B %d %Y %H:%M UTC" ) );
}

char * get_percentwidth( Arr * tinfo )
{
    return ( itoa( 100/get_numcols() ) );
}

char * get_numnets ( Arr * tinfo )
{
    Arr * networks = get_netlist( tinfo );
    return ( itoa( cntarr( networks ) ) );
}

char * get_numstas ( Arr * tinfo )
{
    Arr * net = get_varlist ( tinfo, STA );
    if ( (net == NULL ) || (cntarr( net ) == 0) ) return strdup("0");
    return ( itoa( cntarr(net) ) );
}

char * get_numstas_total( Arr * tinfo )
{
    Arr * networks = (Arr*) getarr( tinfo, SLIST );
    Tbl * nets;
    Arr * stations;
    char * net;
    int i;
    int size;
    int total = 0;

    nets = keysarr( networks );
    size = maxtbl( nets );
    for ( i = 0; i < size; i++ )
    {
        net = poptbl( nets );
        stations = get_stations( networks, net );
        if ( stations == NULL ) continue;
        total += cntarr( stations );
    }
    freetbl( nets, 0 );

    return ( itoa( total ) );
}

char * get_numstas_expected( Arr * tinfo )
{
    Arr * networks = (Arr*) getarr( tinfo, SLIST );
    char * net = get_varname ( tinfo, NET );
    Arr * network = get_network( networks, net );
    Pf * numexp;
    if ( !( numexp = (Pf*) getarr( network, NUMSTASEXP ) ) ) return strdup("0");
    return (char*) getfrom_pfstruct( numexp );
}

char * get_tinfo_numchans( Arr * tinfo )
{
    Arr * networks = (Arr*) getarr( tinfo, SLIST );
    char * net = get_varname( tinfo, NET ) ;
    Arr * network = get_network( networks, net );
    Pf * numchans;
    if ( !( numchans = (Pf*) getarr( network, NUMCHANS ) ) ) return strdup("0");
    return (char*) getfrom_pfstruct( numchans );
}

char * get_tinfo_numchans_total( Arr * tinfo )
{
    Arr * netlist;
    Arr * networks = (Arr*) getarr( tinfo, SLIST );
    Arr * network;
    Tbl * nets;
    char * net;
    char * numchans;
    int totchans = 0;
    int i;
    int numnets;

    nets = keysarr( networks );
    numnets = maxtbl( nets );

    for ( i = 0; i < numnets; i++ )
    {
        net = poptbl( nets );
        network = getpf( networks, net );

        if ( !( numchans =  getpf( network, NUMCHANS ) ) ) 
            continue;

        totchans += atoi( numchans );
    }
    freetbl( nets, 0 );

    return itoa( totchans );
}

char * get_tinfo_min_latency( Arr * tinfo )
{
    Arr * net = get_varlist( tinfo, NET );
    char * name = get_varname( tinfo, NET );
    char * latency;

    if (latency = get_min_net_latency( net, name ) )
        return ( secs_to_readable( latency ) );
    else return NBSP;
}

char * get_tinfo_median_latency( Arr * tinfo )
{
    Arr * net = get_varlist( tinfo, NET );
    char * name = get_varname( tinfo, NET );
    char * latency;

    if ( latency = get_median_net_latency( net, name ) )
        return ( secs_to_readable( latency ) );
    else return NBSP;
}

char * get_tinfo_max_latency( Arr * tinfo )
{
    Arr * net = get_varlist( tinfo, NET );
    char * name = get_varname( tinfo, NET );
    char * latency;

    if (latency = get_max_net_latency( net, name ) )
        return ( secs_to_readable( latency ) );
    else return NBSP;
}

char * get_field_checked( Arr * tinfo )
{
    char * field;
    Tbl *field_list;
    int i, size, checked;

    field = get_varname( tinfo, ALL_FIELD );
    field_list = valsarr( get_varlist( tinfo, USED_FIELD ) );

    size = maxtbl( field_list );
    for ( i = 0; i < size; i++ )
    {
        if ( !strcmp( field, gettbl( field_list, i ) ) )
        {
           freetbl( field_list, 0 );
           return ( strdup(" CHECKED" ) );
        }
    }

    freetbl( field_list, 0 );
    return ( strdup( "" ) );
}

char * get_current_value ( Arr * tinfo )
{
    char * name;
    if ( !(name = getarr( tinfo, CURRENT ) ) ) return NBSP;
    return ( get_varname( tinfo, name ) );
}

char * get_net_value( Arr * tinfo )
{
    char * name = get_varname( tinfo, NET );
    if ( !name ) return NBSP;
    else return name;
}

char * get_alertclass( Arr * tinfo )
{
    Arr * alert;
    Arr * list;
    char * alertname;
    char * style;

    if ( !(list = get_alertlist( tinfo ) ) )
        return ( strdup("") );

    if ( !(alertname = get_varname( tinfo, ALERT_STAGE ) ) )
        return ( strdup("") );
    
    if ( !(alert = getpf( list, alertname ) ) )
        return ( strdup("") );

    if ( !(style = getpf( alert, STYLE ) ) )
        return ( strdup("") );

    return style;
}

char * get_alertdesc( Arr * tinfo )
{
    Arr * alert;
    Arr * list;
    char * alertname;
    char * desc;

    if ( !(list = get_varlist( tinfo, ALERT_STAGE ) ) )
        return ( strdup("") );

    if ( !(alertname = get_varname( tinfo, ALERT_STAGE ) ) )
        return ( strdup("") );
    
    if ( !(alert = getpf( list, alertname ) ) )
        return ( strdup("") );

    if ( !(desc = getpf( alert, "description" ) ) )
        return ( strdup("") );

    return desc;
}

int add_var( Arr* varlist, char * name, TagListFP list, TagValFP fp )
{
    Arr * values = newarr( strcmp );

    if ( varlist == NULL ) return 0;

    setarr( values, LIST, (void*)list );
    setarr( values, FP, (void*)fp );
    setarr( values, NAME, name );
    setarr( varlist, name, (char*)values );

    return 1;
}

int process_comment( Arr * tinfo, char * comment, int indent )
{
    char block[80];
    char * name;
    char * value;
    char loop[256];
    Arr * list;
    Arr * values;
    Tbl * keys;
    int i, count;
    int newindent;
    TagValFP fp;
    FILE * f;

    if (!sscanf( comment, " ?insert %s iterate=%s indent=%d", &block, &loop,
                 &newindent ) )
    {
       if ( !sscanf( comment, " ?%s", &block ) )  return 0;
       if ( !(fp = (TagValFP) get_varfp( tinfo, block ) ) ) return 0;
       value = fp( tinfo );

       if ( !( f = (FILE*) getarr( tinfo, FILEDES ) ) ) return 0;
       fprintf ( f, "%s", value );
    }
    else
    {
        if ((values = (Arr*) getarr( tinfo, VARS ) ) == NULL ) return 0;
        setarr( tinfo, INDENT, itoa( indent + newindent ) );
        if ( !strcmp( loop, "1" ) )
            print_template_info( tinfo, block, indent+newindent );
        else if ( loop[0] == '?' )
        {
            /* If the iterate name begins with a question mark, it means that
               we want to print out only if loop is valid. Like above,
               no actual iteration occurrs. */
            
            if ( !(fp = (TagValFP) get_varfp( tinfo, &loop[1] ) ) ) return 0;
            value = fp( tinfo );
            if ( ( value != NULL ) && ( strcmp ( value, "0" ) != 0 ) )
                print_template_info( tinfo, block, indent+newindent );
        }
        else
        {
            name = loop;
            if ((list = get_varlist( tinfo, name ) ) == NULL ) 
                return 0;
            keys = keysarr( list );
            count = maxtbl( keys );

            setarr ( tinfo, MAXINDEX, itoa(count) );
            for ( i = 0; i < count; i++ )
            {
                if ( ( value = gettbl( keys, i ) ) == NULL ) continue;
                set_varname( tinfo, name, value );

                setarr( tinfo, INDEX, itoa(i) );
                setarr( tinfo, CURRENT, name );
                print_template_info( tinfo, block, indent+newindent );
            }
            if ( strcmp( loop, NET ) == 0 )
            {
                set_varname( tinfo, loop, currentnet );
            }
            else
                set_varname( tinfo, loop, strdup("") );
            setarr( tinfo, INDEX, "-1" );
        }
    }
    
    return 1;
}

int print_template_info( Arr * tinfo, char * name, int indent )
{
    char * next_comment = NULL;
    char * data = NULL;    
    char * fd;
    char * buff;
    char * end_comment;
    FILE * f;

    if ( (data = getarr( html_template, name )) == NULL ) return 0;
    if ( (f = (FILE*) getarr( tinfo, FILEDES )) == NULL ) return 0;
    
    while ( ( next_comment = strstr( data, "<!--" ) ) != NULL )
    {
        buff = (char*) malloc ( (next_comment - data + 1) * sizeof( char ) );
        strncpy( buff, data, next_comment - data);
        buff[ next_comment - data ] = '\0';
        print_insert_indent( f, buff, indent );
        free( buff );

        if ( (end_comment = strstr( data, "-->" ) ) == NULL )
            return 0;
        buff = (char*) malloc ( (end_comment-next_comment-3) * sizeof(char));
        strncpy( buff, next_comment + 4, end_comment - next_comment - 4 );
        buff[ end_comment - next_comment - 4 ] = '\0';
        process_comment( tinfo, buff , indent ); 
        free ( buff );
        if ( data == NULL ) return 0;
        data = end_comment+3;
    }
    print_insert_indent( f, data, indent );
}


/* Converts a Tbl list into an array with the key being the string
   representation of the cardinal order of the list. Useful when we 
   need to maintain ordering but need an associative array for
   generality. */

Arr * create_ordered_array ( Tbl * list )
{
    int size;
    int i;
    Arr * fields = newarr( strcmp );
    char * key;

    size = maxtbl( list );
    for ( i = 0; i < size; i++ )
    {
        key = itoa(i);
        setarr( fields, key, gettbl( list, i ) );
        /* free(key); */
    }

    return fields;
}


int print_header_info ( Arr * tinfo )
{
    Arr * stylearr = getpf( orbinfo, STYLES );
    Tbl * style_list;
    char * style_name;
    char * style_value;
    int i, size;
    char blank[1] = "";

    FILE * f = (FILE *) getarr( tinfo, FILEDES );

    fprintf ( f, "Content-type: text/html\n\n" );
    fprintf ( f, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 " );
    fprintf ( f, "Transitional//EN\"\n" );
    fprintf ( f, "    \"http://www.w3c.org/TR/html4/loose.dtd\">\n" ); 
    fprintf ( f, "<HTML>\n\n" );
    fprintf ( f, "<HEAD>\n" );
    fprintf ( f, "<META http-equiv=\"refresh\" content=\"%d\">\n", 
                 (int)( ( atoi( getpf( orbinfo, UPDATE ) ) * 60) + 0.5) );
    fprintf ( f, "<META http-equiv=\"Content-Type\" content=\"text/html; " );
    fprintf ( f, "charset=ISO-8859-1\">\n" );
    fprintf ( f, "<META http-equiv=\"Expires\" content=\"now\">\n" );
    fprintf ( f, "<META http-equiv=\"Pragma\" content=\"nocache\">\n" );
    fprintf ( f, "<TITLE>%s real-time status</TITLE>\n", 
              getpf( orbinfo, ORBNAME ) );

    fprintf ( f, "<STYLE>\n" );

    /* Print out css styles from general pf. */
    style_list = keysarr( styles );
    size = maxtbl( style_list );
    for ( i = 0; i < size; i++ )
    {
        style_name = gettbl( style_list, i );
        if ( (style_value = (char*) getpf( styles, style_name ) ) == NULL )
            style_value = blank;
        fprintf (f, "  %s { %s }\n", style_name, style_value );
    }

    /* Print out css styles from orbs' pf. */
    style_list = keysarr( stylearr );
    size = maxtbl( style_list );
    for ( i = 0; i < size; i++ )
    {
        style_name = gettbl( style_list, i );
        style_value = getpf( stylearr, style_name );
        fprintf (f, "  .%s { %s }\n", style_name, style_value );
    }

    fprintf ( f, "</STYLE>\n" );

    fprintf ( f, "</HEAD>\n\n" );
    fprintf ( f, "<BODY>\n" );
    return 1;
}



int print_footer_info ( Arr * tinfo )
{
    FILE * f = (FILE*) getarr( tinfo, FILEDES );
    fprintf ( f, "</BODY>\n" );
    fprintf ( f, "</HTML>\n" );
    return 1;
}

int generate_doc ( Arr * slist, Tbl * field_list )
{
    Arr * tinfo = newarr( strcmp );
    Arr * values = newarr( strcmp ); 

    setarr( tinfo, FILEDES, (char*) stdout );
    setarr( tinfo, SLIST, (char*) slist );
    setarr( tinfo, FIELD_LIST, (char*) generate_headers( field_list ) );

    /* First, initialize the tinfo structure with all of the variables
       available to be used. */
    add_var( values, STA, get_stalist, get_current_value );
    add_var( values, NET, get_netlist, get_net_value );
    add_var( values, USED_FIELD, get_used_fieldlist, get_ordered_value );
    add_var( values, ALL_FIELD, get_fieldlist, get_current_value );
    add_var( values, ALERT_STAGE, get_alertlist, get_current_value );
    add_var( values, ORB, get_orblist, get_current_value );
    add_var( values, ROW_SEPERATOR, NULL, get_row_seperator );
    add_var( values, SCRIPT_NAME, NULL, get_script_name );
    add_var( values, ORBNAME, NULL, get_orblookup_orbname );
    add_var( values, NUMCOLS, NULL, get_tinfo_numcols );
    add_var( values, NUMUSEDFIELDS, NULL, get_num_used_fields );
    add_var( values, NUMFIELDS, NULL, get_num_fields );
    add_var( values, NUMNETS, NULL, get_numnets );
    add_var( values, NUMSTAS, NULL, get_numstas );
    add_var( values, NUMSTASTOT, NULL, get_numstas_total );
    add_var( values, NUMSTASEXP, NULL, get_numstas_expected );
    add_var( values, NUMCHANS, NULL, get_tinfo_numchans );
    add_var( values, NUMCHANSTOT, NULL, get_tinfo_numchans_total );
    add_var( values, MINNETLATENCY, NULL, get_tinfo_min_latency );
    add_var( values, MEDIANNETLATENCY, NULL, get_tinfo_median_latency );
    add_var( values, MAXNETLATENCY, NULL, get_tinfo_max_latency );
    add_var( values, PERCENTWIDTH, NULL, get_percentwidth );
    add_var( values, ENCODEDNET, NULL, get_encoded_net );
    add_var( values, ENCODEDNETSTA, NULL, get_encoded_netsta );
    add_var( values, STACLASS, NULL, get_staclass );
    add_var( values, NETCLASS, NULL, get_netclass );
    add_var( values, ALERT_CLASS, NULL, get_alertclass );
    add_var( values, ALERT_DESC, NULL, get_alertdesc );
    add_var( values, ROW_NUM_CLASS, NULL, get_modclass );
    add_var( values, FIELD_ALIGN, NULL, get_fieldalign );
    add_var( values, FIELD_VALUE, NULL, get_fieldvalue );
    add_var( values, FIELD_CLASS, NULL, get_fieldstyle );
    add_var( values, CHECKED_STATUS, NULL, get_field_checked );
    add_var( values, TIME, NULL, get_run_time );
    add_var( values, FULLNET, NULL, get_tinfo_fullnetname );
    add_var( values, NETURL, NULL, get_neturl );
    add_var( values, ORBURL, NULL, get_orburl );
    add_var( values, ALERTMODE, NULL, get_alertmode );
    setarr( tinfo, VARS, (char*) values );

    /* Before we create the pages, initialize the two variables we
       should have: orb and net */
    set_varname ( tinfo, ORB, encode_uri( getpf( orbinfo, ORBNAME ),
                                          strlen( getpf( orbinfo, ORBNAME ))));

    /* Finall, generate the web page */
    print_header_info( tinfo );
    if ( currentnet )
    {
        print_template_info( tinfo, "netspecific", 0 );
        set_varname( tinfo, NET, currentnet );
    }
    else
        print_template_info( tinfo, "overview", 0 );
    print_footer_info( tinfo );

    return 1;
}

int get_field_position( char * fieldname )
{
    char * s;
    Arr * field = (Arr*) getpf( getpf( orbinfo, FIELDS ), fieldname );
    if ( !(s = getpf( field, ORDER ) ) ) return -1;
    return atoi( s ); 
}

Arr * generate_headers ( Tbl * field_list )
{
    int i;
    int numfields = atoi( getpf( orbinfo, NUMFIELDS ) );
    int size;
    char * fieldname;
    char * key;
    Arr * newlist;
    void * private = NULL;
    Arr * fields = getpf( orbinfo, FIELDS );

    newlist = newarr( strcmp );
    if ( field_list == NULL )
    {
        for ( i = 0; i < numfields; i++ )
        {
            if (!(fieldname = get_field_in_position( i ))) 
            {
                free( field_list );
                return NULL;
            }
            key = itoa( i );
            setarr( newlist, key , strdup( fieldname ) );
            /* free(key); */
        }
        return newlist;
    }
    else 
    {
        /* First, we need to eliminate any entries that are not fields. */

        size = maxtbl( field_list );
        for ( i = 0; i < size; i++ )
        {
            fieldname = gettbl( field_list, i );
            if ( getarr( fields, fieldname ) != NULL )
            {
                key = itoa( get_field_position(fieldname) );
                setarr( newlist, key, strdup(fieldname) );
                /* free(key); */
            }
        }

        freetbl( field_list, free );
    }

    return newlist;
}


char * get_alert_name( int priority )
{
    Arr * stages = getpf( orbinfo, ALERT_STAGES );
    Arr * level;
    char * nothing = strdup("");
    char * alert;
    char * name;
    
    if ( ( !orbinfo ) || ( stages == NULL ) ) return nothing;

    name = itoa( priority );
    if ((level = getpf( stages, name )) == NULL )
    {
        /* free(name); */
        return nothing;
    }

    /* free( name ); */
    if ((alert = getpf( level, NAME )) == NULL )   
        return nothing;

    return alert;
}

int get_staclasslevel ( Arr * sinfo )
{
    int i;
    int max = 0;
    int curr = 0;
    int size;
    char * field;
    char * level;
    Tbl * fields;

    if ( sinfo == NULL ) return 0;

    if ( level = getarr( sinfo, "alertlevel" ) )
        return ( atoi( level ) );

    /* check the status of each field. */
    fields = keysarr( get_station_fields( sinfo ) );
    size = maxtbl( fields );
    for ( i = 0; i < size; i++ )
    {
        field = gettbl( fields, i );
        curr = check_field_thresholds( sinfo, field ); 

        if ( curr > max ) max = curr;
    }

    setarr( sinfo, "alertlevel", itoa(max) );

    return max;
}



/* Determines the maximum alert stage for all fields in this group
   and returns the priority corresponding to that stage from the pf. */
char * select_groupstyle( Arr * sinfo )
{
    int level = get_staclasslevel( sinfo );
    int isnew;
    char * alert;

    /* if no alert conditions were triggered and this is a new station,
       report as a new station.  Otherwise, report as the class for
       the priority level found. */
    isnew = is_station_new( sinfo );
    if ( ! get_station_field( sinfo, LATENCY ) )
        alert = getpf( orbinfo, NOSTA );
    else
    if ( ( level != 0 ) || ( !isnew ) )
        alert = get_alert_name( level );
    else
        alert = getpf( orbinfo, NEWSTA );

    return alert; 
}

int generate_test_page( char * data, int size )
{
    int i;
    time_t now;
    Tbl * field_list;
    char * orbname;

    now = time(NULL);

    printf ( "Content-type: text/html\n\n" );
    printf ( "<HTML>\n" );
    printf ( "<META http-equiv=\"refresh\" content=\"300\">\n" );
    printf ( "<TITLE>Test Page for IGPPRT:ANZA</TITLE>\n" );
    printf ( "<BODY>\n  <p>Last updated: %s</p>\n", ctime(&now) );

    field_list = create_field_list( data, size, &orbname );

    if ( field_list != NULL )
    {
        size = maxtbl( field_list );
        for ( i = 0; i < size ; i++ )
            printf( "<P>%s is on</P>\n", gettbl( field_list, i ) );
    }

    printf ( "</BODY>\n\n" );
    printf ( "</HTML>\n" );
    return 1;
}

Arr * read_cached_results( char * cachefile )
{
    FILE * infile;
    char pfpath[256];
    char * time;
    Pf * pf = NULL;
    Arr * networks;
    double newtime;
    Pf * pfarr;
    Pf * pftime;

    pfread( cachefile, &pf );
    networks = (Arr*) getpf( pf->value.arr, NETWORKS );
    time = getpf( pf->value.arr, TIME );
    if ( !time )
    {
        generate_error_page();
        exit(-1);
    }
    cachetime = str2epoch( time );

    fclose ( infile );
    return networks; 
}

char * encode_uri ( char * data, int len )
{
    int loc1 = 0;
    int loc2 = 0;
    int size = strlen( data ) + 10;
    char * temp;
    char * dest = (char*) malloc ( size * sizeof(char ) );
    dest[0] = 0;

    for ( loc1 = 0; loc1 < len; loc1++ )
    {
        if (( loc2+3 ) > size )
        {
            size += 100;
            temp = (char*) malloc ( size * sizeof( char* ) );
            strcpy( temp, dest );
            free(dest);
            dest = temp;
        }

        if ( !isalnum( data[loc1] ) )
        {
            if ( data[loc1]==' ' )
            {
                dest[loc2++] = '+';
            }
            else
            {
                sprintf( &dest[loc2], "%%%x", (int) (data[loc1]) );
                loc2+=3;
            }
        }
        else
            dest[loc2++] = data[loc1];
    }
    
    dest[loc2] = 0;
    return dest;    
}


char * decode_uri ( char * data )
{
    int start = 0;
    int loc = -1;
    int lasts = 0;
    int length_to_copy = 0;
    char buff[5];
    char c;
    char * n;
    char * scr = "";
    char * s;

    if ( data == NULL ) return strdup("");

    /* note: we are making a rather large assumption that content-length
       is not bigger than int size.  If it is, this should have been
       handled upstream. */
    s = (char*) malloc( strlen(data) * sizeof(char) );
    s[0] = 0;

    /* search for encoded chars, starting with the current data position. */
    while ( ( n = strchr(&data[start],'%')) != NULL )
    {
        /* first, concat everything until the encoded character to s. */
        loc = n - data;
        length_to_copy = loc - start;
        strncat( s, &data[start], length_to_copy );

        /* next, decode the hex character and concat that to s. */
        strncpy( buff, &data[loc+1], 2 );
        c = (char)(strtol(buff, &scr, 16 ));
        strncat( s, &c, 1 );

        /* finally, increment start so we skip the char we just decoded. */
        start = loc+3;
    }

    /* copy all the rest of the query string into s */
    strcat( s, &data[start] );
 
    return s;
}

Arr * to_nvpairs( char * data, int len )
{
    int i;
    int curr1 = 0;         /* position in the data string */
    int curr2 = 0;         /* position in the current buffer */
    int nvcount = 0;       /* the index of the next nvpair */
    int isname = 1;        /* boolean, flags whether working on name */
    int failure = 0;       /* boolean, flags if ignore until next & */
    Arr * nvpairs;
    char name[80];
    char value[80];

    if (( data == NULL ) || ( data[0] == '\0' )) return NULL;

    nvpairs = newarr( strcmp );

    while ( curr1 < len )
    {
        switch( data[curr1] )
        {
            case '&' : /* first, copy name/value into nvpairs. */
                       if ( !failure )
                       {
                           if ( isname )
                               name[curr2]='\0';
                           else
                               value[curr2]='\0';
                           setarr( nvpairs, decode_uri(name), decode_uri(value) );
                       }

                       /* next, reset values for the next round. */
                       isname = 1;
                       name[0] = 0;
                       value[0] = 0;
                       curr2 = 0;
                       failure = 0;
                       break;

            case '=' : if ( failure ) break;
                       name[curr2] = 0;

                       /* fail if nothing in buffer, ie no name yet. */
                       if ( strcmp( name, "" ) == 0 )
                           failure = 1;

                       /* reset buffer and vars. */
                       value[0] = 0;
                       curr2 = 0;
                       isname = 0;
                       break;

            case '+' : if ( failure ) break;
                       if (isname)
                           name[curr2++] = ' ';
                       else 
                           value[curr2++] = ' ';
                       break;

            default  : if ( failure ) break;
                       if ( isname )
                           name[curr2++] = data[curr1];
                       else 
                           value[curr2++] = data[curr1];
        }
        curr1++; 
    }
    if ( !failure )
    {
        if ( isname )
            name[curr2] = '\0';
        else
            value[curr2] = '\0';

        setarr( nvpairs, decode_uri(name), decode_uri(value) );
    }

    return nvpairs;
}

Tbl * create_field_list ( char * data, long content_length, char ** orbname )
{
    Tbl * list;
    Tbl * keys;
    int size;
    int i;
    char * buff;
    Arr * nvpairs;
    char * key;

    if ( data == NULL ) return NULL;

    /* generate the name value pairs from data */
    nvpairs = to_nvpairs( data, content_length );
    if ( nvpairs == NULL ) return NULL;
    if ( orbname == NULL ) return NULL;

    if (!(buff = delarr( nvpairs, ORBNAME ) ) ) return NULL;

    *orbname = decode_uri(buff);

    currentnet = delarr( nvpairs, "net" );
    if ( currentnet )
       if ( strcmp( currentnet, "" ) == 0 )
       {
           free( currentnet );
           currentnet = NULL;
       }

    if ( delarr( nvpairs, ALERTMODE ) ) alertmode = 1;
    
    /* if ( delarr( nvpairs, "updatenow" ) ) usecache = 0;  */

    /* if the all flag is tripped, don't create the list now. Let it be
       done later. */
    if ( getarr( nvpairs, "all" ) ) return NULL;

    keys = keysarr( nvpairs );
    size = maxtbl( keys );
    list = newtbl( size );

    for ( i = 0; i < size; i++ )
    {
        key = gettbl( keys, i );
        if ( strcmp( getarr( nvpairs, key ), "on" ) == 0 )
            pushtbl( list, strdup(key) );
    }

    freearr( nvpairs, free );
    return list;
}

int print_usage( char * pn )
{
    fprintf ( stderr, "Usage: %s [ parameter file ]\n", pn );
    return 1;
}

int main ( int argc, char ** argv ) 
{
    Tbl * field_list = NULL;
    char * scr = NULL;
    char * req = NULL; 
    char * data = NULL;
    char * pfn = NULL;
    char * orbname;
    char * cachefile;
    long content_length = 0;
    Arr* slist;

    cachetime = 0;
    currentnet = NULL;
    alertmode = 0;

    if ( argc > 2 )
    {
        print_usage( argv[0] );
        exit(-1);
    }

    if ( argc == 1 )
        pfn = argv[0];
    else
        pfn = argv[1];

    /* Parse the information sent back from the browser. If no request
       method is set, we can assume that this is a standard request
       and no form information will be provided. */
    if (((req = getenv("REQUEST_METHOD" )) == NULL) || !strcmp( req, "" ) )
    {
        generate_entry_page();
        exit(0);
    }
    else 
    {
        /* Grab the length of the message and correct for bad data. */
        if (((scr = getenv("CONTENT_LENGTH")) != NULL ) && strcmp( scr, "" ) )
            content_length = atol( scr );
        if ( content_length == 0 ) data = strdup( "" );

        /* If the GET method was used, get message from query string. */
        if ( strcmp( req, "GET" ) == 0 )
        {
            if (((data = strdup(getenv("QUERY_STRING"))) == NULL ) ||
                 !strcmp( data, "" ) )
            {
                generate_entry_page();
                exit(0);
            }
            if ( content_length < 1 ) content_length = strlen(data);
        }
        /* If the POST method was used, get message from stdin. */
        else if  ( strcmp( req, "POST" ) == 0 ) 
        {
            /* If content length might be munged, set it temporarily and
                move on. */
            if (content_length < 1 ) content_length=100;

            /* Allocate memory for the message. */
            if (!(data = (char*) malloc ( (content_length+1) * sizeof(char))))
            {
                generate_entry_page();
                exit(0);
            }
            /* Read the message from stdin, correcting for content length. */
            if ( (content_length = fread( data, sizeof(char), content_length, 
                                          stdin )) < 0 ) 
            {
                generate_entry_page();
                exit(0);
            }

            data[content_length]='\0';
        }
        /* If neither method was used, we are looking for a list of available
           orbs. */
        else
        {
            generate_entry_page();
            exit(0);
        }
    }

    /* parse the query string to determine what fields to process and
       which orb to check. */
    field_list = create_field_list( data, content_length, &orbname );
    free( data );

    /* generate the web page output from the orb named in orbname displaying
       the fields gathered above. */
    read_orbinfo( "orblookup.pf", 
                  orbname, &cachefile );
    slist = read_cached_results( cachefile );    
    generate_doc( slist, field_list );

    return 0;
}
