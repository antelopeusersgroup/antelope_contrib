/* 
 * site_ew_ext.c
 *
 * Utility functions added to Earthworm to allow interface with Datascope
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * May, 1996
 */

#include <stdlib.h>
#include "earthworm.h"
#include "db.h"
#include "stock.h"
#include "iceworm_extensions.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

static Arr *netstas = NULL;  /* Names of stations in chosen network */

static int cmp_sites_stachan( void *, void *, void * );
static void init_stachan( STACHAN * );
static void set_timecorrs( Dbptr );
static void set_respfiles( Dbptr );

/***********************************************************************
 *  cmp_sites_stachan() compare the station and channel in a pair of    *
 *     stachan structures--used in sorting and searching the list of   *
 *     stachan info structures                                         *
 ***********************************************************************/
static int
cmp_sites_stachan( void *ap, void *bp, void *private )
{
	STACHAN **a, **b;

	a = (STACHAN **) ap;
	b = (STACHAN **) bp;

        if(strcmp((*a)->sta, (*b)->sta) > 0) return 1;
        else if(strcmp((*a)->sta, (*b)->sta) < 0) return -1;
        else
        {
                if(strcmp((*a)->chan, (*b)->chan) > 0) return 1;
                else if(strcmp((*a)->chan, (*b)->chan) < 0) return -1;
                else return 0;
        }
}

/***********************************************************************
 *  read_site_db() reads a database of site information into the       *
 *     stachan structures. Returns the number of stachans found.       *
 ***********************************************************************/
int
read_site_db( char *dbname )
{
	Dbptr	dbstas;
	Dbptr	dbsite;
	Dbptr   dbsitechan;
	Dbptr	dbpins;
	Dbptr	dbcalib;
	STACHAN *stachan;
	int     nstachans;
	int     site_present;
	int     sitechan_present;
	int	pins_present;
	int     network_present;
	int     affiliation_present;
	int     sensor_present;
	int     instrument_present;
	int	calibration_present;
	int	timecorr_present;
	void    *private;

        dbopen( dbname, "r", &dbstas );
        dbstas = dblookup( dbstas, 0, "site", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &site_present );
        dbstas = dblookup( dbstas, 0, "sitechan", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &sitechan_present );
        dbstas = dblookup( dbstas, 0, "pins", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &pins_present );
        dbstas = dblookup( dbstas, 0, "network", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &network_present );
        dbstas = dblookup( dbstas, 0, "affiliation", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &affiliation_present );
        dbstas = dblookup( dbstas, 0, "sensor", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &sensor_present );
        dbstas = dblookup( dbstas, 0, "instrument", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &instrument_present );
        dbstas = dblookup( dbstas, 0, "calibration", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &calibration_present );
        dbstas = dblookup( dbstas, 0, "timecorr", 0, 0 );
        dbquery( dbstas, dbTABLE_PRESENT, &timecorr_present );
 
        if( site_present == 0 )
        {
                logit("e", "Table site not present in %s\n", dbname );
                exit( -1 );
        }
        if( sitechan_present == 0 )
        {
                logit("e", "Table sitechan not present in %s\n", dbname );
                exit( -1 );
        }
        if( pins_present == 0 )
        {
                logit("e", "Table pins not present in %s\n", dbname );
                exit( -1 );
        }
        if( calibration_present == 0 )
        {
                logit("", "Table calibration not present in %s\n", dbname );
		exit( -1 );
        }
        if( network_present == 0 )
        {
                logit("", "Table network not present in %s\n", dbname );
        }
        if( affiliation_present == 0 )
        {
                logit("", "Table affiliation not present in %s\n", dbname );
        }
        if( sensor_present == 0 )
        {
                logit("", "Table sensor not present in %s\n", dbname );
        }
        if( instrument_present == 0 )
        {
                logit("", "Table instrument not present in %s\n", dbname );
        }
        if( timecorr_present == 0 )
        {
                logit("", "Table timecorr not present in %s\n", dbname );
        }

	Stachans = newtbl( 0 );

        dbsite = dblookup( dbstas, 0, "site", 0, 0 );
        dbsitechan = dblookup( dbstas, 0, "sitechan", 0, 0 );
        dbpins = dblookup( dbstas, 0, "pins", 0, 0 );
	dbcalib = dblookup( dbstas, 0, "calibration", 0, 0 );

        dbstas = dbjoin( dbsitechan, dbcalib, 0, 0, 0, 0, 0 );
        dbstas = dbjoin( dbstas, dbpins, 0, 0, 0, 0, 0 );
	dbstas = dbjoin( dbstas, dbsite, 0, 0, 0, 0, 0 );

        dbstas = dbsubset( dbstas, "sitechan.offdate == NULL", 0 );
	dbstas = dbsubset( dbstas, "site.offdate == NULL", 0 );
	dbstas = dbsubset( dbstas, "calibration.endtime == NULL", 0 );

        dbquery( dbstas, dbRECORD_COUNT, &nstachans );
        for(dbstas.record = 0; dbstas.record<nstachans; dbstas.record++)
        {
                stachan = (STACHAN *) malloc( sizeof( STACHAN ) );

		init_stachan( stachan );

                dbgetv(dbstas, 0, "sta", &stachan->sta,
                                  "chan", &stachan->chan,
                                  "chanid", &stachan->chanid,
				  "pinno", &stachan->pinno,
                                  "lat", &stachan->lat,
                                  "lon", &stachan->lon,
                                  "elev", &stachan->elev,
				  "calib", &stachan->calib,
				  "calper", &stachan->calper,
				  "units", &stachan->units,
				  "savechan", &stachan->savechan, 0);

                pushtbl( Stachans, stachan );
        }

        sorttbl( Stachans, cmp_sites_stachan, private );

	if( timecorr_present ) set_timecorrs( dbstas );
	if( sensor_present && instrument_present ) set_respfiles( dbstas );

        dbclose( dbstas );
	
	return( maxtbl( Stachans ) );
}

/***********************************************************************
 *  init_stachan() initialize the contents of a stachan structure      *
 ***********************************************************************/
static void
init_stachan( STACHAN *stachan )
{
	strcpy( stachan->sta, "" );
	strcpy( stachan->chan, "" );
	stachan->chanid = -1;
	stachan->pinno = -1;
	stachan->lat = -999.;
	stachan->lon = -999.;
	stachan->elev = -999.;
	stachan->calib = 0.;
	stachan->calper = -1.;
	stachan->commdelay = 0.;
	strcpy( stachan->units, "" );
	strcpy( stachan->savechan, "n" );
	strcpy( stachan->respfile, "" );
	return;
}


/***********************************************************************
 *  set_respfiles() set the response-file pathnames for stachans       *
 ***********************************************************************/
static void
set_respfiles( Dbptr db )
{
	int	nrows;
	char	sta[7];
	char	chan[9];
	char	respfile[FILENAME_MAX];
	double	commdelay;
	STACHAN	*stachan;
	Dbptr	dbsensor;

	db = dblookup( db, 0, "instrument", 0, 0 );
	dbsensor = dblookup( db, 0, "sensor", 0, 0 );
	db = dbjoin( db, dbsensor, 0, 0, 0, 0, 0 );
	db = dbsubset( db, "endtime == NULL", 0 );

	dbquery( db, dbRECORD_COUNT, &nrows );

	for(db.record = 0; db.record<nrows; db.record++)
	{
		dbgetv( db, 0, "sta", sta,
			       "chan", chan,
			       0 );
		if( dbfilename( db, respfile ) <= 0 ) continue;

		stachan = lookup_stachan( sta, chan );
		if( stachan != (STACHAN *) NULL )
		{
			strcpy( stachan->respfile,  respfile );
		}
	}

	return;
}

/***********************************************************************
 *  set_timecorrs() set the time-correction values for stachans        *
 ***********************************************************************/
static void
set_timecorrs( Dbptr db )
{
	int	nrows;
	char	sta[7];
	char	chan[9];
	double	commdelay;
	STACHAN	*stachan;

	db = dblookup( db, 0, "timecorr", 0, 0 );
	dbquery( db, dbRECORD_COUNT, &nrows );
	for(db.record = 0; db.record<nrows; db.record++)
	{
		dbgetv( db, 0, "sta", sta,
			       "chan", chan,
			       "commdelay", &commdelay, 0 );
		stachan = lookup_stachan( sta, chan );
		if( stachan != (STACHAN *) NULL )
		{
			stachan->commdelay = commdelay;
		}
	}

	return;
}

/***********************************************************************
 *  lookup_stachan() returns a pointer to a stachan structure for the  *
 *     named station and channel				       *
 ***********************************************************************/
STACHAN *
lookup_stachan( char *sta, char *chan )
{
	STACHAN *stachan;
	int     ns_stachan, ne_stachan;
	void    *private;
	int     n;
	
	if( Stachans == NULL ) 
	{
		return (STACHAN *) NULL;
	}

	stachan = (STACHAN *) malloc( sizeof( STACHAN ) );
	strcpy( stachan->sta, sta );
	strcpy( stachan->chan, chan );
	n = searchtbl( (char **) &stachan, Stachans, cmp_sites_stachan,
				private, &ns_stachan, &ne_stachan );
	free( stachan );
	if( n != 1 )
	{
		return (STACHAN *) NULL;
	}
	else
	{
		stachan = gettbl( Stachans, ns_stachan );
		return stachan;
	}
}

/***********************************************************************
 *  lookup_network() lookup the stations in a given network and        *
 *    stores the list. The network string specified can be multiple    *
 *    network names separated by plusses and minusses, indicating      *
 *    set-theoretic unions and differences between the specified       *	
 *    networks.  						       *
 ***********************************************************************/
int
lookup_network( char *sitedb, char *network )
{
	Dbptr	db;
	char	expr[STRSZ];
	int	nrows;
	char	sta[7];
	static int nonnull = 0x1;
	char	*net, *netptr;
	char	network_copy[STRSZ];
	int	add = 1;
	Tbl	*plusnets, *minusnets;

	if( netstas != (Arr *) NULL ) freearr( netstas, 0 );

	if( STREQ( network, "-" ) )
	{
		return( 0 );
	}

	strcpy( network_copy, network );

	plusnets = newtbl( 0 );
	minusnets = newtbl( 0 );
	netptr = network_copy;
	while( *netptr != 0 )
	{
		while( *netptr == ' ' || *netptr == '+' || *netptr == '-' )
		{
			switch( *netptr )
			{
			case ' ':
				*netptr++;
				break;
			case '+':
				add = 1;
				*netptr++;
				break;
			case '-':
				add = 0;
				*netptr++;
				break;
			}
		}
		if( *netptr != 0 )
		{
			if( add == 1 ) pushtbl( plusnets, netptr );
			else pushtbl( minusnets, netptr );
		}
		while( *netptr != ' ' && *netptr != 0 &&
			*netptr != '+' && *netptr != '-' ) netptr++;
		if( *netptr == ' ' || *netptr == '+' || *netptr == '-' )
		{
			switch( *netptr )
			{
			case ' ':
				*netptr++ = 0;
				break;
			case '+':
				add = 1;
				*netptr++ = 0;
				break;
			case '-':
				add = 0;
				*netptr++ = 0;
				break;
			}
		}
	}

        netstas = newarr( 0 );
        dbopen( sitedb, "r", &db );
	while( ( net = poptbl( plusnets ) ) != NULL )
	{
        	db = dblookup( db, 0, "affiliation", 0, 0 );
        	sprintf( expr, "net == \"%s\"", net );
        	db = dbsubset( db, expr, 0 );
        	dbquery( db, dbRECORD_COUNT, &nrows );
                for( db.record = 0; db.record < nrows; db.record++ )
                {
                       	dbgetv( db, 0, "sta", &sta, 0 );
                       	setarr( netstas, sta, &nonnull );
                }
	}
	while( ( net = poptbl( minusnets ) ) != NULL )
	{
        	db = dblookup( db, 0, "affiliation", 0, 0 );
        	sprintf( expr, "net == \"%s\"", net );
        	db = dbsubset( db, expr, 0 );
        	dbquery( db, dbRECORD_COUNT, &nrows );
                for( db.record = 0; db.record < nrows; db.record++ )
                {
                       	dbgetv( db, 0, "sta", &sta, 0 );
                       	delarr( netstas, sta );
                }
	}
	freetbl( plusnets, 0 );
	freetbl( minusnets, 0 );
	return( maxtbl( keysarr( netstas ) ) );
}

/***********************************************************************
 * station_in_network() Checks to see if a station is in the loaded    *
 *   network. Fails silently returning 0 if no network has been loaded.*
 ***********************************************************************/
int
station_in_network( char *sta )
{
	if( netstas == (Arr *) NULL ) return( 0 );

	if( getarr( netstas, sta ) == NULL )
	{
		return( 0 );
	} 
	else
	{
		return( 1 );
	}
}
/***********************************************************************
 * subset_for_network() Removes from the Stachans table all stachans   *
 *   in the loaded network. Returns the number of stachans remaining.  *
 ***********************************************************************/
int
subset_for_network()
{
	int	n, i;
	STACHAN *stachan;

	if( netstas == (Arr *) NULL ) return( -1 );

	n = maxtbl( Stachans );
	for( i = n - 1; i >= 0; i-- )
	{
		stachan = gettbl( Stachans, i );
		if( station_in_network( stachan->sta ) )
		{
			continue;
		} 
		else 
		{
			deltbl( Stachans, i );
		}
	}

	return( maxtbl( Stachans ) );
}

/***********************************************************************
 * units_to_segtype() Attempt to convert units specification into      *
 *   css3.0 segtype 						       *
 ***********************************************************************/
void
units_to_segtype( char *units, char *segtype )
{
		if( STREQ( units, "nm" ) )
		{
			strcpy( segtype, "D" );
		}
		else if( STREQ( units, "nm/sec" ) )
		{
			strcpy( segtype, "V" );
		}
		else if( STREQ( units, "nm/sec**2" ) )
		{
			strcpy( segtype, "A" );
		}
		else
		{
			strcpy( segtype, "-" );
		}
		return;
}
