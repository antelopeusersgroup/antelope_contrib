#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "elog.h"
#include "ak_ahheader.h"

static void n2h_ak_ahhead( ak_ahhed * );
static int aah_datatype_to_size( short );
static double aah_abstime_to_epoch( struct time );

static void
usage ()
{
	char *usage = "aah2db [-vV] ahfile [ahfile...] dbname";
	char *author = "Kent Lindquist";
	char *location = "Geophysical Institute, U. of Alaska";
	char *email = "kent@giseis.alaska.edu";
	cbanner ( "$Revision$ $Date$",
		usage, 
		author,
		location,
		email );
}

int
main (int argc, char **argv)
{
	ak_ahhed ah;
	int	c;
	int	errflg = 0;
	Dbptr	db;
	char	*dbname;
	char	dbdir[FILENAME_MAX];
	char	dbdfile[FILENAME_MAX];
	int	verbose = 0;
    	int	nargs;		   
	char	dir[FILENAME_MAX];
	char	dfile[FILENAME_MAX];
	char	suffix[STRSZ];
	char	*ahfile;
	FILE	*fp;
	struct stat fpstat;
	int	datasize;
	char	*datatype = "aa";
	int	nsamp;
	int	foff;
	double	samprate;
	double	time;

    	elog_init ( argc, argv ) ; 

	while ((c = getopt (argc, argv, "vV")) != -1) {
		switch (c) {
	
		case 'v':
	    		verbose++;
	    		break;
	
		case 'V':
	    		usage();
	    		exit (0);

		default:
	    		errflg++;
	    		break ;
		}
	}

	nargs = argc - optind;
	if (errflg || argc - optind < 2 ) {
		usage ();
		exit( 1 );
	}

	dbname = argv[argc-1];
	parsepath( dbname, dbdir, dbdfile, 0 );
	
	dbopen( dbname, "r+", &db );
	db = dblookup( db, "", "wfdisc", "", "" );

	while( optind < argc - 1 ) {

		ahfile = argv[optind];
		parsepath( ahfile, dir, dfile, suffix );

		if( strcmp( dir, dbdir ) ) {
			abspath( dir, dir );
		}

		if( strcmp( suffix, "" ) &&
		    strcmp( suffix, "Z" ) &&
		    strcmp( suffix, "gz" ) ) {
			/* Oops, don't strip suffix */
			strcat( dfile, "." );
			strcat( dfile, suffix );
		}

		fp = zopen( ahfile, "r" );
		if( fp == NULL ) {
			die( 1, "Error opening %s", ahfile );
		}
		foff = ftell( fp );
		fstat( fileno( fp ), &fpstat );

		while( foff < fpstat.st_size ) {

			fread( &ah.station, 1, 520, fp );
			fread( &ah.event, 1, 22, fp );
			fread( &ah.event.ot.sec, 1, 86, fp );
			fread( &ah.record.ndata, 1, 22, fp );
			fread( &ah.record.abstime.sec, 1, 290, fp );
			fread( &ah.extra, 1, 84, fp );

			n2h_ak_ahhead( &ah );

			datasize = aah_datatype_to_size( ah.record.type );
			nsamp = ah.record.ndata;
			samprate = 1.0 / ah.record.delta;

			time = aah_abstime_to_epoch( ah.record.abstime );

			db.record = dbaddnull( db );

			dbputv( db, 0, 
				   "sta", ah.station.code,
				   "chan", ah.station.chan,
				   "time", time,
				/* "wfid", wfid, */
				/* "chanid", chanid, */
				   "jdate", yearday( time ),
				   "endtime", ENDTIME(time,samprate,nsamp),
				   "nsamp", nsamp, 
				   "samprate", samprate, 
				/* "calib", calib, */
				/* "calper", calper, */
				/* "instype", instype, */
				/* "segtype", segtype, */
				   "datatype", datatype,
				/* "clip", clip, */
				   "dir", dir, 
				   "dfile", dfile, 
				   "foff", foff, 
				/* "commid", commid, */
				   0 );

			fseek( fp, nsamp * datasize, SEEK_CUR );
			foff = ftell( fp );
		}

		fclose( fp );

		optind++;
	}

	return 0;
}

static void
n2h_ak_ahhead( hed )
ak_ahhed	*hed;
{
	int	i;

	N2H4( &hed->station.slat, &hed->station.slat, 1 );
	N2H4( &hed->station.slon, &hed->station.slon, 1 );
	N2H4( &hed->station.elev, &hed->station.elev, 1 );
	N2H4( &hed->station.DS, &hed->station.DS, 1 );
	N2H4( &hed->station.A0, &hed->station.A0, 1 );

	for(i=0; i< NOCALPTS; ++i)
	{
		N2H4( &hed->station.cal[i].pole.r, 
		      &hed->station.cal[i].pole.r, 1 );
		N2H4( &hed->station.cal[i].pole.i, 
		      &hed->station.cal[i].pole.i, 1 );
		N2H4( &hed->station.cal[i].zero.r, 
		      &hed->station.cal[i].zero.r, 1 );
		N2H4( &hed->station.cal[i].zero.i, 
		      &hed->station.cal[i].zero.i, 1 );
	}

	N2H4( &hed->event.lat, &hed->event.lat, 1 );
	N2H4( &hed->event.lon, &hed->event.lon, 1 );
	N2H4( &hed->event.dep, &hed->event.dep, 1 );

	N2H2( &hed->event.ot.yr, &hed->event.ot.yr, 1 );
	N2H2( &hed->event.ot.mo, &hed->event.ot.mo, 1 );
	N2H2( &hed->event.ot.day, &hed->event.ot.day, 1 );
	N2H2( &hed->event.ot.hr, &hed->event.ot.hr, 1 );
	N2H2( &hed->event.ot.mn, &hed->event.ot.mn, 1 );

	N2H4( &hed->event.ot.sec, &hed->event.ot.sec, 1 );

	N2H2( &hed->record.type, &hed->record.type, 1 );

	N2H4( &hed->record.ndata, &hed->record.ndata, 1 );
	N2H4( &hed->record.delta, &hed->record.delta, 1 );
	N2H4( &hed->record.maxamp, &hed->record.maxamp, 1 );

	N2H2( &hed->record.abstime.yr, &hed->record.abstime.yr, 1 );
	N2H2( &hed->record.abstime.mo, &hed->record.abstime.mo, 1 );
	N2H2( &hed->record.abstime.day, &hed->record.abstime.day, 1 );
	N2H2( &hed->record.abstime.hr, &hed->record.abstime.hr, 1 );
	N2H2( &hed->record.abstime.mn, &hed->record.abstime.mn, 1 );

	N2H4( &hed->record.abstime.sec, &hed->record.abstime.sec, 1 );

	N2H4( &hed->record.rmin, &hed->record.rmin, 1 );

	N2H4( &hed->extra, &hed->extra, NEXTRAS );

	return;
}

static int 
aah_datatype_to_size( short datatype )
{
    int datasize;

    switch( datatype ) {

    	case( ak_ahSHORT ):
		datasize = 2;
		break;
	
    	case( ak_ahFLOAT ):
    	case( ak_ahLONG ):
		datasize = 4;
		break;
	
    	case( ak_ahDOUBLE ):
    	case( ak_ahCOMPLEX ):
		datasize = 8;
		break;
    	default:
		datasize = 0;
		break;
    }

    return datasize;
}

static double aah_abstime_to_epoch( struct time abstime )
{
	double	time;
	char 	timestr[STRSZ];

	sprintf( timestr, "%d/%d/%d %d:%d:%f", 
			abstime.mo,
			abstime.day,
			abstime.yr,
			abstime.hr,
			abstime.mn,
			abstime.sec );

	return str2epoch( timestr );
}
