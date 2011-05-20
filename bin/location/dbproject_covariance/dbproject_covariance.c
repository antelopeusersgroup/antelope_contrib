#include <stdlib.h>
#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "pf.h"
#include "location.h"
#include <math.h>

#define STREQ(a, b) (strcmp((a), (b)) == 0)


void 
Usage() {
	elog_die( 1, "Usage: dbproject_covariance [-v] [-c conf] dbname\n" );
}

main( argc, argv )
int	argc;
char	*argv[];
{
	Dbptr	db;
	Dbptr	dborigin;
	char	*dbname;
	int	i,j;
	int	verbose = 0;
	double	sxx,syy,szz,stt,sxy,sxz,syz,stx,sty,stz;
	double	**covar;
	double conf;
	double	conf_setting = 0.683;
	int	np = 4;
	int	ndef;
	int	ndeg_of_freedom;
	int	orid;
	char	orid_str[STRSZ];
	char	dtype[10];
	double	sdobs;
	double	strike_rad, strike_deg;
	double	smajax, sminax, sdepth, stime;
	int	nrecs;
	char 	c;
	int	rc;

	while( ( c = getopt( argc, argv, "c:v" ) ) != -1 ) {
		switch( c ) {
		case 'c':
			conf_setting = atof( optarg );
			break;
		case 'v':
			verbose = 1;
			break;
		default:
			Usage();
			break;
		}
	}

	if( argc - optind != 1 ) {
		Usage();
	} else {
		dbname = argv[optind++];
	}

	allot( double **, covar, 4 );
	allot( double *, covar[0], 4 );
	allot( double *, covar[1], 4 );
	allot( double *, covar[2], 4 );
	allot( double *, covar[3], 4 );

	dbopen( dbname, "r+", &db );

	db = dblookup( db, 0, "origerr", 0, 0 );
	dborigin = dblookup( db, 0, "origin", 0, 0 );

	dbquery( db, dbRECORD_COUNT, &nrecs );

	for( db.record = 0; db.record < nrecs; db.record++ ) {

		dbgetv( db, 0, 
			    "orid", &orid,
			    "sxx", &sxx,
			    "syy", &syy,
			    "szz", &szz,
			    "stt", &stt,
			    "sxy", &sxy,
			    "sxz", &sxz,
			    "syz", &syz,
			    "stx", &stx,
			    "sty", &sty,
			    "stz", &stz,
			    "sdobs", &sdobs,
			    "smajax", &smajax, 
			    "sminax", &sminax,
			    "sdepth", &sdepth,
			    "stime", &stime,
			    "strike", &strike_deg,
			    0 );
		

		if( smajax != -1 || sminax != -1 || 
		    sdepth != -1 || stime != -1 || strike_deg != -1  ) {

			continue;
		}

		if( sxx == -999999999.9999 ||
		    syy == -999999999.9999 ||
		    szz == -999999999.9999 ||
		    stt == -999999999.9999 ||
		    sxy == -999999999.9999 ||
		    sxz == -999999999.9999 ||
		    syz == -999999999.9999 ||
		    stx == -999999999.9999 ||
		    sty == -999999999.9999 ||
		    stz == -999999999.9999 ) {
			continue;
		}

		sprintf( orid_str, "%d", orid );
		dborigin = dblookup( dborigin, 0, "origin",
						"orid", orid_str );
		dbgetv( dborigin, 0, "ndef", &ndef, 0 );
		dbgetv( dborigin, 0, "dtype", &dtype, 0 );

		ndeg_of_freedom = ndef;
		if( STREQ( dtype, "r" ) || STREQ( dtype, "g" ) ) {
			ndeg_of_freedom -= 3;
		} else {
			ndeg_of_freedom -= 4;
		}
		if( ndeg_of_freedom < 1 ) {
			continue;
		} 

		covar[0][0] = sxx;
		covar[1][0] = sxy;
		covar[2][0] = sxz;
		covar[3][0] = stx;
		covar[1][1] = syy;
		covar[2][1] = syz;
		covar[3][1] = sty;
		covar[2][2] = szz;
		covar[3][2] = stz;
		covar[3][3] = stt;

		conf = conf_setting;

		rc = project_covariance( covar, CHI_SQUARE, &conf, 
			sdobs, ndeg_of_freedom,
			&smajax, &sminax, &strike_deg, &sdepth, &stime );

		if( rc != 0 ) {
			elog_complain(0, "project_covariance failed." );
			smajax = -1;
			sminax = -1;
			strike_deg = -1;
			sdepth = -1;
			stime = -1;
			conf = 0.;
		}
		
		rc = dbputv( db, 0, "conf", conf, 
			       "strike", strike_deg,
			       "smajax", smajax,
			       "sminax", sminax,
			       "sdepth", sdepth,
			       "stime", stime,
			       0 );
		if( rc < 0 ) {
			elog_complain( 1, "dbputv failed on orid %d.\n", orid );
		} else if( verbose ) {
		printf( "orid %d\tndef %d\tdtype %s\tconf %5.3f\n",
			orid, ndef, dtype, conf );
		printf( "\tsmajax %5.2f sminax %5.2f\tstrike %5.1f\n",
			smajax, sminax, strike_deg );
		printf( "\tsdepth %5.2f\tstime %5.2f\n",
			sdepth, stime );
		}
		

	}
	dbclose( db );

	exit( 0 );
}
