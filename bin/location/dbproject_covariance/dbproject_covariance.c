#include <stdlib.h>
#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "pf.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)


void 
Usage() {
	die( 1, "Usage: dbproject_covariance [-v] [-c conf] dbname\n" );
}

main( argc, argv )
int	argc;
char	*argv[];
{
	Dbptr	db;
	Dbptr	dborigin;
	char	*dbname;
	Pf	*chisqpf;
	Tbl	*chi2tbl;
	double	*conf_list;
	Pf	*confpf;
	Tbl	*chi2_table;
	double	*chi2_values;
	double	chi2val;
	int	i,j;
	int	verbose = 0;
	double	sxx,syy,szz,stt,sxy,sxz,syz,stx,sty,stz;
	double	covar[4][4];
	double	conf = 0.683;
	int	np = 4;
	int	ndef;
	int	ndeg_of_freedom;
	int	orid;
	char	orid_str[STRSZ];
	char	dtype[10];
	double	strike_rad, strike_deg;
	double	smajax, sminax, sdepth, stime;
	int	nrecs;
	float	epstr, fstt, fstx, fsty, fstz, fsxx, fsxy;
	float	fsxz, fsyy, fsyz, fszz;
	double	epmaj, epmin, hymaj, hymid, hymin, hyplu, hyrak, hystr;
	double	zfint;
	char 	c;

	while( ( c = getopt( argc, argv, "c:v" ) ) != -1 ) {
		switch( c ) {
		case 'c':
			conf = atof( optarg );
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

	pfread( "chi2", &chisqpf );
	chi2tbl = pfget_tbl( chisqpf, "chi2" );
	allot( double *, conf_list, maxtbl( chi2tbl ) );
	for( i=0; i < maxtbl( chi2tbl ); i++ ) {
		confpf = (Pf *) gettbl( chi2tbl, i );
		conf_list[i] = pfget_double( confpf, "conf" );
	}
	for( i=0; i < maxtbl( chi2tbl ); i++ ) {
		if( conf_list[i] == conf ) {
			confpf = (Pf *) gettbl( chi2tbl, i );
			chi2_table = pfget_tbl( confpf, "values" );
			allot( double *, chi2_values, maxtbl( chi2_table )+1 );
			for( j=0; j<maxtbl(chi2_table); j++ ) {
				/* Avoid off-by-one indexing. Index 
				   by degrees-of-freedom value */
				chi2_values[j+1] = atof( gettbl(chi2_table,j) );
			}
			break;
		}
	}
	if( i >= maxtbl( chi2tbl ) ) {
		die( 1, "Couldn't find conf %f in chi2.pf\n", conf );
	} 
	
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
		} else if( ndeg_of_freedom > maxtbl( chi2_table ) ) {
			complain( 1, 
		"Exceeded max degrees of freedom in chi2.pf conf %f table (need %d, have %d)\n",
			conf, ndeg_of_freedom, maxtbl( chi2_table ) );
			continue;
		}

		chi2val = chi2_values[ndeg_of_freedom];

		covar[0][0] = stt * chi2val;
		covar[0][1] = stx * chi2val;
		covar[0][2] = sty * chi2val;
		covar[0][3] = stz * chi2val;
		covar[1][1] = sxx * chi2val;
		covar[1][2] = sxy * chi2val;
		covar[1][3] = sxz * chi2val;
		covar[2][2] = syy * chi2val;
		covar[2][3] = syz * chi2val;
		covar[3][3] = szz * chi2val;

		ellips_( &np, covar, &hymaj, &hymid, &hymin, &hystr, &hyplu,
			 &hyrak, &epmaj, &epmin, &epstr, &zfint, &fstt,
			 &fstx, &fsty, &fsxx, &fsyy, &fstz, &fsxz,
			 &fsyz, &fszz );

		smajax = epmaj;
		sminax = epmin;
		sdepth = zfint;
		stime = sqrt( covar[0][0] );
		strike_deg = epstr;

		dbputv( db, 0, "conf", conf, 
			       "strike", strike_deg,
			       "smajax", smajax,
			       "sminax", sminax,
			       "sdepth", sdepth,
			       "stime", stime,
			       0 );


		if( verbose ) {
		printf( "orid %d\tndef %d\tdtype %s\tconf %5.3f\tchi2 %5.2f\n",
			orid, ndef, dtype, conf, chi2val );
		printf( "\tsmajax %5.2f sminax %5.2f\tstrike %5.1f\n",
			smajax, sminax, strike_deg );
		printf( "\tsdepth %5.2f\tstime %5.2f\n",
			sdepth, stime );
		}
		

	}
	dbclose( db );
}
