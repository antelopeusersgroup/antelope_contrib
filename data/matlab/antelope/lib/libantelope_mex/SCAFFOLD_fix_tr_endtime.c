/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include "antelope_mex.h"
#include "tr.h"

/* trload_css and trload_cssgrp have a bug in setting the endtime 
   correctly. This bug-fix scaffolding function is necessary until
   that problem is fixed at the source. */

void
SCAFFOLD_fix_tr_endtime( Dbptr tr )
{
	double	time;
	double	endtime;
	double	old_endtime;
	double	samprate;
	int	nsamp;
	int	nrecs;

	dblookup( tr, 0, "trace", 0, 0 );
	antelope_mex_clear_register( 1 );

	dbquery( tr, dbRECORD_COUNT, &nrecs );
	antelope_mex_clear_register( 1 );

	if( nrecs <= 0 ) return;

	for( tr.record = 0; tr.record < nrecs; tr.record++ )
	{
		dbgetv( tr, 0, "time", &time,
			       "nsamp", &nsamp, 
			       "samprate", &samprate,
			       "endtime", &old_endtime,
			       0 );
		antelope_mex_clear_register( 1 );
		
		endtime = ENDTIME( time, samprate, nsamp );
		
/* 		mexPrintf( "old_endtime %f, endtime %f, diff %f\n", 
			old_endtime, endtime, endtime - old_endtime ); */
		dbputv( tr, 0, "endtime", endtime, 0 );
		antelope_mex_clear_register( 1 );
	}
}

