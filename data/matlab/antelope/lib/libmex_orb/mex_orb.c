/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include "mex_orb.h"

int
mex_orbopen( char *filename, char *permissions )
{
	return orbopen( filename, permissions );
}

int
mex_orbclose( int orbfd )
{
	return orbclose( orbfd );
}

int
mex_orbtell( int orbfd )
{
	return orbtell( orbfd );
}

int
mex_orbseek( int orbfd, int which )
{
	return orbseek( orbfd, which );
}

int
mex_orbafter( int orbfd, double time )
{
	return orbafter( orbfd, time );
}

int
mex_orbselect( int orbfd, char *regex )
{
	return orbselect( orbfd, regex );
}

int
mex_orbreject( int orbfd, char *regex )
{
	return orbreject( orbfd, regex );
}

int	
mex_orbping( int orbfd, int *version )
{
	return orbping( orbfd, version );
}

int
mex_orbget( int orbfd, int code, int *pktid, char *srcname, double *time,
		char **packet, int *nbytes, int *bufsize )
{

	return orbget( orbfd, code, pktid, srcname, time,
		       packet, nbytes, bufsize );
}

int
mex_orbreap( int orbfd, int *pktid, char *srcname, double *time,
		char **packet, int *nbytes, int *bufsize )
{

	return orbreap( orbfd, pktid, srcname, time,
			packet, nbytes, bufsize );
}

int
mex_orbreap_nd( int orbfd, int *pktid, char *srcname, double *time,
		char **packet, int *nbytes, int *bufsize )
{

	return orbreap_timeout( orbfd, 0, pktid, srcname, time,
			   packet, nbytes, bufsize );
}
