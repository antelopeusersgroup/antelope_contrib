/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include <stdlib.h>
#include "antelope_mex.h"

void
get_malloced_string( const mxArray *array_ptr, char **buf )
{
	int	len;

	len = (mxGetM(array_ptr)*mxGetN(array_ptr)) + 1;
	*buf = mxCalloc( len, sizeof( char ) );
	mxGetString( array_ptr, *buf, len );
}
