
/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 2001
 */

#include <stdio.h>
#include "antelope_mex.h"

Dbptr
dbprocess_error( Dbptr db, char *s )
{
	char warning[STRSZ];

	sprintf( warning, "dbprocess: statement not recognized: %s", s );
		
	mexWarnMsgTxt( warning );

	return dbinvalid();
}
