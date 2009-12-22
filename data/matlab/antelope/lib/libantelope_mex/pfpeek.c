/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include "antelope_mex.h"

/* Peek ahead to see what the named field is, and whether it's there */

int
pfpeek( Pf *pf, char *name, Pf **value )
{
	
	switch( pf->type )
	{
	case PFFILE:
	case PFARR:
		*value = (Pf *) getarr( pf->value.arr, name );
		break;
	case PFTBL:
		*value = (Pf *) gettbl( pf->value.tbl, (long) name );
		break;
#ifdef PFPROMPT
	case PFPROMPT:
#endif
	case PFSTRING:
		/* Can't have named child of a prompt or a string */
		*value = 0;
		break;
	}

	if( *value == 0 ) 
	{
		return PFINVALID;
	}
	else 
	{
		return (*value)->type;
	}
}
