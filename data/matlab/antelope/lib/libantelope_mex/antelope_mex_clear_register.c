/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include "antelope_mex.h"

void
antelope_mex_clear_register( int printflag )
{
	Tbl	*messages;
	Elog_msg *elog_msg;
	char	repeated[STRSZ];
	char	*warning;
	int	nmessages;
	int	i;

	if( printflag )
	{
		elog_query( ELOG_TBL, -1, (void **) &messages );
	
		nmessages = maxtbl( messages );

		for( i = 0; i < nmessages; i++ )
		{
			elog_msg = (Elog_msg *) gettbl( messages, i );
			
			if( elog_msg->n > 1 )
			{
				sprintf( repeated, 
					 " (repeated %d times)", 
					 elog_msg->n );
			}
			else 
			{
				sprintf( repeated, "" );
			}

			warning = (char *) mxCalloc( 
			  strlen( elog_msg->msg ) + strlen( repeated ) + 1,
			  sizeof( char ) );

			sprintf( warning, "%s%s", elog_msg->msg, repeated );

			mexWarnMsgTxt( warning );

			mxFree( warning );
		}
	}

	elog_clear();
}

