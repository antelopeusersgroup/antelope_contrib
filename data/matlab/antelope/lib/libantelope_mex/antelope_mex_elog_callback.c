/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting, Inc.
 * 2010
 */

#include "antelope_mex.h"

int
antelope_mex_elog_callback( int severity, char *string, Tbl *Elog )
{
	Elog_msg *elog_msg;
	char	repeated[STRSZ];
	char	*message;
	int	nmessages;
	int	i;

	nmessages = maxtbl( Elog );

	for( i = 0; i < nmessages; i++ )
	{
		elog_msg = (Elog_msg *) gettbl( Elog, i );
		
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

		message = (char *) mxCalloc( 
		  strlen( elog_msg->msg ) + strlen( repeated ) + 1,
		  sizeof( char ) );

		sprintf( message, "%s%s", elog_msg->msg, repeated );

		if( severity == ELOG_NOTIFY ) {

			mexPrintf( "%s", message );

		} else if( severity >= ELOG_COMPLAIN ) {

			mexWarnMsgTxt( message );
		}

		mxFree( message );
	}

	return 1;
}

