/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include <ctype.h>
#include "antelope_mex.h"

static char *True[] =
    {"yes", "ok", "y", "true", "t", "positive",
    "aye", "please", "affirmative", "+", "1"} ;
static int NTrue = sizeof(True) / sizeof(char *) ;
     
static char *False[] =
    {"no", "n", "false", "f", "negative", "nay", "-", "0"} ;
static int NFalse = sizeof(False) / sizeof(char *) ;
	  

mxArray *
mxTranslate_Boolean( const mxArray *mxboolean )
{
	mxArray	*result;
	char	*boolean_str;
	char	*s;
	double	boolean;
	int	i, n, retcode;

	if( mxboolean == (mxArray *) NULL )
	{
		return (mxArray *) NULL;
	}

	if( mxIsDouble( mxboolean ) )
	{
		get_scalar( mxboolean, &boolean );
		if( boolean == 0 )
		{
			return CreateDouble( (double) 0 );
		} 
		else
		{
			return CreateDouble( (double) -1. );
		}
	}

	get_malloced_string( mxboolean, &boolean_str );

	s = boolean_str;

	/* Sidestep possible longstanding habits of Matlab users */
	if( s[0] == '\'' && s[strlen(s)-1] == '\'' )
	{
		s++;
		s[strlen(s)-1] = '\000';
	}

	retcode = 1 ;
	n = strlen(s) ;
	for ( i=0 ; i<n ; i++ ) 
		s[i] = tolower(s[i]) ; 

	for(i=0 ; i<NTrue ; i++ ) {
		if (strcmp(s,True[i]) ==0) {
		    retcode = -1 ; 
		    break ;
		}
	}

	if ( retcode == 1 ) {
		for(i=0 ; i<NFalse ; i++ ) {
		    if (strcmp(s,False[i]) ==0) {
			retcode = 0 ; 
			break ;
		    }
		}
	}

	if ( retcode == 1 ) {
		elog_complain( 0, "Bad boolean value '%s'\n",
		    (char *) boolean_str); 
	}

	mxFree( boolean_str );

	switch( retcode )
	{
	case -1:
		return CreateDouble( (double) -1. );
		break;
	case 0:
		return CreateDouble( (double) 0 );
		break;
	case 1:
	default:
		return (mxArray *) NULL;
		break;
	}
}
