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

mxArray *
mxPfprompt_string( char *prompt )
{
	Pf	*pf;
	mxArray *input_array[2];
	mxArray *output_array[1];
	char	*full_prompt;
	static char prompt_tag[] = " : ";

	full_prompt = (char *) mxCalloc( strlen( prompt ) +
					 strlen( prompt_tag ) + 1,
					 sizeof( char ) );

	sprintf( full_prompt, "%s%s", prompt, prompt_tag );
	input_array[0] = mxCreateString( full_prompt );
	input_array[1] = mxCreateString( "s" );
	mxFree( full_prompt );

	if( mexCallMATLAB( 1, output_array, 2, input_array, "input" ) == 0 )
	{
		mxDestroyArray( input_array[0] );
		return output_array[0];
	}
	else
	{
		mxDestroyArray( input_array[0] );
		return (mxArray *) NULL;
	}

}
