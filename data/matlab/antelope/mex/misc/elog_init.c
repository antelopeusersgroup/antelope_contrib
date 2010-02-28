/* 
 *   Antelope Toolbox for Matlab
 *                                                                     
 *   Copyright (c) 2006 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 *   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 *   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
 *   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
 *   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
 *   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 *   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 *   This software may be used freely in any way as long as 
 *   the copyright statement above is not removed. 
 *
 */

#define USAGE "Error using ==> elog_init\n\n\
Usage: RC = ELOG_INIT ( [ARGV0] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	int	rc;
	int	argc = 1;
	char	**argv;

	allot( char **, argv, 1 );

	if( nrhs == 0 ) 
	{
		argv[0] = strdup( "Matlab" );
	} 
	else if( nrhs == 1 ) 
	{

		if( ! mtlb_get_string( prhs[0], &argv[0] ) ) 
		{
			free( argv );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}
	else
	{
		free( argv );
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	rc = elog_init( argc, argv );

	elog_set( ELOG_CALLBACK, 0, antelope_mex_elog_callback );

	strcpy( Program_Name, argv[0] );

	plhs[0] = CreateDouble( (double) rc );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "elog_init: failed to create return value" );
	}
}
