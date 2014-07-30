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

#define USAGE "Error using ==> elog_mark\n\n\
Usage: N = ELOG_MARK ( )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	int	n;

	if( nlhs > 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 0 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	n = elog_mark();

	plhs[0] = CreateDouble( (double) n );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "elog_mark: failed to create return value" );
	}
}
