/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include "antelope_mex.h"

int pfput_mxArray( Pf *pf, char *name, mxArray *array )
{
	Pf	*sub_pf;
	double	number;
	char	*string;
	double  isinteger;
	mxArray *in[2], *out[1];
	char	warning[STRSZ];
	char	*fieldname;
	mxArray	*mxfield;
	mxArray	*mxcell;
	int	M,N;
	int	rc;
	int	i;

	if( mxIsClass( array, "dbpf" ) )
	{
		if( ! get_pf( array, &sub_pf ) )
		{
			return PFINVALID;
		}

		if( sub_pf->type == PFFILE) 
		{
			/* Don't embed a PFFILE in something else */
			sub_pf->type = PFARR;
		}

    		switch (pf->type) {
        	case PFFILE:
        	case PFARR:
            		setarr ( pf->value.arr, name, sub_pf ) ;
            		break ;

        	case PFTBL:
            		settbl ( pf->value.tbl, (int) name, sub_pf ) ;
            		break ;
	
        	default :
			return PFINVALID;
        	}
		antelope_mex_clear_register( 1 );
	}
	else if( mxIsDouble( array ) )
	{
		if( ! get_scalar( array, &number ) )
		{
			return PFINVALID;
		}
	
		in[0] = (mxArray *) array; /* Input scalar */
		mexCallMATLAB( 1, out, 1, in, "floor" );
		in[1] = out[0];  /* floor( Input scalar ) */
		mexCallMATLAB( 1, out, 2, in, "eq" );
		get_scalar( out[0], &isinteger );
		mxDestroyArray( out[0] );
		mxDestroyArray( in[1] );
	
		if( (int) isinteger ) 
		{
			pfput_int( pf, name, (int) number );
		} 
		else 
		{
			pfput_double( pf, name, number );
		}
		antelope_mex_clear_register( 1 );
	}
	else if( mxIsChar( array ) )
	{
		if( ! get_string( array, &string ) )
		{
			sprintf( warning, 
			  "failed to extract string for parameter %s\n",
			  name );
			mexWarnMsgTxt( warning );
			return PFINVALID;
		}

		pfput_string( pf, name, string );

		antelope_mex_clear_register( 1 );

		mxFree( string );
	}
	else if( mxIsStruct( array ) )
	{
		if( mxGetNumberOfDimensions( array ) > 2 ) 
		{
			sprintf( warning,
			  "structure has too many dimensions for parameter %s\n",
			  name );
			mexWarnMsgTxt( warning );
			return PFINVALID;
		}
		else if( mxGetM( array ) != 1 || mxGetN( array ) != 1 )
		{
			sprintf( warning,
			  "structure has too many elements for parameter %s\n",
			  name );
			mexWarnMsgTxt( warning );
			return PFINVALID;
		}
		N = mxGetNumberOfFields( array );

		sub_pf = pfnew( PFARR );

		for( i = 0; i < N; i++ ) 
		{
			fieldname = (char *) mxGetFieldNameByNumber( array, i );
			mxfield = mxGetFieldByNumber( array, 0, i );
			rc = pfput_mxArray( sub_pf, fieldname, mxfield );

			if( rc == PFINVALID )
			{
				pffree( sub_pf );
				return PFINVALID;
			}
		}

    		switch (pf->type) {
        	case PFFILE:
        	case PFARR:
            		setarr ( pf->value.arr, name, sub_pf ) ;
            		break ;

        	case PFTBL:
            		settbl ( pf->value.tbl, (int) name, sub_pf ) ;
            		break ;
	
        	default :
            		pffree( sub_pf );
			return PFINVALID;
        	}
		antelope_mex_clear_register( 1 );
	}
	else if( mxIsCell( array ) )
	{
                if( mxGetNumberOfDimensions( array ) > 2 )
                {
                        sprintf( warning,
                          "cell array has too many dimensions for parameter %s\n",
                          name );
                        mexWarnMsgTxt( warning );
                        return PFINVALID;
                }
                M = mxGetM( array );
                N = mxGetN( array );

		sub_pf = pfnew( PFTBL );
		for( i = 0; i < M * N; i++ ) 
		{
			mxcell = mxGetCell( array, i );
			rc = pfput_mxArray( sub_pf, (char *) i, mxcell );

			if( rc == PFINVALID )
			{
				pffree( sub_pf );
				return PFINVALID;
			}
		} 
    		switch (pf->type) {
        	case PFFILE:
        	case PFARR:
            		setarr ( pf->value.arr, name, sub_pf ) ;
            		break ;

        	case PFTBL:
            		settbl ( pf->value.tbl, (int) name, sub_pf ) ;
            		break ;
	
        	default :
            		pffree( sub_pf );
			return PFINVALID;
        	}
		antelope_mex_clear_register( 1 );
	}
	else 
	{
		return PFINVALID;
	}
	
	return 0;
}
