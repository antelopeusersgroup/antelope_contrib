/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting 
 * 1997-2003
 */

#define USAGE "Error using ==> dblist2subset\n\n\
Usage: DBPTR = DBLIST2SUBSET ( DBPTR [, LIST] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*list = 0;
	char	errmsg[STRSZ];
	int	rhs_index;
	int	type = 1;
	double 	type_fp = 1;
	int	ns; 			/* SCAFFOLD */
	int	ne; 			/* SCAFFOLD */
	int	n; 			/* SCAFFOLD */
	int	i; 			/* SCAFFOLD */
	int	list_is_scaffold = 0;	/* SCAFFOLD */

	if( nrhs < 1 || nrhs > 2 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 && ! get_inttbl( prhs[1], &list ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs == 1 ) 
	{
		/* Work around bug in dblist2subset 	   SCAFFOLD *
		dbget_range( db, &ns, &ne );		/* SCAFFOLD *
		n = ne - ns;				/* SCAFFOLD *
		list_is_scaffold++;			/* SCAFFOLD *
		list = newtbl( 0 );			/* SCAFFOLD *
		for( i = 0; i < n; i++ ) {		/* SCAFFOLD *
			pushtbl( list, (char *) i );	/* SCAFFOLD *
		}					/* SCAFFOLD */
		list = (Tbl *) NULL;
	}

	db = dblist2subset( db, list );
	antelope_mex_clear_register( 1 );

/*	if( list_is_scaffold ) { freetbl( list, 0 ); } * SCAFFOLD */

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dblist2subset: subset creation failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dblist2subset: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
