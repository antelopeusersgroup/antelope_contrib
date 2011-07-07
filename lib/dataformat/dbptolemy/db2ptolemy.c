/** db2ptolemy
 *
 *  This library produces a string representing a datascope table (or view)
 *  in the Ptolemy II expression language.  The table is represented as a
 *  list (array) of records, where each record has a field corresponding to
 *  each field in the Datascope table (view).
 *
 *  Other ideas: Instead of writing many db2* programs, we might be able to
 *  write a db2fmt program that reads a parameter file containing a description
 *  of how a table is to be transformed to a textual representation.  The
 *  morph(3) functions combined with a parameter file describing the xformation
 *  could make this quite easy.
 *
 *  Alternatively this could be accomplished using db2xml and XSLT.
 *
 *  Based on db2xml by Kent Lindquist, Lindquist Consulting, Inc.
 *  2004-08-02 Tobin Fricke <tobin@splorg.org> University of California
 *
 *  For information on Ptolemy II, see http://ptolemy.eecs.berkeley.edu/ .
 */

#include <stdio.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "bns.h"
#include "dbptolemy.h"

static char *Library_Name = "db2ptolemy";

static void 
safe_strsub( char *source, char *pattern, char *replacement, char *dest )
{
	char 	*copy;

	allot( char *, copy, strlen( source ) + STRSZ );

	strsub( source, pattern, replacement, copy );
	
	strcpy( dest, copy );

	free( copy );
}

static void
add_dataelement( void **vstack, char *tagname, char *value )
{
	char	*copy;

	copy = strdup( value );
	strtrim( copy );

/*	safe_strsub( copy, "\"", "\\\"", copy ); /* escape double quotes */

        pushstr( vstack, " ");
        pushstr( vstack, tagname );
	pushstr( vstack, " = " );
	pushstr( vstack, copy );
	pushstr( vstack, "");

	free( copy );
}

int
db2ptolemy( db, fields_in, expressions_in, ptexp, flags )
Dbptr 	db; 
Tbl 	*fields_in;
Tbl 	*expressions_in;
void 	**ptexp;
int 	flags;
{
	Bns	*xml_bns = 0;
	char	*xmlstring = 0;
	char	*field; 
	char	*fieldname; 
	char	*vstack = 0;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;
	int	i;
	int	n;
	int	setflag = 0; 
	char	*field_separator = ",";
 	char	*row_separator = ", ";
	char	*indent = "";
        char    *table_start = "{";
        char    *table_end = "}\n";
	char	*row_start = "{";
	char	*row_end = "}";
	Expression **expr; 
	Dbvalue	result;
	char	temp[STRSZ]; 
	int 	retcode = 0; 
	int	ns, ne; 

	if( db.table < 0 ) {
		elog_complain( 0, "%s: not a view or a table\n", Library_Name );
		return -1;
	}

	if( fields_in == 0 && expressions_in == 0 ) {

		dbquery( db, dbTABLE_FIELDS, &result );

		fields = expressions = result.tbl;

	} else if( expressions_in == 0 ) {

		expressions = fields = fields_in;

	} else if( fields_in == 0 ) {

		elog_complain( 0, 
		"%s: must specify field names with nonzero list "
		"of expressions\n", Library_Name );
		return -1;

	} else {

		fields = fields_in;
		expressions = expressions_in;
	}

	if( maxtbl( fields ) != maxtbl( expressions ) ) {

		elog_complain( 0, 
		"%s: number of fields must match number of expressions\n",
                Library_Name );
		return -1;
	}

	dbget_range ( db, &ns, &ne ); 

	n = maxtbl( expressions );
	allot ( Expression **, expr, n ); 
	for (i = 0; i < n; i++)
	{
		field = (char *) gettbl( expressions , i);
		if ( dbex_compile ( db, field, &(expr[i]), 0 ) < 0 ) {
		  	return -1;
		}
	}

        pushstr( (void **) &vstack, table_start);

	for (db.record = ns; db.record < ne; db.record++ ) {

                pushstr( (void **) &vstack, row_start );

		for ( i=0; i<n; i++ ) {
			fieldname = (char *) gettbl( fields, i);
			if (dbex_eval ( db, expr[i], setflag, &result ) < 0 ) {
				retcode = -1;
				sprintf( temp, "***Bad Expression***"); 
			} else {
				switch ( expr[i]->type ) {
				case dbBOOLEAN: 
			    		sprintf ( temp, "%-5s", result.i ? "true" : "false" ); 
			    		break;
				case dbYEARDAY:
				case dbINTEGER:  
			    		sprintf ( temp, "%8d", result.i ); 
			    		break;
				case dbTIME:
			    		sprintf ( temp, "%17.5lf", result.d ); 
			    		break; 
				case dbREAL:
			    		sprintf ( temp, "%10.5g", result.d ); 
			    		break;
				case dbSTRING:
			    		sprintf ( temp, "%-10s", result.t ); 
			    		free(result.t);
			    		result.t = 0;
			    		break;
				default:
			    		sprintf ( temp, "**INVALID**" ); 
			    		break;
				}
			}

			if ( indent != 0 ) {

				pushstr( (void **) &vstack, indent );
				pushstr( (void **) &vstack, indent );
			}

			add_dataelement( (void **) &vstack, fieldname, temp );

			if ( field_separator != 0 && i < (n - 1) ) {

				pushstr( (void **) &vstack, field_separator );
			}
		}

		pushstr( (void **) &vstack, indent );
                pushstr( (void **) &vstack, row_end );
                if (db.record < ( ne - 1))
		  pushstr( (void **) &vstack, row_separator );
	}
        
        pushstr( (void **) &vstack, table_end );

	for ( i=0; i<n; i++ ) {
		dbex_free ( expr[i] ); 
	}

	free(expr);

	xmlstring = popstr( (void **) &vstack, 1 );

	if( flags & DBPTOLEMY_BNS ) {

		xml_bns = bnsnew( -1, strlen( xmlstring ) + 2 );

		buf2bns( xml_bns, xmlstring, strlen( xmlstring ) + 1 );

		free( xmlstring );

		*ptexp = (void *) xml_bns;

	} else {
		
		*ptexp = (void *) xmlstring;
	}

	return retcode;
}
