/* 
 * db2xml.c
 * Kent Lindquist
 * Lindquist Consulting
 * 2003-2004
 */

#include <stdio.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "bns.h"
#include "dbxml.h"

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
add_starttag( void **vstack, char *tagname )
{
	char	*copy;

	copy = strdup( tagname );
	strtrim( copy );

	pushstr( vstack, "<" );
	pushstr( vstack, copy );
	pushstr( vstack, ">" );

	free( copy );
}

static void
add_endtag( void **vstack, char *tagname )
{
	char	*copy;

	copy = strdup( tagname );
	strtrim( copy );

	pushstr( vstack, "</" );
	pushstr( vstack, copy );
	pushstr( vstack, ">" );

	free( copy );
}

static void
add_dataelement( void **vstack, char *tagname, char *value )
{
	char	*copy;

	copy = strdup( value );
	strtrim( copy );

	safe_strsub( copy, "&", "&amp;", copy );
	safe_strsub( copy, "<", "&lt;", copy );
	safe_strsub( copy, ">", "&gt;", copy );

	add_starttag( vstack, tagname );
	pushstr( vstack, copy );
	add_endtag( vstack, tagname );

	free( copy );
}

static void
add_fieldname( Tbl *fieldnames, char *tablename, char *fieldname )
{
	char	qualified[STRSZ];

	sprintf( qualified, "%s.%s", tablename, fieldname );

	pushtbl( fieldnames, strdup( qualified ) );

	return;
}

static Tbl *
get_fieldnames( Dbptr db, int flags )
{
	Tbl	*fieldnames;
	Tbl	*view_tables;
	Tbl	*table_fields;
	char	*tablename;
	char	*fieldname;
	char	afield[STRSZ];
	Dbvalue	result;
	int	itable;
	int	ifield;
	static Hook *hook = 0;
	int	start;
	int	nchars;
	int	is_view;

	fieldnames = newtbl( 0 );
	view_tables = newtbl( 0 );

	dbquery( db, dbTABLE_IS_VIEW, &is_view );

	if( is_view ) {

		dbquery( db, dbVIEW_TABLES, &view_tables );

		view_tables = duptbl( view_tables, strdup );

	} else {

		dbquery( db, dbTABLE_NAME, &result );

		view_tables = strtbl( strdup( result.t ), 0 );
	}

	for( itable = 0; itable < maxtbl( view_tables ); itable++ ) {

		tablename = gettbl( view_tables, itable );

		db = dblookup( db, "", tablename, "", "" );

		if( flags & DBXML_PRIMARY ) {

			dbquery( db, dbPRIMARY_KEY, &table_fields );

		} else {

			dbquery( db, dbTABLE_FIELDS, &table_fields );
		}

		for( ifield = 0; ifield < maxtbl( table_fields ); ifield++ ) {
			
			fieldname = gettbl( table_fields, ifield );

			if( strcontains( fieldname, "::", &hook,
					 &start, &nchars ) ) {
				
				strcpy( afield, fieldname );
				afield[start] = '\0';
				fprintf( stderr, "SCAFFOLD: adding %s\n", afield);
				add_fieldname( fieldnames, tablename, afield );

				strcpy( afield, fieldname + start + 2 );
				fprintf( stderr, "SCAFFOLD: adding %s\n", afield);
				add_fieldname( fieldnames, tablename, afield );

			} else {

				add_fieldname( fieldnames, tablename, fieldname );
			}
		}
	}

	freetbl( view_tables, free );

	return fieldnames;
}

int
db2xml( db, rootnode, rownode, fields_in, expressions_in, xml, flags )
Dbptr 	db; 
char 	*rootnode;
char 	*rownode;
Tbl 	*fields_in;
Tbl 	*expressions_in;
void 	**xml;
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
	char	*separator = "\n";
	char	*indent = "   ";
	char	*roottag = 0;
	char	*rowtag = 0;
	char	*default_rowtag = "row";
	Expression **expr; 
	Dbvalue	result;
	char	temp[STRSZ]; 
	int 	retcode = 0; 
	int	ns, ne; 
	int	free_fieldnames = 0;

	if( db.table < 0 ) {
		register_error( 0, "db2xml: not a view or a table\n" );
		return -1;
	}

	if( ( flags & DBXML_PRIMARY ) && 
	    ( fields_in != 0 || expressions_in != 0 ) ) {

		register_error( 0, "db2xml: fields are explicitly specified; "
				   "ignoring useless request for primary keys\n" );
	}

	if( rootnode != 0 && *rootnode != 0 ) {
		
		roottag = strdup( rootnode );

	} else {
		
		dbquery( db, dbTABLE_NAME, &result );
		roottag = strdup( result.t );
	} 
	add_starttag( (void **) &vstack, roottag );
	pushstr( (void **) &vstack, separator );

	if( rownode != 0 && *rownode != 0 ) {
		
		rowtag = strdup( rownode );

	} else {
		
		rowtag = strdup( default_rowtag );
	} 

	if( fields_in == 0 && expressions_in == 0 ) {

		fields = expressions = get_fieldnames( db, flags );

		free_fieldnames++;


	} else if( expressions_in == 0 ) {

		expressions = fields = fields_in;

	} else if( fields_in == 0 ) {

		register_error( 0, 
		"db2xml: must specify field names with nonzero list "
		"of expressions\n" );
		return -1;

	} else {

		fields = fields_in;
		expressions = expressions_in;
	}

	if( maxtbl( fields ) != maxtbl( expressions ) ) {

		register_error( 0, 
		"db2xml: number of fields must match number of expressions\n" );
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

	for (db.record = ns; db.record < ne; db.record++ ) {

		pushstr( (void **) &vstack, indent );
		add_starttag( (void **) &vstack, rowtag );
		pushstr( (void **) &vstack, separator );

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

			if ( separator != 0 ) {

				pushstr( (void **) &vstack, separator );
			}
		}

		pushstr( (void **) &vstack, indent );
		add_endtag( (void **) &vstack, rowtag );
		pushstr( (void **) &vstack, separator );
	}

	if( free_fieldnames ) {

		freetbl( fields, free );
	}

	for ( i=0; i<n; i++ ) {
		dbex_free ( expr[i] ); 
	}

	free(expr);

	add_endtag( (void **) &vstack, roottag );
	pushstr( (void **) &vstack, separator );

	free( roottag );
	free( rowtag );

	xmlstring = popstr( (void **) &vstack, 1 );

	if( flags & DBXML_BNS ) {

		xml_bns = bnsnew( -1, strlen( xmlstring ) + 2 );

		buf2bns( xml_bns, xmlstring, strlen( xmlstring ) + 1 );

		free( xmlstring );

		*xml = (void *) xml_bns;

	} else {
		
		*xml = (void *) xmlstring;
	}

	return retcode;
}
