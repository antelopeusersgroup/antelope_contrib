
#include <stdlib.h>
#include "stock.h"
#include "pf.h"

#include "pfxml.h"

#define OPTNEWLINE { if( flags & PFXML_NEWLINES ) { pushstr( (void **) &vstack, "\n" ); } }
#define TBL_ELEM "pftbl_entry"

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
add_dataelement( void **vstack, char *value )
{
        char    *copy;

	allot( char *, copy, strlen( value ) + STRSZ );
	strcpy( copy, value );
        strtrim( copy );

        safe_strsub( copy, "&", "&amp;", copy );
        safe_strsub( copy, "<", "&lt;", copy );
        safe_strsub( copy, ">", "&gt;", copy );

        pushstr( vstack, copy );

        free( copy );
}

static char *
maketag( char *tagtype, char *name, int flags )
{
	char	*vstack = 0;

	pushstr( (void **) &vstack, "<" );	

	if( flags & PFXML_STRONG ) {

		if( strcmp( name, "" ) ) {

			pushstr( (void **) &vstack, name );	

		} else {

			pushstr( (void **) &vstack, TBL_ELEM );
		}

		if( tagtype != (char *) NULL && *tagtype != (char) NULL ) {
			pushstr( (void **) &vstack, " type=\"" );
			pushstr( (void **) &vstack, tagtype );
			pushstr( (void **) &vstack, "\"" );
		}

	} else {

		pushstr( (void **) &vstack, tagtype );	

		if( name != (char *) NULL && *name != (char) NULL ) {
			pushstr( (void **) &vstack, " name=\"" );
			pushstr( (void **) &vstack, name );
			pushstr( (void **) &vstack, "\"" );
		}
	}

	pushstr( (void **) &vstack, ">" );


	return popstr( (void **) &vstack, 1 );
}

static char *
endtag( char *tagtype, char *name, int flags )
{
	char	*vstack = 0;

	pushstr( (void **) &vstack, "</" );	

	if( flags & PFXML_STRONG ) {

		if( strcmp( name, "" ) ) {

			pushstr( (void **) &vstack, name );	

		} else {

			pushstr( (void **) &vstack, TBL_ELEM );
		}

	} else {

		pushstr( (void **) &vstack, tagtype );	
	}

	pushstr( (void **) &vstack, ">" );

	return popstr( (void **) &vstack, 1 );
}

char *
pf2xml( Pf *pf, char *name, char *prolog, int flags )
{
	int	type;
	char	*value;
	char 	*vstack = 0;
	char	*s = 0;
	char	*xml = 0;
	char	*tag;
	char	*key;
	int	ikey;
	int	irow;
	Tbl	*keys;
	
	if( pf == NULL ) {
		return NULL;
	}

	if( name == NULL ) {

		register_error( 0, "pf2xml: name may not be null!\n" );
		return NULL;
	}

	if( prolog != NULL ) {

		pushstr( (void **) &vstack, prolog ); 
		OPTNEWLINE;
	}

	switch( pf->type ) {
	case PFFILE:
		
		if( flags & PFXML_PRESERVE_PFFILE ) {

			tag = maketag( "pffile", name, flags );

		} else {

			tag = maketag( "pfarr", name, flags );
		}

		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}
		}

		if( flags & PFXML_PRESERVE_PFFILE ) {

			tag = endtag( "pffile", name, flags );

		} else {

			tag = endtag( "pfarr", name, flags );
		}

		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		freetbl( keys, 0 );

		break;

	case PFARR:
		
		tag = maketag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}
		}

		tag = endtag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag);
		free( tag );

		freetbl( keys, 0 );

		break;

	case PFTBL:
		tag = maketag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		for( irow = 0; irow < pfmaxtbl( pf ); irow++ ) {

			type = pfget( pf, (char *) irow, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, "", 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}

		}

		tag = endtag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );

		break;
	default:
		complain( 0, "pf2xml: unknown pf type %d\n", pf->type );
		return 0;
	}

	xml = popstr( (void **) &vstack, 1 );

	return xml;
}
