
#include <stdlib.h>
#include "stock.h"
#include "pf.h"

#include "pfxml.h"

#define OPTNEWLINE { if( flags & PFXML_NEWLINES ) { pushstr( (void **) &vstack, "\n" ); } }

static char *
maketag( char *tagtype, char *name )
{
	char	*vstack = 0;

	pushstr( (void **) &vstack, "<" );	
	pushstr( (void **) &vstack, tagtype );	

	if( name != (char *) NULL && *name != (char) NULL ) {
		pushstr( (void **) &vstack, " name=\"" );
		pushstr( (void **) &vstack, name );
		pushstr( (void **) &vstack, "\"" );
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

	if( prolog != NULL ) {

		pushstr( (void **) &vstack, prolog ); 
		OPTNEWLINE;
	}

	switch( pf->type ) {
	case PFFILE:
		
		tag = maketag( "pffile", name );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				pushstr( (void **) &vstack, "</pfstring>" ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}
		}

		pushstr( (void **) &vstack, "</pffile>" ); 
		OPTNEWLINE;

		break;

	case PFARR:
		
		tag = maketag( "pfarr", name );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				pushstr( (void **) &vstack, "</pfstring>" ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}
		}

		pushstr( (void **) &vstack, "</pfarr>" ); 
		OPTNEWLINE;

		break;

	case PFTBL:
		tag = maketag( "pftbl", name );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		for( irow = 0; irow < pfmaxtbl( pf ); irow++ ) {

			type = pfget( pf, (char *) irow, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", "" );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				pushstr( (void **) &vstack, "</pfstring>" ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, "", 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}

		}

		pushstr( (void **) &vstack, "</pfarr>" ); 
		OPTNEWLINE;

		break;
	default:
		complain( 0, "pf2xml: unknown pf type %d\n", pf->type );
		return 0;
	}

	xml = popstr( (void **) &vstack, 1 );

	return xml;
}
