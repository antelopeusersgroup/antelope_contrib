
#include <stdlib.h>
#include "stock.h"
#include "pf.h"

#include "pfxml.h"

#define OPTNEWLINE { if( flags & PFXML_NEWLINES ) { pushstr( (void **) &vstack, "\n" ); } }
#define TBL_ELEM "pftbl_entry"

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

		pushstr( (void **) &vstack, name );	

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

	if( prolog != NULL ) {

		pushstr( (void **) &vstack, prolog ); 
		OPTNEWLINE;
	}

	switch( pf->type ) {
	case PFFILE:
		
		tag = maketag( "pffile", name, flags );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}
		}

		tag = endtag( "pffile", name, flags );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		break;

	case PFARR:
		
		tag = maketag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}
		}

		tag = endtag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag);

		break;

	case PFTBL:
		tag = maketag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 
		OPTNEWLINE;

		for( irow = 0; irow < pfmaxtbl( pf ); irow++ ) {

			type = pfget( pf, (char *) irow, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				pushstr( (void **) &vstack, value );
				tag = endtag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, "", 0, flags );
				pushstr( (void **) &vstack, xml );
				OPTNEWLINE;
			}

		}

		tag = endtag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 

		break;
	default:
		complain( 0, "pf2xml: unknown pf type %d\n", pf->type );
		return 0;
	}

	xml = popstr( (void **) &vstack, 1 );

	return xml;
}
