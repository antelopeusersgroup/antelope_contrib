#include <stdlib.h>
#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "dbxml.h"

#define XML2DB_ERROR -1
#define XML2DB_NOTFOUND 0
#define XML2DB_START 1
#define XML2DB_END 2
#define XML2DB_EMPTY 3

int 
get_token( char *source, char *next )
{
	static Hook *hook = 0;
	char	*pattern = "</?[^>]+/?>";
	int	start;
	int	nchars;
	int	rc;

	rc = strcontains( source, pattern, &hook, &start, &nchars);

	if( rc == 0 ) {

		return XML2DB_NOTFOUND;

	} else if( rc < 0 ) {

		return XML2DB_ERROR;
	} 

	if( *(source + start + nchars - 2) == '/' ) {
		
		return XML2DB_EMPTY;

	} else if( *(source + start + 1) == '/' ) {

		return XML2DB_END;

	} else {

		return XML2DB_START;
	}
}

int
xml2db( Dbptr db, char *xml )
{
	char	*p; 
	char	*string;
	char	*next;
	int	rc;

	string = xml;

	rc = get_token( string, next );

	printf( "tag '%s': rc %d\n", string, rc );
}
