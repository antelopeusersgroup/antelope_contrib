/*
 * datasize.c
 *
 * Utility function to allow multiple data types in Earthworm system
 *
 * Added by:
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * May, 1996
 */

#include <stdlib.h>
#include <string.h>

#define STREQ(a, b) (strcmp((a), (b)) == 0)

/***********************************************************************
 * datasize() Return size of data in bytes for indicated CSS data type *
 ***********************************************************************/
size_t
datasize( char *type )
{
	if( STREQ( type, "s2" ) )
	{
		return (size_t) 2;
	}
	else if( STREQ( type, "s4" ) )
	{
		return (size_t) 4;
	} 
	else if( STREQ( type, "t4" ) )
	{
		return (size_t) 4;
	}
	else if( STREQ( type, "i2" ) )
	{
		return (size_t) 2;
	}
	else if( STREQ( type, "i4" ) )
	{
		return (size_t) 4;
	} 
	else if( STREQ( type, "f4" ) )
	{
		return (size_t) 4;
	} 
	else
	{
		/* Data type not supported */

		return 0;
	}
}

