#include <stdlib.h>
#include "iceworm_extensions.h"

/* Change working directory to environment variable EW_PARAMS value
 ***************************************************************/

void
chdir_ewparams( char *program )
{
	char        *runPath;

   	runPath = getenv( "EW_PARAMS" );

   	if ( runPath == NULL )
   	{
      		printf( "%s: Environment variable EW_PARAMS not defined.", program );
      		printf( " Exiting.\n" );
      		exit( -1 );
   	}

   	if ( *runPath == '\0' )
   	{
      		printf( "%s: Environment variable EW_PARAMS ", program );
      		printf( "defined, but has no value. Exiting.\n" );
      		exit( -1 );
   	}

   	if ( chdir( runPath ) == -1 )
   	{
      		printf( "%s: Params directory not found: %s\n", program, runPath );
      		printf( "%s: Reset environment variable EW_PARAMS.", program );
      		printf( " Exiting.\n" );
      		exit( -1 );
   	}
}
