#include <stdio.h>
#include <string.h>
#include <math.h>
#include "sac.h"

int
write_sac_header( FILE *outfile, sac_t *sach )

{
	if (fwrite((char *) sach, sizeof (struct sac), 1, outfile) != 1)
	  {
	    fprintf(stderr, "failed to properly write SAC header\n" );
	  }

      return sizeof( struct sac );
}
