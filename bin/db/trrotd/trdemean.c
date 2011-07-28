/* Copyright (c) 1993-1996 University of Colorado */
/* All rights reserved */

#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
trdemean1 (tr)
Dbptr           tr;
{
    float          *data;
    int             rs,
                    re;
    Dbptr           bundle;
    int             bundletype;
    int             retcode = 0;
    int             nsamp;
    double	    mean ;
    int		    i, bs, be, ntot ;

    dbget_range(tr, &rs, &re);
    printf ("... trace range is %d to %d\n",rs, re);

    for (tr.record = rs; tr.record < re; tr.record++)
    {
	    dbgetv(tr, 0,
	       "bundletype", &bundletype,
	       0);

	    if (bundletype != 0) elog_die(0,"trdemean:bundletype != 0");
	    dbgetv(tr, 0,
		       "data", &data,
		       "nsamp", &nsamp,
		       0);
	    ntot += nsamp ; 
	    for ( i=0 ; i<nsamp ; i++ ) 
		    mean += data[i] ; 
    }
    mean = mean / ntot ; 
    for (tr.record = rs; tr.record < re; tr.record++)
    {
	    dbgetv(tr, 0,
		       "data", &data,
		       "nsamp", &nsamp,
		       0);
	    for ( i=0 ; i<nsamp ; i++ ) 
		    data[i] -= mean ; 
    }
    return retcode;
}
