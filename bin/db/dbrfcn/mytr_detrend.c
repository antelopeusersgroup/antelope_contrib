/* Detrend a trace set */
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
mytr_detrend(tr)
Dbptr           tr;
{
    float          *data;
    int             rs,bs,
                    re,be, i;
    Dbptr           bundle;
    int             bundletype;
    int             retcode = 0;
    int             nsamp;
    double	    d0, d1, samprate ; 
    float	    x_sum,y_sum,xy_sum,x_sum2,x_bar,y_bar,intercept,slope;

    dbget_range(tr, &rs, &re);

    for (tr.record = rs; tr.record < re; tr.record++)
      {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++)
		{
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"rot:bundletype != 0");

		if (dbgetv(bundle, 0,
		 "data", &data,
		  "nsamp", &nsamp,
		   0)!=0) elog_die(0,"rot:dbgetv problem\n");

		x_sum=y_sum=xy_sum=x_sum2=0.0;
	    	for ( i=0 ; i<nsamp ; i++ ){ 
			x_sum += 1.;
			y_sum += data[i];
			xy_sum+=(i*data[i]);
			x_sum2+=(i*i);
		}
		x_bar=x_sum/nsamp;
		y_bar=y_sum/nsamp;
		slope=(xy_sum-x_sum*y_sum/nsamp)/(x_sum2-x_sum*x_sum/nsamp);
		intercept=y_bar-slope*x_bar;	

		/* Detrend data */
	    	for ( i=0 ; i<nsamp ; i++ ){ 
			data[i] -=(intercept+slope*i);
		}


	}
    }
    return (retcode);
}
