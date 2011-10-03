/*  Get min/max/N/ave from each record in a TR object */
#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
trwavestats (tr)
     Dbptr           tr;
{
  float          *data;
  int             rs, re;
  Dbptr           bundle;
  int             bundletype;
  int             retcode = 0;
  int             nsamp;
  double	  mean , dmin, dmax, dmax2;
  int             imn=0,imx=0,imx2=0;
  int		  i, bs, be, ntot ;
  char            datatype[4];

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
	     "datatype", datatype,
	     0);
      ntot += nsamp ; 
      for ( i=0 ; i<nsamp ; i++ ) { 
	if (i==1) {
	  dmin = data[i];
	  dmax = data[i];
	  mean = 0.;
	} else {
	  if (data[i]<dmin) { 
	    dmin=data[i];
	    imn=i;
	  }
	  if (data[i]>dmax) {
	    dmax=data[i];
	    imx=i;
	  }
	}
	mean += data[i] ;
      }
      dmax2 = dmin;
      for ( i=0; i<nsamp; i++)
	if (data[i]>dmax2 && data[i]<dmax) {
	  dmax2=data[i];
	  imx2=i;
	}

      mean = mean / nsamp ;

      printf("record %d type: %s N: %d mean: %.1f min: %.1f @ %d max: %.1f @ %d max-1: %.1f @ %d\n", tr.record, datatype, nsamp, mean, dmin, imn, dmax, imx, dmax2, imx2);
    }
  return 0;
}
