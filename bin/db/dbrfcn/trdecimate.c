#include <stdio.h>
#include <math.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int trdecimate(tr, idecim)
/* Decimate a trace-set */
Dbptr tr;
int  idecim;			/* Decimation factor (safest if pwr of 2) */
{
    int rs, re, bs, be, retcode=0;
    Dbptr  bundle;
    int bundletype;
     int             nsamp, i, length2, nout, lengthout;
    double          time,
                    endtime;
    double          samprate,
                    calib, freq, fcorner, famp;
    char            sta[10],
                    chan[10];
    char            orig_datatype[10],
		    segtype[10];
    float          *data, *result;

    dbget_range(tr, &rs, &re);
    for (tr.record = rs; tr.record < re; tr.record++)
    {
	dbgetv(tr, 0,
	       "bundletype", &bundletype,
	       0);

	switch (bundletype)
	  {
	  case 0:
	    dbgetv(tr, 0,
		   "sta", sta,
		   "chan", chan,
		   "time", &time,
		   "endtime", &endtime,
		   "nsamp", &nsamp,
		   "samprate", &samprate,
		   "data", &data,
		   0);

	/*******  STEPS TO TAKE:
		find pwr of 2 above nsamp
		dump "data" to another array
		FFT, apply filter, reset nsamp, IFFT, load back into data
		reset nsamp, endtime, samprate
	*/
	    for(i=1;((int)pow((double)2,(double)i)) < nsamp;++i);
	    length2=(int)pow((double)2,(double)(i+1));
	
  	    nout = nsamp / idecim;
	    if (nout < 2) {
		fprintf(stderr, "Insufficient data %d for resample\n",nout);
		return(-1);
	    }
	    for(i=1;((int)pow((double)2,(double)i)) < nout;++i);
	    lengthout=(int)pow((double)2,(double)(i+1));

	    if((result=(float *)calloc((unsigned)(length2+2),sizeof(float))) == NULL)
	    {
		fprintf(stderr,"Error allocating space for result in decimate\n");
		return(-2);
	    }
	    for (i=0; i<nsamp; i++) result[i]=data[i];
	    for (i=nsamp; i<length2; i++) result[i] = data[0];
	    /*FFT datastreams and process */
	    cfftr(result,length2);

	    /* Figure out nyquist, corner frequencies... */
	    
	    fcorner = (double)lengthout/2.0;		/* SAFE 50% of nyquist-frequency */
	    for (i=0, freq=0.; i<lengthout; i+= 2, freq+= 2.0) {

		famp = freq*freq/(fcorner*fcorner);
		famp = 1./( (famp+1)*((famp+1)*(famp+1) - 3.*famp) );   /* 6-pole 0-phase BWTH */
		result[i] *= famp;
		result[i+1] *= famp;
	    }
	    /* zero out nyquist frequency, just to be sure, and IFFT */
	    result[lengthout] = 0.;
	    result[lengthout+1] = 0.;
	    cfftri(result, lengthout);
	    for(i=0;i<lengthout;i++)result[i]*=(2.0/lengthout);    /* rescale */

	    /*  reload and reset database values */
	    for(i=0;i<nout;i++) data[i]=result[i];  
	    samprate /= (double)idecim;
	    endtime = time  + (double)(nout-1) / samprate; 
	    dbputv(tr, 0,
		"nsamp", nout,
		"endtime", endtime,
		"samprate", samprate,
		0);



	    free ( (char *)result);
	    break;

	  default:
	    dbgetv(tr, 0,
		   "bundle", &bundle,
		   0);
	    retcode += trdecimate(bundle, idecim);
	    break;
	  }

      }
    return retcode;
}

