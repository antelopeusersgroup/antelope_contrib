/* Manage the receiverfunction calculation:  standard freq-domain
	Uses "rfcn.c" and some FFT routines from ah-lib
	On input, hopes traces are in order "BHT BHR BHZ" or similar
	On output, replaces R by R/Z (the reciever function)
		T by T/Z (tangential receiver function)
		Z by Z/Z (check on procedure)

	11/27/00   modified so saves amplitude rescaling (correcting for filtering) in calib  GAA
 */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "db.h"
#include "tr.h"

extern float *rfcn();
extern int mytr_detrend();

int rfcn_calc(tr, phshift, wlev, gfreq, hpfreq)
Dbptr tr;
float phshift, wlev, gfreq, hpfreq;
{
	float *rfcn_r, *rfcn_t, *rfcn_z, *data, *data_r, *data_t, *data_z;
	int nin_t=0, nin_r=0, nin_z=0, nsamp;
	float delta=0, zamp;
	double samprate;
	int i, rs, re, bs, be, bundletype;
	char chan[8];
	Dbptr bundle;

    mytr_detrend(tr);
    dbget_range(tr, &rs, &re);

    for (tr.record = rs; tr.record < re; tr.record++) {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++) {
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"rot:bundletype != 0");

		if (dbgetv(bundle, 0,
		 "data", &data,
		  "nsamp", &nsamp,
		  "samprate", &samprate,
		  "chan", chan,
		   0)!=0) elog_die(0,"rfcn_calc:dbgetv problem\n");


	    	if (chan[2]=='T' || chan[2]=='t'){
	    		data_t = data;
			nin_t = nsamp;
		}
	    	if (chan[2]=='R' || chan[2]=='r'){
	    		data_r = data;
			nin_r = nsamp;
		}
	    	if (chan[2]=='Z' || chan[2]=='z'){
	    		data_z = data;
			nin_z = nsamp;
			delta = 1./samprate;
		}
	}
    }
    if (nin_t==0 || nin_r==0 || nin_z==0) elog_die(0,"rfcn_calc:not enough channels\n");

    rfcn_r = rfcn(nin_r, nin_z, data_r, data_z, delta, phshift, wlev, gfreq, hpfreq);
    rfcn_t = rfcn(nin_t, nin_z, data_t, data_z, delta, phshift, wlev, gfreq, hpfreq);
    rfcn_z = rfcn(nin_z, nin_z, data_z, data_z, delta, phshift, wlev, gfreq, hpfreq);

	/* Rescale to Z-amplitude peak */
    zamp=0.;
    for (i=0; i<nsamp; i++) 
	if (fabs((double)rfcn_z[i]) > zamp) 
		zamp = (float)fabs((double)rfcn_z[i]);
    if (zamp>0.) {
	fprintf(stdout," Rescale by %f\n",1./zamp);
   	for (i=0; i<nsamp; i++) {
	    rfcn_z[i] /= zamp;
	    rfcn_r[i] /= zamp;
	    rfcn_t[i] /= zamp;
	}
    }
    
    for (tr.record = rs; tr.record < re; tr.record++) {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++) {
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);

		if (bundletype != 0) elog_die(0,"rot:bundletype != 0");

		if (dbgetv(bundle, 0,
		 "data", &data,
		  "nsamp", &nsamp,
		  "chan", chan,
		   0)!=0) elog_die(0,"rfcn_calc:dbgetv problem2\n");


	    	if (chan[2]=='T' || chan[2]=='t'){
			for (i=0; i<nsamp; i++) data[i] = rfcn_t[i];
			strcpy(chan,"rf_T");
		}
	    	if (chan[2]=='R' || chan[2]=='r'){
			for (i=0; i<nsamp; i++) data[i] = rfcn_r[i];
			strcpy(chan,"rfcn");
		}
	    	if (chan[2]=='Z' || chan[2]=='z'){
			for (i=0; i<nsamp; i++) data[i] = rfcn_z[i];
			strcpy(chan,"rf_Z");
		}
		dbputv(bundle, 0,
		    "chan", chan,
		    "calib", zamp,
		    0);
		dbputv(tr, 0,
		    "chan", chan,
		    "calib", zamp,
		    0);

	}
    }

}
