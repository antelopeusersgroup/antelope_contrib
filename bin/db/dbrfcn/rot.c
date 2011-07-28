#include <stdio.h>
#include <math.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "tr.h"
/* Rotation:  Assumes that: 
	1.  horizontal component channel names are 3 letters ending in N or E
	2.  vang=0 for horizontal component channels, +-90 for vertical channels
	3.  vertical components are rightside up (FIX ME)
	4.  horizontal components are orthogonal, and are correctly oriented if hang<-360.
	returns R-component in channel ending in "N", and T-comp in channel ending in "E"
		--> renames them to ??R, ??T
 */
int
rot(tr,angle)
Dbptr           tr;
double 		angle;
{
    float          *data,*data_east,*data_north;
    int             rs,bs,
                    re,be;
    Dbptr           bundle;
    int             bundletype;
    int             retcode = 0;
    int             nsamp;
    double	    d0, d1, samprate ; 
    double	    cosphi,sinphi,temp, cose, sine, cosn, sinn, cosz;
    int		    i ,eastflag, northflag; 
    char	    chan[8];
    double	    hang,vang, calib, dn_true, de_true;
    double	    rpd = 0.017453293;

    /* convert to radians */
    angle = angle*rpd;
    cosphi = cos(angle);
    sinphi = sin(angle);
 
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
		  "samprate", &samprate,
		  "chan", chan,
		  "calib", &calib,
		  "hang", &hang,
		  "vang", &vang,
		   0)!=0) elog_die(0,"rot:dbgetv problem\n");

		if (calib!=0.0) {
	    		for ( i=0 ; i<nsamp ; i++ ){ 
				data[i] *= calib;
			}
		}

	    	if (chan[2]=='Z'){			/* Correct reversed Z-comp */
			if (vang < -360.) {
				cosz = 1.;
			} else {
				cosz = cos(vang*rpd);
			}
	    		for ( i=0 ; i<nsamp ; i++ ){ 
				data[i] *= cosz;
			}
	  	}
	    	if (chan[2]=='E'){
	    		data_east = (float *)malloc(nsamp*sizeof(float));
			if (hang < -360.) {
				sine = 1.;
				cose = 0.;
			} else {
				sine = sin(hang*rpd);
				cose = cos(hang*rpd);
			}
	    		for ( i=0 ; i<nsamp ; i++ ){ 
				data_east[i] = data[i];
				}
			eastflag = 1;
			}
	    	if (chan[2]=='N'){
	    		data_north = (float *)malloc(nsamp*sizeof(float));
			if (hang < -360.) {
				sinn = 0.;
				cosn = 1.;
			} else {
				sinn = sin(hang*rpd);
				cosn = cos(hang*rpd);
			}
	    		for ( i=0 ; i<nsamp ; i++ ){ 
				data_north[i] = data[i];
				}
			northflag = 1;
			}
		}
	}

/* calculate new components */
 if ((eastflag==1) && (northflag==1)){
  for ( i=0 ; i<nsamp ; i++ ) 
	{
/* fix sensor orientation then rotate */
	dn_true = cosn*data_north[i] + cose*data_east[i];
	de_true = sinn*data_north[i] + sine*data_east[i];
	temp = dn_true*cosphi + de_true*sinphi;
	data_east[i] = de_true*cosphi - dn_true*sinphi;	/* Changed 2nd term sign from RM's version */
	data_north[i] = temp;			/* Radial */
	data_east[i] = data_east[i]*(-1.0);  	/* Tangential */
	}
/* now put back into trace */
    for (tr.record = rs; tr.record < re; tr.record++)
      {
	dbgetv ( tr, 0, "bundle", &bundle, 0 ) ;
    	dbget_range(bundle, &bs, &be);
    	for (bundle.record = bs; bundle.record < be; bundle.record++)
		{
		dbgetv(bundle, 0,
              		"bundletype", &bundletype,
              		0);
		if (dbgetv(bundle, 0,
	 	"data", &data,
	  	"nsamp", &nsamp,
	  	"samprate", &samprate,
	  	"chan", chan,
	   	0)!=0) elog_die(0,"rot:dbgetv problem\n");

	    /*	if ((strcmp(chan,"HHE")==0)||(strcmp(chan,"BHE")==0)||(strcmp(chan,"BLE")==0)||(strcmp(chan,"HLE")==0)){ */
	    	if (chan[2]=='E'){
			for ( i=0 ; i<nsamp ; i++ ){
				data[i] = data_east[i];
				}
			chan[2]='T';
			dbputv(bundle, 0,
			    "chan", chan,
			    0);
			}
	   /* 	if ((strcmp(chan,"HHN")==0)||(strcmp(chan,"BHN")==0)||(strcmp(chan,"BLN")==0)||(strcmp(chan,"HLN")==0)){ */
	    	if (chan[2]=='N'){
			for ( i=0 ; i<nsamp ; i++ ){
				data[i] = data_north[i];
				}
			chan[2]='R';
			dbputv(bundle, 0,
			    "chan", chan,
			    0); 
			}
		}
	}
	free(data_north);
	free(data_east);
  } else {
    fprintf(stdout, "WARNING:  Components not found; no rotation \n ");
  }
}
