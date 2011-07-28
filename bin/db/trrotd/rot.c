#include <stdio.h>
#include <math.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
rot(tr,angle)
Dbptr           tr;
double 		angle;
{
    float          *data,*data_east,*data_north;
    int             rs,bs,
                    re,be;
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

    angle = angle/57.296;
    cosphi = cos(angle);
    sinphi = sin(angle);
 
    dbget_range(tr, &rs, &re);

    for (tr.record = rs; tr.record < re; tr.record++) {
	dbgetv ( tr, 0, "bundletype", &bundletype, 0 ) ;
	if (bundletype != 0 ) elog_die(0, "rot: bundletype != 0");

	if (dbgetv(tr, 0,
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
		calib = 1.;
		dbputv(tr, 0,
		    "calib", calib,
		    0);
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
	if ((chan[2]=='E') || (chan[2]=='2')){
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
	if ((chan[2]=='N') || (chan[2]=='1')){
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

	/* calculate new components */
    if ((eastflag==1) && (northflag==1)){
	for ( i=0 ; i<nsamp ; i++ ) {
/* fix sensor orientation then rotate */
	dn_true = cosn*data_north[i] + cose*data_east[i];
	de_true = sinn*data_north[i] + sine*data_east[i];
	temp = dn_true*cosphi + de_true*sinphi;
	data_east[i] = de_true*cosphi - dn_true*sinphi;	/* Changed 2nd term sign from RM's version */
	data_north[i] = temp;			/* Radial */
	data_east[i] = data_east[i]*(-1.0);  	/* Tangential */
	}
	/* now put back into trace */
	for (tr.record = rs; tr.record < re; tr.record++)  {
		if (dbgetv(tr, 0,
	 	"data", &data,
	  	"nsamp", &nsamp,
	  	"samprate", &samprate,
	  	"chan", chan,
	   	0)!=0) elog_die(0,"rot:dbgetv problem\n");

	    	if  ((chan[2]=='E') || (chan[2]=='2')) {
			for ( i=0 ; i<nsamp ; i++ ){
				data[i] = data_east[i];
			}
			chan[2]='T';
			dbputv(tr, 0,
			    "chan", chan,
			    0);
		}
	    	if ((chan[2]=='N') || (chan[2]=='1')) {
			for ( i=0 ; i<nsamp ; i++ ){
				data[i] = data_north[i];
			}
			chan[2]='R';
			dbputv(tr, 0,
			    "chan", chan,
			    0); 
		}
	}
	free(data_north);
	free(data_east);
    } else {
	fprintf(stderr, "trrotd.rot:  Missing a component\n");
	if (eastflag==1) free(data_east);
	if (northflag==1) free(data_north);
    }
}
