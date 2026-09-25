/*Copyright (c) Peter Lombard
Berkeley Seismological Laboratory
University of California, Berkeley */

#include "tdmtinv_iso.h"
#include <string.h>
#include <math.h>

mt_dat(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,scale,Pdc,Pclvd,Piso,Mo,Mw,E,VR,depth,fname)
     int nsta, depth;
     struct MOMENT M;
     struct GREEN *gg;
     struct DATA  *ss;
     float Strike, Rake, Dip, Pdc, Pclvd, Piso, Mo, Mw, E, VR, scale;
     float St2, Rk2, Dp2;
     char *fname;
{
    int i,j, n, Z, Zg, Np, Zd, count;
    float Az, dt;
    FILE *fp;
    
    /* open data file */
    fp = fopen(fname,"w");
    if (fp == NULL) {
	fprintf(stderr, "mt_dat: error opening %s: %s\n", fname,
		strerror(errno));
	return -1;
    }
    
    /* write tdmt-run related info: */
    fprintf(fp, "#depth variance  VR       nsta\n");
    fprintf(fp, "%3d %10.3e %10.3e %3d\n", depth, E, VR, nsta);
    fprintf(fp, "#mxx        mxy        mxz        myy        myz        mzz\n");
    fprintf(fp, "%10.3e %10.3e %10.3e %10.3e %10.3e %10.3e\n", M.mxx*scale, 
	    M.mxy*scale, M.mxz*scale, M.myy*scale,M.myz*scale, M.mzz*scale);
    fprintf(fp, "#st1 rk1 dp1 st2 rk2 dp2\n");
    fprintf(fp, "%3.0f %3.0f %3.0f %3.0f %3.0f %3.0f\n", Strike, Rake, Dip, 
	    St2, Rk2, Dp2);
    fprintf(fp, "#pdc pclvd piso\n");
    fprintf(fp, "%3.0f %3.0f %3.0f\n", Pdc, Pclvd, Piso);

    for(i=0; i < nsta; i++) {
	Np = ss[i].nn;
	Z  = ss[i].zz;
	dt = ss[i].dt;
	Az = ss[i].azi;
	Zg = gg[i].zz;

	fprintf(fp, "\n#filename\n");
	fprintf(fp, "%s\n", ss[i].name);
	fprintf(fp, "#dt   npts  dist   Az   Zcor VR\n");
	fprintf(fp, "%5.3f %4d %5.1f %5.1f %4d %10.3e\n", dt, Np, ss[i].dist, 
		Az*180.0/PI, Z, ss[i].vr);
	fprintf(fp, "#data T          R          Z    synth T          R          Z\n");
	
	for(j=0; j < Np; j++) {
	    fprintf(fp, "%10.3e %10.3e %10.3e %10.3e %10.3e %10.3e\n",
		    ss[i].t[j+Z],
		    ss[i].r[j+Z],
		    ss[i].z[j+Z],
		    (M.mxx*0.5*gg[i].u1[j+Zg]*sin(2*Az) /*Tangential Synth*/
		     - M.myy*0.5*gg[i].u1[j+Zg]*sin(2*Az)
		     - M.mxy*gg[i].u1[j+Zg]*cos(2*Az)
		     - M.mxz*gg[i].u2[j+Zg]*sin(Az)
		     + M.myz*gg[i].u2[j+Zg]*cos(Az)),
		    (M.mxx*0.166667*gg[i].u5[j+Zg]    /*Radial Synth*/
		     - M.mxx*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		     + M.mxx*0.3333*gg[i].u9[j+Zg]
		     + M.myy*0.166667*gg[i].u5[j+Zg] 
		     + M.myy*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		     + M.myy*0.3333*gg[i].u9[j+Zg]
		     + M.mzz*0.3333*gg[i].u9[j+Zg]
		     - M.mzz*0.3333*gg[i].u5[j+Zg]
		     - M.mxy*gg[i].u3[j+Zg]*sin(2*Az)
		     + M.mxz*gg[i].u4[j+Zg]*cos(Az)
		     + M.myz*gg[i].u4[j+Zg]*sin(Az)),
		    (M.mxx*0.166667*gg[i].u8[j+Zg]             /*Vertical Synth*/
		     - M.mxx*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		     + M.mxx*0.3333*gg[i].u10[j+Zg]
		     + M.myy*0.166667*gg[i].u8[j+Zg] 
		     + M.myy*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		     + M.myy*0.3333*gg[i].u10[j+Zg]
		     + M.mzz*0.3333*gg[i].u10[j+Zg]
		     - M.mzz*0.3333*gg[i].u8[j+Zg]
		     - M.mxy*gg[i].u6[j+Zg]*sin(2*Az)
		     + M.mxz*gg[i].u7[j+Zg]*cos(Az)
		     + M.myz*gg[i].u7[j+Zg]*sin(Az)));
	}
    }
    fclose(fp);
    return 0;
}
