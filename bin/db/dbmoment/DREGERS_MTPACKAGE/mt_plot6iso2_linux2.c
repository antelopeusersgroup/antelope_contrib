#include"tdmt_invb.h"
#include<string.h>
#include<stdlib.h>

float A[6];
void yplot();

mt_plot(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,d_mt,Pdc,Pclvd,Piso,Mo,Mw,E,VR)
int nsta;
struct MOMENT M;
struct GREEN *gg;
struct DATA  *ss;
float Strike, Rake, Dip, Pdc, Pclvd, Piso, Mo, Mw, E, VR;
float St2, Rk2, Dp2;
double d_mt[3][3];
   {
   int i,j, n, Z, Zg, Np, Zd, count;
   float xorig=1.0, yorig=7.0,xscale,yscale;
   float dt, spin, x, y, *tmp, height, Mscl,Az;
   float scalefac,scaler,scalet,scalez;
   char name[6];

   Mscl = Mo/1.0e+20;

settextfont(1);
   setfat(0);
   if (nsta < 7)
     yscale=1.0;
   else
     yscale=6.0/nsta;

   text(1.0,7.7,"Tangential");
   text(4.0,7.7,"Radial");
   text(6.0,7.7,"Vertical");

   y=0.0;
   height=0.8;
   for(i=0; i < nsta; i++)
      {
      setdash(0);
      Np=ss[i].nn;
      Z =ss[i].zz;
      dt=ss[i].dt;
      y=7.0-i*yscale;
      spin = (float)Np*ss[i].dt/2.0;
      tmp=(float *)malloc(sizeof(float)*Np);

      scaler=scalet=scalez=scalefac=0.0;
	    for(j=0;j < Np;j++)
	      {
	      if(ABS(ss[i].t[j+Z]) > scalet) scalet=ABS(ss[i].t[j+Z]);
	      if(ABS(ss[i].r[j+Z]) > scaler) scaler=ABS(ss[i].r[j+Z]);
	      if(ABS(ss[i].z[j+Z]) > scalez) scalez=ABS(ss[i].z[j+Z]);
	      }
	     if(scalefac < scalet) scalefac=scalet;
	     if(scalefac < scaler) scalefac=scaler;
	     if(scalefac < scalez) scalefac=scalez;

      settextsize(0.15);

      //text(0.2,y-0.50,"%4s,%.0f  Max Amp=%.2e cm",ss[i].name,ss[i].azi*180.0/PI,scalefac);
      text(0.2,y-0.65,"VR=%.1f",ss[i].vr);


      for(j=0; j < Np; j++)
	 tmp[j]=ss[i].t[j+Z];
      yplot(0.5,y,height*scalet/scalefac,spin,tmp,Np,dt,0); /*Tangential Data*/
      for(j=0; j < Np; j++)
	 tmp[j]=ss[i].r[j+Z];
      yplot(3.0,y,height*scaler/scalefac,spin,tmp,Np,dt,0); /*Radial*/
      for(j=0; j < Np; j++)
	 tmp[j]=ss[i].z[j+Z];
      yplot(5.5,y,height*scalez/scalefac,spin,tmp,Np,dt,0); /*Vertical*/

      setdash(1);
      Az=ss[i].azi;
      partl(Strike,Rake,Dip,Az);
      Np=gg[i].nn;
      Zg =gg[i].zz;
      dt=gg[i].dt;
      spin = (float)Np*gg[i].dt/2.0;
      scalet=scaler=scalez=0.0;
      for(j=0; j< Np; j++)
	 {
	 tmp[j]=(M.mxx*0.5*gg[i].u1[j+Zg]*sin(2*Az)
		- M.myy*0.5*gg[i].u1[j+Zg]*sin(2*Az)
	        - M.mxy*gg[i].u1[j+Zg]*cos(2*Az)
	        - M.mxz*gg[i].u2[j+Zg]*sin(Az)
	        + M.myz*gg[i].u2[j+Zg]*cos(Az));

         if(ABS(tmp[j]) > scalet) scalet=ABS(tmp[j]);
	 }

      yplot(0.5,y,height*scalet/scalefac,spin,tmp,Np,dt,0); /*Tangential Synth*/
      for(j=0; j< Np; j++)
	 {
	 tmp[j]= (M.mxx*0.5*gg[i].u5[j+Zg] 
		 - M.mxx*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		 + M.mxx*0.3333*gg[i].u9[j+Zg]
	         + M.myy*0.5*gg[i].u5[j+Zg] 
		 + M.myy*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		 + M.myy*0.3333*gg[i].u9[j+Zg]
 	         + M.mzz*0.3333*gg[i].u9[j+Zg]
		 - M.mxy*gg[i].u3[j+Zg]*sin(2*Az)
		 + M.mxz*gg[i].u4[j+Zg]*cos(Az)
		 + M.myz*gg[i].u4[j+Zg]*sin(Az));

         if(ABS(tmp[j]) > scaler) scaler=ABS(tmp[j]);
	 }
      yplot(3.0,y,height*scaler/scalefac,spin,tmp,Np,dt,0); /*Radial Synth*/
      for(j=0; j< Np; j++)
	 {
	 tmp[j]= (M.mxx*0.5*gg[i].u8[j+Zg] 
		 - M.mxx*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		 + M.mxx*0.3333*gg[i].u10[j+Zg]
                 + M.myy*0.5*gg[i].u8[j+Zg] 
		 + M.myy*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		 + M.myy*0.3333*gg[i].u10[j+Zg]
                 + M.mzz*0.3333*gg[i].u10[j+Zg]
		 - M.mxy*gg[i].u6[j+Zg]*sin(2*Az)
		 + M.mxz*gg[i].u7[j+Zg]*cos(Az)
		 + M.myz*gg[i].u7[j+Zg]*sin(Az));

         if(ABS(tmp[j]) > scalez) scalez=ABS(tmp[j]);
	 }

      yplot(5.5,y,height*scalez/scalefac,spin,tmp,Np,dt,1); /*Vertical Synth*/
	


      free(tmp);
      }


      settextsize(0.2);
      text(8.0,7.0,"Strike=%.0f ; %0.f",Strike,St2);
      text(8.0,6.7,"Rake  =%.0f ; %0.f",Rake,Rk2);
      text(8.0,6.4,"Dip   =%.0f ; %0.f",Dip,Dp2);
      text(8.0,6.1,"Mo    =%.2e",Mo);
      text(8.0,5.8,"Mw    =%.1f",Mw);
      text(8.0,5.5,"Percent DC=%.0f",Pdc);
      text(8.0,5.2,"Percent CLVD=%.0f",Pclvd);
      text(8.0,4.9,"Percent ISO=%.0f",Piso);
      text(8.0,4.6,"Variance=%.2e",E);
      text(8.0,4.3,"Var. Red=%.2e",VR);
      text(8.0,4.0,"RES/Pdc.=%.2e",E/Pdc);

      focplt(Strike,Rake,Dip,d_mt,9.0,2.7,4,3.0);



   }

/***** SUBROUTINES *****/


/* Subroutine to Calculate the A (Horizontal Radiation */
/* Pattern Coefficients*/


   partl(theta,lambda, delta, azi)
   float theta, lambda, delta, azi;
     {
     int i;
     float con=0.017453, tc, lc, dc;

     tc =azi - theta*con;
     lc =lambda*con;
     dc =delta *con;

      /* The Following are the Ai Coefficients */
      /* The Variables are defined Globally    */
      A[0]=sin(2.0*tc)*cos(lc)*sin(dc) + 0.5*cos(2.0*tc)*sin(lc)*sin(2.0*dc);

      A[1]=cos(tc)*cos(lc)*cos(dc) - sin(tc)*sin(lc)*cos(2.0*dc);

      A[2]=0.5*sin(lc)*sin(2.0*dc);

      A[3]=cos(2.0*tc)*cos(lc)*sin(dc) - 0.5*sin(2.0*tc)*sin(lc)*sin(2.0*dc);

      A[4]=sin(tc)*cos(lc)*cos(dc) + cos(tc)*sin(lc)*cos(2.0*dc);

      A[4] *= -1.0;
   }



void yplot(xorig,yorig,height,spin,yy,num,dt,kspin)
int kspin;
float xorig,yorig;
float spin,dt,height;
float *yy;
int num;
{
int i;
float nxm,xm;
float scv,sch;
float x,y,z,y1;
float xp,yp;
z=0;
xm=0;
for(i=0;i<num;i++){
	x=ABS(yy[i]);
	if(yy[i]>xm){
		xm=yy[i];
		nxm=i;
		}
	if(x>z)z=x;
	}
setorig(xorig,yorig);
yy[0]=0.0;/*Reset first point to zero*/
setmin(0.0,yy[0]);
/*setdash(0);*/
scv=0.5*height/z;
sch=dt/spin;
uplot(0.0,yy[0],0);
for(i=0;i<num;i++){
	x=sch*i;
	y=(yy[i]-yy[0])*scv;
	uplot(x,y,1);
	}
/* x=sch*nxm+0.3;*/
x=sch*(0.8*num);
y1=0.35*height;
uplot(x,y1,0);
settextsize(0.12);
settextfont(1);
/*Don't plot the amps
utext(x,y1,"%0.1e cm",z);*/
if(kspin==1){
        setdash(0);
	y=0.0;
	uplot(x,y,0);
	where(&xp,&yp);
	y1=0.2*height;
	plot(xp,yp-y1,0);
	plot(xp,yp-y1-0.1,1);
	plot(xp,yp-y1-0.05,0);
	plot(xp+0.5,yp-y1-0.05,1);
	where(&xp,&yp);
	plot(xp,yp-0.05,0);
	plot(xp,yp+0.05,1);
	spin=spin/2.0;
	text(xp-0.525,yp-0.2,"%4.2f sec",spin);
	}
}

#include <stdio.h>
#include <sys/file.h>
#include <math.h>
#include <strings.h>
#define sign(X,Y) (( (Y) < 0.0 ) ? -fabs(X) : fabs(X) )
#define CHNUM   40
#define DTR 0.0174533
#define RTD 57.29578
#define SQRT2 1.414214
#define EPS 0.00001
#define SMALLER 0.05
#define PATC 3
#define TEXFAC 0.05
#define DL rad*0.025
#define MAXSTA  32400

struct azphi { float az, phi; } mtpol[MAXSTA],ev[MAXSTA],p,t,b;


struct coord { float x, y; } cent;
float delt1, delt2;
float dip2, al2, strk2;
float dip1, al1, strk1;
int fast;
char ip[4]="P";
char it[4]="T";



char fin[CHNUM],fot[CHNUM];
char *chold;
float delt1, delt2;
float azp, pap, azt, pat, azb, pab;
FILE *inf, *itf, *fopen();


focplt(th,lam,del,d_mt,x,y,opt,r90)
int opt; /*Equal to 4 or 5 */
float th,lam,del,x,y,r90;
double d_mt[3][3];
{
	int iqual[MAXSTA];
        char rcode[MAXSTA];
	float dip1, al1, strk1;


	cent.x = x;
	cent.y = y;
	iqual[0]=0;
	ev[0].az=0.0;
	ev[0].phi=0.0;
	strk1=th;
        al1=lam;
        dip1=del;
	mplot(32400," ",rcode,iqual,ev,r90,dip1,al1,strk1," ",opt,0,cent,d_mt);
}


mplot(nsta,rsta,rcode,iqual,ev,radc,xdip1,xal1,xstrk1,enam,opt,pro,delt,d_mt)

/*
     mplot: by RJ Stead
     subroutine for mechanism plot,  based on JH Whitcomb's focplt
	     and H Kanamori's mplot
     nsta = number of stations,
     rsta = station name,
     rcode = c or d or n(nodal),
     iqual = quality (1 or 2); 2 is higher q and plots a larger symbol,
     ev = event azimuth and take-off angle,
     radc = radius of the plot in cm (if <0, do fast plot),
     dip1 = dip angle,  al1 = slip angle,  strk1 = strike of plane1,
     enam = event id,
     opt determines plot option:
	opt = 0 1st-motion data only
	opt = 1 ist-motion data plus station name
	opt = 2 1st-motion data plus nodal lines
	opt = 3 1st-motion data,  station name plus nodal lines
	opt = 4 nodal lines only( p and t axes labelled )
	opt = 5 shade quadrants only
     pro = 0 plots on equal area projection
     pro = 1 plots on wulff net (equal angle).
     delt = absolute position on plot of center of focal sphere in inches
*/

double d_mt[3][3];
char **rsta;
int nsta, *iqual, opt, pro;
char *rcode;
char *enam;
struct azphi *ev;
struct coord delt;
float radc, xdip1, xal1, xstrk1;
{
	static char **iptl;
	int i, iline, nplp1, nplp2;
	struct azphi pt[2], pl1[200], pl2[200], *pp;
	float px[3], tx[3], bx[3];
	float xz, yz;
	float rad, dip1r, dip2r, azr;
	int incr, ok;
	int pat2[32], patC[32];
	static int alreadydone=0;

	setfat(0);

	if (alreadydone == 0) {
		iptl = (char **) malloc(4 * 2);
		iptl[0] = ip;
		iptl[1] = it;
		alreadydone = 1;
	}

	dip1 = xdip1; al1 = xal1; strk1 = xstrk1;
	for (i=0; i<32; i++) pat2[i] = 0;
	defpattern(2,pat2);
	delt1 = delt.x;
	delt2 = delt.y;
	fast = 0.;
	if (radc < 0) {
		fast = 1;
		radc *= -1;
	}
	rad = radc / 2.54;
	incr = 1;
	if (fast) incr = 10;

	/* plot and label circle */
	i = 360/incr;
	circl3a (rad, delt, i);
	plot(delt1 - rad - DL, delt2, 0);
	plot(delt1 - rad + DL, delt2, 1);
	plot(delt1 - DL, delt2, 0);
	plot(delt1 + DL, delt2, 1);
	plot(delt1 + rad - DL, delt2, 0);
	plot(delt1 + rad + DL, delt2, 1);
	plot(delt1, delt2 - rad - DL, 0);
	plot(delt1, delt2 - rad + DL, 1);
	plot(delt1, delt2 - DL, 0);
	plot(delt1, delt2 + DL, 1);
	plot(delt1, delt2 + rad - DL, 0);
	plot(delt1, delt2 + rad + DL, 1);

	/* plot MT first motion polarity*/
	     iline = 0;
	     polar(d_mt,rcode,mtpol);
             plambm(32400, rsta, rcode, iqual, mtpol, rad, opt, iline, pro);

	/* plot nodal lines */
	if (opt > 1) {

		/* solve for planes, stress axes */
		ok = fptr(&strk1,&dip1,&al1,&strk2,&dip2,&al2,px,tx,bx,0);
		if (ok < 0) return(-1);

		/* set dips to range 0<dip<=90 */
		if (dip1 > 90.0) {
			dip1  = 180.0 - dip1;
			strk1 = 180.0 + strk1;
			al1   = 360.0 - al1;
		}
		if (dip2 > 90.0) {
			dip2  = 180.0 - dip2;
			strk2 = 180.0 + strk2;
			al2   = 360.0 - al2;
		}
		dip1r = dip1 * DTR;
		dip2r = dip2 * DTR;

		/* find plane #1 */
		nplp1 = 181;
		if (fast) nplp1 = 19;
		pl1->az = strk1;
		pl1->phi = 90.0;

		/* for vertical plane, 2 points suffice */
		if (fabs(dip1) > 89.9) {
			pl1[1].az = pl1->az + 180.0;
			pl1[1].phi = 90.0;
			nplp1 = 2;
		}
		else {
			pp = pl1;
			for (i = 1; i<(nplp1-1); i++) {
				pp++;
				pp->az = strk1 + incr * (float) i;
				pp->phi = 90.0;
				if (fabs(dip1) >= 0.01) {
					azr = (pp->az - strk1) * DTR;
					pp->phi = RTD * atan(1.0 / (tan(dip1r)
						* sin(azr)));
				}
			}
			pp++;
			pp->az = strk1 + 180.;
			pp->phi = 90.;
		}

		/* find plane #2 */
		nplp2 = 181;
		if (fast) nplp2 = 19;
		pl2->az = strk2;
		pl2->phi = 90.0;
		if (fabs(dip2) > 89.9) {
			pl2[1].az = pl2->az + 180.0;
			pl2[1].phi = 90.0;
			nplp2 = 2;
		}
		else {
			pp = pl2;
			for (i = 1; i <(nplp2-1); i++) {
				pp++;
				pp->az = strk2 + incr * (float) i;
				pp->phi = 90.0;
				if(fabs(dip2) >= 0.01) {
					azr = (pp->az - strk2) * DTR;
					pp->phi = RTD * atan(1.0 / (tan(dip2r)
						* sin(azr)));
				}
			}
			pp++;
			pp->az = strk2 + 180.;
			pp->phi = 90.;
		}

			pt->az = p.az;
			pt[1].az = t.az;
			pt->phi = 180.0 - p.phi;
			pt[1].phi = 180.0 - t.phi;
			iline = 2;
			plambm(2,iptl,rcode,iqual,pt,rad,opt,iline,pro);
		}


		/* shade quadrants */
		else {
			setbrushpat(nsta);
			mshade(nplp1,nplp2,pl1,pl2,al1,al2,rad,pro);
		}

		/* plot nodal lines */
		iline = 1;
		plambm(nplp1,rsta,rcode,iqual,pl1,rad,opt,iline,pro);
		plambm(nplp2,rsta,rcode,iqual,pl2,rad,opt,iline,pro);
//	}
//
	/* print mechanism label */
	settextsize(rad*TEXFAC);
	xz = delt1 - rad;
	yz = delt2 + 1.3 * rad;
//	text(xz, yz, "%s", enam);
	return(0);
}
      polar(d_mt,rcode,mtpol)
/*...... compute first motion polarity as a function of aoi & theta */
/*       for an arbitrary moment tensor */
      char *rcode;
      struct azphi *mtpol;
      double d_mt[3][3];
      {
      int i, j;
      float aoi, theta, polar;
      float  mxx,mxy,mxz,myy,myz,mzz, fa, fb, smax;

        mxx=(float)(d_mt[0][0]);
        mxy=(float)(d_mt[0][1]);
        mxz=(float)(d_mt[0][2]);
        myy=(float)(d_mt[1][1]);
        myz=(float)(d_mt[1][2]);
        mzz=(float)(d_mt[2][2]);
        smax=0.0;
        if(ABS(mxx)  > smax) smax=ABS(mxx);
        if(ABS(mxy)  > smax) smax=ABS(mxy);
        if(ABS(mxz)  > smax) smax=ABS(mxz);
        if(ABS(myy)  > smax) smax=ABS(myy);
        if(ABS(myz)  > smax) smax=ABS(myz);
        if(ABS(mzz)  > smax) smax=ABS(mzz);
        mxx=mxx/smax;
        mxy=mxy/smax;
        mxz=mxz/smax;
        myy=myy/smax;
        myz=myz/smax;
        mzz=mzz/smax;

	for(i=0; i < 360; i++)
	  for(j=0; j < 90; j++)
	  {
	  theta = (float) i;
	  aoi   = (float) j;
	  mtpol[i*90+j].az =theta;
	  mtpol[i*90+j].phi=aoi;
	  theta *= PI/180.0;
	  aoi   *= PI/180.0;

      fa = cos(theta)*cos(theta)*mxx + 2.0*sin(theta)*cos(theta)*mxy +
           sin(theta)*sin(theta)*myy - mzz;

      fb = cos(theta)*mxz + sin(theta)*myz;

      polar        = sin(aoi)*sin(aoi)*fa + 2.*sin(aoi)*cos(aoi)*fb + mzz;

      if(polar > 0.0)
	 rcode[i*90+j]='c';
      else
	 rcode[i*90+j]='n';

	   }


     }

plambm(npts, rsta, rcode, iqual, ar, rad, opt, iline, pro)

/*
       plamb plots in stereographic projection the points in ar for
	     a circle of radius rad inches;
       iline = 0 plot first-motion data;
       iline = 1 plot nodal lines;
       iline = 2 plot p and t axes;
       pro = 0   equal area projection;
       pro = 1   wulff net;
       opt determines the plot option,  see mplot;
       ar:    angular spherical location of each point (in degrees);
       rad:   desired radius of plotted circle (inches);
       npts:  the number of points to be plotted;
       delt1: x offset of center of plotted circle from lower left
            corner of plotter page (inches);
       delt2: y offset of center.
*/

char **rsta;
char *rcode;
int *iqual, npts, opt, iline, pro;
struct azphi *ar;
float rad;
{
	struct coord axy[34200], *paxy;
	struct coord ptaxes[4];
	float xc, yc, rsize;
	int i, nspar;
	char c='c', d='d', n='x';
	float smrat=SMALLER;
	int ipatc=PATC;
	int ipatd=2;

	nspar = 0;
	if (opt == 1 || opt == 3) nspar = 1;
	net_coord(ar,axy,pro,npts,rad);

/*	plot MT first motions  */

	switch (iline) {

	case 0:
		paxy = axy;
		for (i = 0; i < npts; i += 3) {
			xc = paxy[i].x;
			yc = paxy[i].y;
			rsize = rad * smrat * 0.3;
			if (rcode[i] == c)
			    point(xc,yc);

		}
		break;

	case 1:
		setfat(2);
		plot(axy->x, axy->y, 0);
		paxy = axy;
		for (i = 1; i < npts; i++) {
			paxy++;
			plot(paxy->x, paxy->y, 1);
		}
		break;

	case 2:
		rsize = rad * smrat*1.2;
		paxy = axy;
		settextsize(rsize * 0.8);
		setbrushpat(0);
		for (i = 0; i < npts; i++) {
			/*
			symbol(paxy->x,paxy->y,8,rsize,0.0);
			*/
			setbrushpat(0);
			ptaxes[0].x = paxy->x+rsize/3.;
			ptaxes[0].y = paxy->y;
			ptaxes[1].x = paxy->x;
			ptaxes[1].y = paxy->y+rsize/2.;
			ptaxes[2].x = paxy->x-rsize/3.;
			ptaxes[2].y = paxy->y;
			ptaxes[3].x = paxy->x;
			ptaxes[3].y = paxy->y-rsize/2.;
			polyfill(ptaxes,4);
			//if(opt >= 3)
			//	text(paxy->x + 0.7*rsize,paxy->y,"%s",rsta[i]);
			paxy++;
		}
		break;
	}
}

circl3a (rad, cent, kpts)
float rad;
struct coord cent;
int kpts;
{
	float x, y, theta;
	float hold;
	int l;

	x= cent.x + rad;
	plot (x, cent.y, 0);
	hold = 2. * PI / kpts;
	for (l = 1; l <= kpts; l++) {
		theta = l * hold;
		x = rad * cos(theta) + cent.x;
		y = rad * sin(theta) + cent.y;
		plot (x, y, 1);
	}
}

circl3 (rad, cent, kpts, ipat)
float rad;
struct coord cent;
int kpts, ipat;
{
	struct coord circ[361];
	float theta;
	float hold;
	int l;

	if (kpts <= 3 || kpts > 360) kpts = 360;
	hold = 2. * PI / kpts;
	for (l = 0; l < kpts; l++) {
		theta = l * hold;
		circ[l].x = cent.x + rad * cos(theta);
		circ[l].y = cent.y + rad * sin(theta);
	}
	setbrushpat(ipat);
  	polyfill(circ,kpts);  
}

fptr(f1d, d1d, al1d, f2d, d2d, al2d, px, tx, bx, id)

/*
     f1d, f2d: strike in degrees;
     did, d2d: dip in degrees;
     al1, al2: slip angle in degrees;
     px: pressure axis in geographical coordinates;
     tx: tension axis in geograph. coordinates;
     bx: null axis in geograph. coordinates;
     geograph. coord. are:
     north = x1,  west = x2,  vertical = x3;
     d1d should be 90=>d1d>0;
     p, t, b: azimuth and polar angle (degrees) of pressure, tension and null;
     if id = 0 f1d, d1d, al1d are given,  calculate other parameters;
     if id != 0 f1d, d1d, f2d, d2d are given,  calculate other parameters;
     al1d is uncertain by 180.0;
     by RJ Stead, based on H Kanamori's fptr subroutine.
*/

float *f1d, *d1d, *al1d, *f2d, *d2d, *al2d;
float *px, *tx, *bx;
int id;
{
	float a[3], an[3];
	float f1, d1, al1, f2, d2, col, sil, cod, sid, cof, sif;
	float cc1, cc2, cc3, pck, tck, bck, atest1, atest2, c3, c4, c5;
	float c0 = 0.70711;
	float c1 = 57.29578;
	float c2 = 0.0174533;
	int i, ia3, ian3;

	f1 = *f1d * c2;
	d1 = *d1d * c2;
	al1 = *al1d * c2;
	f2 = *f2d * c2;
	d2 = *d2d * c2;
	al2 = *al2d * c2;
	col = cos(al1);
	sil = sin(al1);
	cod = cos(d1);
	sid = sin(d1);
	cof = cos(f1);
	sif = sin(f1);
	if (id != 0) {
		cc3 = cod * cos(d2) + sid * sin(d2) * cos(f2 - f1);
		if (fabs(cc3) >= 0.05) {
			fprintf(stderr,"two planes are not orthogonal\n");
			return(-1);
		}
		cc1 = cos(d2) / sid;
		cc2 = sin(d2) * sin(f1 - f2);
		al1 = atan2(cc1, cc2);
		col = cos(al1);
		sil = sin(al1);
	}
	a[0] = col * cof + sil * cod * sif;
	a[1] = -col * sif + sil * cod * cof;
	a[2] = sil * sid;
	an[0] = -sid * sif;
	an[1] = -sid * cof;
	an[2] = cod;
	bx[0] = an[1] * a[2] - an[2] * a[1];
	bx[1] = an[2] * a[0] - an[0] * a[2];
	bx[2] = an[0] * a[1] - an[1] * a[0];
	tx[0] = (a[0] + an[0]) * c0;
	tx[1] = (a[1] + an[1]) * c0;
	tx[2] = (a[2] + an[2]) * c0;
	px[0] = (an[0] - a[0]) * c0;
	px[1] = (an[1] - a[1]) * c0;
	px[2] = (an[2] - a[2]) * c0;
	for (i = 0; i < 3; i++) {
		if ( fabs(px[i]) >= 1.0 ) px[i] = sign(1.0, px[i]);
		if ( fabs(tx[i]) >= 1.0 ) tx[i] = sign(1.0, tx[i]);
		if ( fabs(bx[i]) >= 1.0 ) bx[i] = sign(1.0, bx[i]);
	}
	p.az = 0.0;
	t.az = 0.0;
	b.az = 0.0;
	pck = px[0] * px[0] + px[1] * px[1];
	if (pck > 0.000001) p.az = (-atan2(px[1], px[0])) * c1;
	tck = tx[0] * tx[0] + tx[1] * tx[1];
	if (tck > 0.000001) t.az = (-atan2(tx[1], tx[0])) * c1;
	bck = bx[0] * bx[0] + bx[1] * bx[1];
	if (bck > 0.000001) b.az = (-atan2(bx[1], bx[0])) * c1;
	p.phi = acos(px[2]) * c1;
	t.phi = acos(tx[2]) * c1;
	b.phi = acos(bx[2]) * c1;
	atest1 = fabs(an[0]) + fabs(an[1]);
	atest2 = fabs(a[0]) + fabs(a[1]);
	if (atest1 <= 0.0 || atest2 <= 0.0) {
		/* horizontal fault or vertical slip */
		*f1d = c1 * f1;
		*d1d = c1 * d1;
		*al1d = c1 * al1;
		if (*d1d > 90) {
			*d1d = 180. - *d1d;
			*f1d += 180.;
			*al1d = 360. - *al1d;
		}
		*f2d = -(*f1d);
		*d2d = 90. - *d1d;
		*al2d = *al1d;
		return(0);
	}
	d2 = acos(a[2]);
	c3 = -sid * col;
	al2 = atan2(cod, c3);
	ia3 = a[2];
	ian3 = an[2];
	if (ian3 == 0 || ia3 == 0) 
		f2 = atan2(-a[0], -a[1]);
	else {
		c4 = -1.0 / (tan(d1) * tan(d2));
		c5 = col / sin(d2);
		f2 = f1 - atan2(c5, c4);
	}
	*f1d = c1 * f1;
	*d1d = c1 * d1;
	*al1d = c1 * al1;
	*f2d = c1 * f2;
	*d2d = c1 * d2;
	*al2d = c1 * al2;
	if (*d1d > 90.0) {
		*d1d = 180.0 - *d1d;
		*f1d = *f1d + 180.0;
		*al1d = 360.0 - *al1d;
	}
	if (*d2d <= 90.0) return(1);
	*d2d = 180.0 - *d2d;
	*f2d = *f2d + 180.0;
	*al2d = 360.0 - *al2d;
	return(1);
}

net_coord(in,out,pro,n,rad)
struct azphi *in;
struct coord *out;
int pro, n;
float rad;
{
	float r, az, phi;

	while (n--) {
		out->x = delt1;
		out->y = delt2;
		if (fabs(in->phi) > EPS) {
			az = DTR * in->az;
			phi = DTR * in->phi / 2.;
			if (in->phi > 90.) az += PI;
			if (in->phi > 90.) phi = PI / 2. - phi;
			if (pro == 0) r = SQRT2 * rad * sin(phi);
			else r = rad * tan(phi);
			out->x += r * sin(az);
			out->y += r * cos(az);
		}
		out++;
		in++;
	}
}

mshade(n1,n2,plane1,plane2,rake1,rake2,rad,pro)

/* shades quadrants of focal sphere */
/*
	Uses convention strike to left as you look down dip,
	0<dip<=90 and strike is measured clockwise from north.
	Rake is motion of hanging wall relative to footwall
	and is measured counter-clockwise from horizontal
	(measured on plane of footwall starting at strike).
	Tensional quadrants are shaded.
	Planes must have azimuth increasing with index, starting
	at strike, thus shading direction is determined
	entirely from rake and strike.
	Pure dip-slip are special cases.
	RJ Stead 4/7/87
*/

int n1,n2,pro;
struct azphi *plane1, *plane2;
float rake1, rake2;
float rad;
{
	struct azphi area[400];
	struct coord areaxy[400];
	float first, last;
	int cnt0, cnt1, cnt2, total, i, di, xincr;
	float lasttest, which1, which2;
	int notdone;
	float incr, pincr, baz;
	struct azphi *pa, *pp1, *pp2;

	incr = 1.;
	if (fast) incr = 10.;
	di = (int) incr;
	first = area->az = plane1->az;
	area->phi = plane1->phi;
	total = 1;
	pa = area;
	pa++;
	last = plane2->az;
	lasttest = sin(DTR*(last - first));
	which1 = cos(DTR*rake1);
	which2 = cos(DTR*rake2);
	if (fabs(which1)<EPS || fabs(which2)<EPS || fabs(lasttest)<EPS) {
		which1 = sin(DTR*rake1);

		/* pure dip-slip */
		if (n1 == 2 || n2 == 2) {
			/* horizontal plane */
			if (n2 == 2) {
				first = area->az = plane2->az;
				area->phi = plane2->phi;
				which1 = sin(DTR*rake2);
			}
			if (which1 > 0.) {
				if (first < 180.) first += 180.;
				else first -= 180.;
				area->az = first;
			}
			for (i=0; i<180; i += di) {
				first += incr;
				pa->az = first;
				pa->phi = 90.;
				pa++;
			}
			total = 180 / di;
			net_coord(area,areaxy,pro,total,rad);
			polyfill(areaxy,total);
			return;
		}

		if (which1 > 0.) {
			/* thrust */
			pp1 = plane1;
			pa = area;
			for (i=0; i<(n1-1); i++) {
				pa->az = pp1->az;
				pa->phi = pp1->phi;
				pa++;
				pp1++;
			}
			pp2 = plane2;
			for (i=0; i<(n2-1); i++) {
				pa->az = pp2->az;
				pa->phi = pp2->phi;
				pa++;
				pp2++;
			}
			total = n1 + n2 - 2;
			net_coord(area,areaxy,pro,total,rad);
			polyfill(areaxy,total);
			return;
		}

		/* normal fault */
		pa = area;
		pp1 = plane1;
		for (i=0; i<(n1-1); i++) {
			pa->az = pp1->az;
			pa->phi = pp1->phi;
			pa++;
			pp1++;
		}
		first = plane1->az+180.0;
		for (i=0; i<(n1-1); i++) {
			pa->az = first;
			pa->phi = 90.;
			first -= incr;
			pa++;
		}
		total = 2*n1 - 2;
		net_coord(area,areaxy,pro,total,rad);
		polyfill(areaxy,total);
		pa = area;
		pp2 = plane2;
		for (i=0; i<(n2-1); i++) {
			pa->az = pp2->az;
			pa->phi = pp2->phi;
			pa++;
			pp2++;
		}
		first = plane2->az+180.0;
		for (i=0; i<(n2-1); i++) {
			pa->az = first;
			pa->phi = 90.;
			first -= incr;
			pa++;
		}
		total = 2*n2 - 2;
		net_coord(area,areaxy,pro,total,rad);
		polyfill(areaxy,total);
		return;
	}

	/* oblique- or strike-slip */
	/* area #1 */
	lasttest *= which1;
	if (lasttest < 0.) last = plane2[n2-1].az;
	cnt0 = (int) (last - first);
	if (which1 < 0.) cnt0 = (int) (first - last);
	notdone = 1;
	while (notdone) {
		if (cnt0 < 0) cnt0 += 180;
		else if (cnt0 >= 180) cnt0 -= 180;
		else notdone = 0;
	}
	cnt0 /= di;
	pincr = incr;
	if (which1 < 0.) pincr *= -1.;
	for (i=0; i<cnt0; i++) {
		/* leg #1 */
		first += pincr;
		pa->az = first;
		pa->phi = 90.;
		pa++;
	}
	total += cnt0;
	pa->az = last;
	pa->phi = 90.;
	pa++;
	total++;
	pp2 = plane2;
	xincr = 1;
	if (lasttest < 0.) {
		pp2 += n2 - 1;
		xincr = -1;
	}
	if (n2 != 2) {
		/* leg #2 */
		pp2 += xincr;
		cnt1 = 0;
		notdone = 1;
		baz = b.az;
		while (notdone) {
			if (baz < plane2->az) baz += 180.;
			else if (baz >= (plane2->az+180)) baz -= 180.;
			else notdone = 0;
		}
		while (fabs(pp2->az - baz) > incr) {
			pa->az = pp2->az;
			pa->phi = pp2->phi;
			pa++;
			cnt1++;
			pp2 += xincr;
		}
		total += cnt1;
	}
	pa->az = b.az;
	pa->phi = 180. - b.phi;
	pa++;
	total++;
	pp1 = plane1;
	if (n1 != 2) {
		/* leg #3 */
		pp1++;
		cnt2 = 0;
		notdone = 1;
		baz = b.az;
		while (notdone) {
			if (baz < plane1->az) baz += 180.;
			else if (baz >= (plane1->az+180)) baz -= 180.;
			else notdone = 0;
		}
		while (fabs(pp1->az - baz) > incr) {
			pp1++;
			cnt2++;
		}
		pp1--;
		for (i=0; i<cnt2; i++) {
			pa->az = pp1->az;
			pa->phi = pp1->phi;
			pa++;
			pp1--;
		}
		total += cnt2;
	}
	net_coord(area,areaxy,pro,total,rad);
	polyfill(areaxy,total);

	/* area #2 */
	first = area->az = plane1[n1-1].az;
	area->phi = plane1[n1-1].phi;
	total = 1;
	pa = area;
	pa++;
	last = plane2[n2-1].az;
	if (lasttest < 0.) last = plane2->az;
	total += cnt0;
	while (cnt0--) {
		/* leg #4 */
		first += pincr;
		pa->az = first;
		pa->phi = 90.;
		pa++;
	}
	pa->az = last;
	pa->phi = 90.;
	pa++;
	total++;
	pp2 = plane2;
	xincr = -xincr;
	if (lasttest >= 0.) pp2 += n2 - 1;
	if (n2 != 2) {
		/* leg #5 */
		pp2 += xincr;
		cnt1 = n2 - cnt1 - 2;
		total += cnt1;
		while (cnt1--) {
			pa->az = pp2->az;
			pa->phi = pp2->phi;
			pa++;
			pp2 += xincr;
		}
	}
	pa->az = b.az;
	pa->phi = 180. - b.phi;
	pa++;
	total++;
	pp1 = plane1;
	pp1 += n1 - 1;
	if (n1 != 2) {
		/* leg #6 */
		cnt2 = n1 - cnt2 - 2;
		pp1 -= cnt2;
		total += cnt2;
		while (cnt2--) {
			pa->az = pp1->az;
			pa->phi = pp1->phi;
			pa++;
			pp1++;
		}
	}
	net_coord(area,areaxy,pro,total,rad);
	polyfill(areaxy,total);
	return;
}
