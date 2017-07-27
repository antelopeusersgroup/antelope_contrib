/*Copyright (c) Douglas Dreger
Berkeley Seismological Laboratory
University of California, Berkeley */
/* Moment Tensor Inversion */
/* Uses previously determined Green's functions in D. Helmbergers format*/

#include <stdlib.h>
#include"tdmtinv_iso.h"

void gaussj();
int correlate(),fclose();
FILE *fopen(), *par,*fd, *fd1, *fd2, *fd3, *out;


int main()
{
    int i,j,l,N,Np,Z,Zs,Zg,k,ntr,nsta,nn,*npts = NULL ,wflag=0;
    int depth,cnt1, cnt2, cnt3, np2,QUAL,plotflag,WCNT;
    float *W, *tmpp,*tmp,*dt, cormax, mindist, dist,E,VR,junk;
    float tracelang;
    float eigen1, eigen2, eigen3;
    float eigaz1, eigaz2, eigaz3;
    float eigpl1, eigpl2, eigpl3;
    double **AIV,**AJ, **B;
    struct MOMENT M;
    struct DATA *ss;
    struct GREEN *gg;
    char infile[200],binfile[205],datfile[200];
    /**Variables needed for implementation of R Urhammer's MT decomposition**/
    /**Algorithms**/
    char d_axis[3];
    int d_mtrep;
    double d_mf[6],d_mt[3][3],d_d[3],d_v[3][3],d_miso[3][3],d_mdc[3][3],
	d_mclvd[3][3],d_m0,d_mw,d_plunge[3],d_azimuth[3],d_strike[3],
	d_slip[3],d_dip[3],d_pcdc,d_pcclvd,d_pciso;
    float gfscale=1.0e+20; /*Dyne cm*/
    /**************/
    float fullMo, Mo, isoMo, Mw, Strike, Rake, Dip, Pdc, Piso, Pclvd;
    float St2, Rk2, Dp2;
    int isoflag;

    if ( (par=fopen(MT_INFILE,"r")) == NULL) {
	fprintf(stderr, "Error opening %s: %s\n", MT_INFILE, strerror(errno));
	exit(1);
    }
    
    if ( (out=fopen(MT_OUTFILE,"a")) == NULL) {
	fprintf(stderr, "Error opening %s: %s\n", MT_OUTFILE, strerror(errno));
	exit(1);
    }

    fscanf(par,"%d %d %d %d %d\n",&nsta,&depth,&wflag,&isoflag,&plotflag);
    M.isoflag=isoflag;
    fprintf(out,"isoflag=%d Depth=%d\n",isoflag,depth);
#ifndef ONLY_ERRORS_TO_STDERR
    fprintf(stderr,"isoflag=%d Depth=%d\n",isoflag,depth);
#endif

    AIV=(double **)malloc(sizeof(double *)*isoflag);
    for(i=0 ; i < isoflag ; i++)
	AIV[i]=(double *)malloc(sizeof(double)*isoflag);

    B=(double **)malloc(sizeof(double *)*isoflag);
    for(i=0 ; i < isoflag; i++)
	B[i]=(double *)malloc(sizeof(double)*1);

    ss=(struct DATA *)malloc(sizeof(struct DATA)*(nsta));
    gg=(struct GREEN *)malloc(sizeof(struct GREEN)*(nsta));

    mindist=100000.0;
    WCNT=0;
    ntr = 0;
    for(i=0 ; i < nsta ; i++)
    {
	fscanf(par,"%s %f %f %d %d\n",infile,&dist,&(ss[i].azi),&Z,&Np);
	WCNT += Np;
	strcpy(ss[i].name,infile);
	ss[i].azi *= PI/180.0;

	/* Read in seismic trace data */
	readhelm(infile,&ntr,&npts,&dt,&tmp);
	if (ntr != 3) {
	    fprintf(stderr, "read %d traces from %s; 3 required\n",ntr,infile);
	    exit(1);
	}
	/* We assume all the traces have the same time step and length;
	   That's why we have tdmt_redi_sched, to do this checking */
	nn=npts[0];

	if(mindist > dist) mindist = dist;
	ss[i].dist=dist;
	ss[i].np=nn;
	ss[i].dt=dt[0];
	ss[i].nn=Np;
	ss[i].zz=Z;

	/*Allocate data structure arrays*/
	ss[i].t=(float *)malloc(sizeof(float)*nn);
	ss[i].r=(float *)malloc(sizeof(float)*nn);
	ss[i].z=(float *)malloc(sizeof(float)*nn);

	for(j=0 ; j < nn ; j++)
	{
	    ss[i].t[j]=tmp[j];
	    ss[i].r[j]=tmp[j + nn];
	    ss[i].z[j]=tmp[j + 2*nn];
	}
    }
    free(tmp);
    if (npts) free(npts);
    free(dt);
    
    /*Allocate Memory for Station Weights*/
    WCNT *= 3;
    W=(float *)malloc(sizeof(float)*WCNT);
    for(j=0; j < WCNT ; j++)
	W[j]=1.0;

    /*Allocate Memory for A matrix*/
    AJ=(double **)malloc(sizeof(double *)*isoflag);
    for(j=0 ; j < isoflag ; j++)
	AJ[j]=(double *)malloc(sizeof(double)*WCNT);

    /* Read in the Green's functions */
    ntr = 0;    
    for(i=0 ; i < nsta ; i++) {
	fscanf(par,"%s %d %d\n",infile,&Z,&Np);
	sprintf(binfile, "%s.bin", infile);
	if (access(binfile, R_OK) == 0)
	    readbin(binfile, &ntr, &npts, &dt, &tmp);
	else
	    readhelm(infile,&ntr,&npts,&dt,&tmp);

	if ( (isoflag == 5 && ntr < 8) || isoflag == 6 && ntr != 10) {
	    fprintf(stderr, "GF file %s has unexpected sector count: %d\n",
		    infile, ntr);
	    exit(1);
	}
	
	nn=npts[0];

	gg[i].np=nn;
	gg[i].dt=dt[0];
	gg[i].nn=Np;
	gg[i].zz=Z;
	
	/*Allocate Greens Function structure arrays*/
	gg[i].u1=(float *)malloc(sizeof(float)*nn);
	gg[i].u2=(float *)malloc(sizeof(float)*nn);
	gg[i].u3=(float *)malloc(sizeof(float)*nn);
	gg[i].u4=(float *)malloc(sizeof(float)*nn);
	gg[i].u5=(float *)malloc(sizeof(float)*nn);
	gg[i].u6=(float *)malloc(sizeof(float)*nn);
	gg[i].u7=(float *)malloc(sizeof(float)*nn);
	gg[i].u8=(float *)malloc(sizeof(float)*nn);
	gg[i].u9=(float *)malloc(sizeof(float)*nn);
	gg[i].u10=(float *)malloc(sizeof(float)*nn);

	for(j=0 ; j < nn ; j++) {
	    gg[i].u1[j]=tmp[j];
	    gg[i].u2[j]=tmp[j + nn];
	    gg[i].u3[j]=tmp[j + 2*nn];
	    gg[i].u4[j]=tmp[j + 3*nn];
	    gg[i].u5[j]=tmp[j + 4*nn];
	    gg[i].u6[j]=tmp[j + 5*nn] * (-1.0);/*Note the vertical GF's are*/ 
	    gg[i].u7[j]=tmp[j + 6*nn] * (-1.0);/*flipped in earqt1.f and TW's*/
	    gg[i].u8[j]=tmp[j + 7*nn] * (-1.0);/* Blackbox.f DVH conv. z + down*/
	    if(isoflag==6) {
		gg[i].u9[j]=tmp[j + 8*nn] * (-1.0);           /*Radial   exp*/
		gg[i].u10[j]=tmp[j + 9*nn] * (-1.0);          /*Vertical exp*/
	    }
	    if(isoflag==5) {
		gg[i].u9[j]=0.0;
		gg[i].u10[j]=0.0;
	    }
	}
    }
    free(tmp);
    free(npts);
    free(dt);

    /* Cross-Correlation to obtain zero shift*/
    for(i=0; i < nsta; i++) {
	N =ss[i].np;
	Np=ss[i].nn;
	//Z =ss[i].zz;
	np2 = (int)(log10((float)N)/log10(2.0) + 0.5);
	np2=2<<(np2-1);
	if(Z == 0)
	    ss[i].zz=correlate(ss,gg,i,np2);/*Compute cross-correlation*/
    Z=correlate(ss,gg,i,np2);/*Compute cross-correlation*/
    Zg = 0;
    Zs = 0;
    if (Z > 0)
        Zs = Z;
    if (Z < 0)
        Zg = Z;

	/*Return zshift for largest cor.*/
    }

    /*Construct distance 1/R weighting*/
    if(wflag==1 && (nsta >= 1)) {
#ifndef ONLY_ERRORS_TO_STDERR
	fprintf(stderr,"Station Information\n");
#endif
	fprintf(out,"Station Information\n");
	l=0;
	for(i=0; i < nsta; i++) {
	    cormax = ss[i].dist / mindist;
#ifndef ONLY_ERRORS_TO_STDERR
	    fprintf(stderr,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
#endif	    
	    fprintf(out,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
	    N = ss[i].nn;
	    for(j=0; j < 3*N; j++)
		W[l++]=cormax;
	}
    }
       
    /* INVERSION ROUTINE */

    for(i=0 ; i < isoflag ; i++)                /*Normalize AtA and AIV matix*/
	for(l=0 ; l < isoflag ; l++)
	    AIV[i][l]=0.0;

    for(i=0 ; i < isoflag ; i++)
	B[i][0]=0.0;

    cnt1=cnt2=cnt3=0;
    for(i=0; i < nsta; i++) {
	Np=ss[i].nn;
	//Z =gg[i].zz;
	cnt1=cnt2 = cnt3;
	cnt2 += Np;
	cnt3 += 2*Np;
	//for(j=Z; j < Z+Np; j++) {                /*Index over time*/
	for(j=Zg; j < Np-Zs; j++) {                /*Index over time*/
	    /*Mxx term*/
           AJ[0][cnt1]	    = (double)(0.5*sin(2*ss[i].azi)*gg[i].u1[j]);
	if(isoflag==6)
	  {
          AJ[0][cnt2]	     = (double)(0.166667*gg[i].u5[j] - 0.5*cos(2*ss[i].azi)*gg[i].u3[j] + 0.33333*gg[i].u9[j]);
	  AJ[0][cnt3]	     = (double)(0.166667*gg[i].u8[j] - 0.5*cos(2*ss[i].azi)*gg[i].u6[j] + 0.33333*gg[i].u10[j]);
	  }
	if(isoflag==5)
	  {
          AJ[0][cnt2]	     = (double)(0.500000*gg[i].u5[j] - 0.5*cos(2*ss[i].azi)*gg[i].u3[j]);
      	  AJ[0][cnt3]	     = (double)(0.500000*gg[i].u8[j] - 0.5*cos(2*ss[i].azi)*gg[i].u6[j]);
      	  }
	    /*Myy term*/
	 AJ[1][cnt1]	    = (double)((-0.5)*sin(2*ss[i].azi)*gg[i].u1[j]);
	if(isoflag==6)
	  {
         AJ[1][cnt2]	    = (double)(0.166667*gg[i].u5[j] + 0.5*cos(2*ss[i].azi)*gg[i].u3[j] + 0.33333*gg[i].u9[j]);
	 AJ[1][cnt3]	    = (double)(0.166667*gg[i].u8[j] + 0.5*cos(2*ss[i].azi)*gg[i].u6[j] + 0.33333*gg[i].u10[j]);
          }
        if(isoflag==5)
          {
         AJ[1][cnt2]	    = (double)(0.500000*gg[i].u5[j] + 0.5*cos(2*ss[i].azi)*gg[i].u3[j]);
	 AJ[1][cnt3]	    = (double)(0.500000*gg[i].u8[j] + 0.5*cos(2*ss[i].azi)*gg[i].u6[j]);
          }

	    /*Mxy term*/
	 AJ[2][cnt1]	    = (double)((-1.0)*cos(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[2][cnt2]	    = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u3[j]);
	 AJ[2][cnt3]	    = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u6[j]);

	 /*Mxz term*/
	 AJ[3][cnt1]        = (double)((-1.0)*sin(ss[i].azi)*gg[i].u2[j]);
	 AJ[3][cnt2]        = (double)(       cos(ss[i].azi)*gg[i].u4[j]);
	 AJ[3][cnt3]        = (double)(       cos(ss[i].azi)*gg[i].u7[j]);

	 /*Myz term*/
	 AJ[4][cnt1]        = (double)(       cos(ss[i].azi)*gg[i].u2[j]);
	 AJ[4][cnt2]        = (double)(       sin(ss[i].azi)*gg[i].u4[j]);
	 AJ[4][cnt3]        = (double)(       sin(ss[i].azi)*gg[i].u7[j]);

	 /*Mzz term*/
	 if(isoflag==6)
          {
         AJ[5][cnt1]	    = (double)(0.0);
	 AJ[5][cnt2]	    = (double)(0.33333*gg[i].u9[j]-0.33333*gg[i].u5[j]);
	 AJ[5][cnt3]	    = (double)(0.33333*gg[i].u10[j]-0.3333*gg[i].u8[j]);
          }


         cnt1++;
         cnt2++;
         cnt3++;
	}
    }


    for(i=0 ; i < isoflag ; i++)                /*Compute AtA                */
	for(j=0 ; j < isoflag ; j++)
	    for(k=0 ; k < cnt3 ; k++)
		AIV[i][j] += AJ[i][k]* AJ[j][k] * (double)W[k];


    cnt1=cnt2=cnt3=0;
    tmp=(float *)malloc(10*N*nn*sizeof(float));
    for(j=0; j < nsta; j++) {
	l=0;
	//Z =ss[j].zz;
	Np=ss[j].nn;
	cnt1=cnt2 = cnt3;
	cnt2 += Np;
	cnt3 += 2*Np;
	//for(i=Z ; i < Np+Z ; i++) {
	for(i=Zs ; i < Np-Zg ; i++) {
	    tmp[cnt1]        = ss[j].t[i];
	    tmp[cnt2]        = ss[j].r[i];
	    tmp[cnt3]        = ss[j].z[i];
	    cnt1++;
	    cnt2++;
	    cnt3++;
        }
    }

    for(i=0 ; i < isoflag ; i++)               /* Calculate Righthand Side */
	for(j=0 ; j < cnt3 ; j++)
	    B[i][0] += AJ[i][j] * (double)tmp[j] * (double)W[j];


    minvdbl(AIV,B,isoflag,1);                     /* Determine Solution Vector */
     
    M.mxx=(float)B[0][0];
    M.myy=(float)B[1][0];
    M.mxy=(float)B[2][0];
    M.mxz=(float)B[3][0];
    M.myz=(float)B[4][0];
if(isoflag==6)
    M.mzz=(float)B[5][0];
if(isoflag==5)
    M.mzz=-1.0*(M.mxx + M.myy);

    /*Call Bob's MT decomposition routines*/
    /*The minus one is needed to map Helmbergers convention into Aki's*/
    /*Jost and Hermann (1989) state that AKI's convention is -1*LANGSTONS*/

tracelang=(M.mxx+M.myy+M.mzz)/3; /*Trace of MT*/

    d_mtrep=1;
/*Convert deviatoric moment tensor to AKI convention*/
d_mt[0][0] = (double)(-1.0*gfscale*M.mxx);
d_mt[0][1] = (double)(-1.0*gfscale*M.mxy);
d_mt[0][2] = (double)(-1.0*gfscale*M.mxz);
d_mt[1][0] = (double)(-1.0*gfscale*M.mxy);
d_mt[1][1] = (double)(-1.0*gfscale*M.myy);
d_mt[1][2] = (double)(-1.0*gfscale*M.myz);
d_mt[2][0] = (double)(-1.0*gfscale*M.mxz);
d_mt[2][1] = (double)(-1.0*gfscale*M.myz);
d_mt[2][2] = (double)(-1.0*gfscale*M.mzz);

fprintf(out,"isomoment=%g\n",tracelang*gfscale);


    fprintf(out,"MT Aki Convention\n");
    fprintf(out,"Mxx=%.3f\nMxy=%.3f\nMxz=%.3f\nMyy=%.3f\nMyz=%.3f\nMzz=%.3f\n",
	    d_mt[0][0]/gfscale,d_mt[0][1]/gfscale,d_mt[0][2]/gfscale,d_mt[1][1]/gfscale,
	    d_mt[1][2]/gfscale,d_mt[2][2]/gfscale);

    m0dcf_(&d_mtrep,d_mf,d_mt,d_d,d_v,d_miso,d_mdc,d_mclvd,
	   &d_m0,&d_mw,d_axis,d_plunge,d_azimuth,d_strike,d_dip,
	   d_slip,&d_pcdc,&d_pcclvd,&d_pciso);
 
    eigen1=(float)d_d[0];
    eigen2=(float)d_d[1];
    eigen3=(float)d_d[2];
    fprintf(stderr,"eigen1=%g\n",eigen1);
    fprintf(stderr,"eigen2=%g\n",eigen2);
    fprintf(stderr,"eigen3=%g\n",eigen3);
    fprintf(out,"eigen1=%g\n",eigen1);
    fprintf(out,"eigen2=%g\n",eigen2);
    fprintf(out,"eigen3=%g\n",eigen3);
    eigaz1=(float)d_azimuth[0];
    eigaz2=(float)d_azimuth[1];
    eigaz3=(float)d_azimuth[2];
    eigpl1=(float)d_plunge[0];
    eigpl2=(float)d_plunge[1];
    eigpl3=(float)d_plunge[2];

    Mo = (float) d_m0;
    Mw = (float) d_mw;
    Strike = (float) d_strike[0];
    Rake   = (float) d_slip[0];
    Dip    = (float) d_dip[0];
    St2    = (float) d_strike[1];
    Rk2    = (float) d_slip[1];
    Dp2    = (float) d_dip[1];
    Pdc    = (float) d_pcdc;
    Pclvd  = (float) d_pcclvd;
    Piso    = (float) d_pciso;
    isoMo  = tracelang*gfscale;
    fullMo = fabs(isoMo) + fabs(eigen3);
    if(isoflag == 6)
       Mw = (log10(fullMo)-16.05)*2/3;

#ifndef ONLY_ERRORS_TO_STDERR
    fprintf(stderr,"isoMo: %g\n",isoMo);
    fprintf(stderr,"Mo=%g\nMoFull=%g\nMw=%.2f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
	    Mo,fullMo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);
#endif
    fprintf(out,"Mo=%g\nMoFull=%g\nMw=%.2f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f ; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
	    Mo,fullMo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);

    fitcheck(ss,gg,W,M,Mo,nsta,isoflag,&E,&VR);  /*Function to compute vr and flag bad stations*/

#ifdef TDMT_PLOT
    //if(plotflag==1)
	//mt_plot(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,d_mt,Pdc,Pclvd,Piso,Mo,Mw,E,VR);
#endif

    fprintf(out,"Variance=%.3e\n",E);
    fprintf(out,"VarRed=%.3e\n",VR);
#ifndef ONLY_ERRORS_TO_STDERR
    fprintf(stderr,"Var/Pdc=%.3e\n",E/Pdc);
#endif
    fprintf(out,"Var/Pdc=%.3e\n",E/Pdc);

    if(VR < 20.0) QUAL=0;
    if(VR > 20.0 && VR < 40.0) QUAL=1;
    if(VR > 40.0 && VR < 60.0) QUAL=2;
    if(VR > 60.0 && VR < 80.0) QUAL=3;
    if(VR > 80.0) QUAL=4;
    fprintf(out,"Quality=%d\n",QUAL);
#ifndef ONLY_ERRORS_TO_STDERR
    fprintf(stderr,"Quality=%d\n",QUAL);
#endif

    if (fscanf(par, "%s\n", datfile) == 1) {
	mt_dat(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,-gfscale,Pdc,Pclvd,Piso,Mo,Mw,E,VR,depth,datfile);
    }
    fclose(par);
    fclose(out);
    
    return 0;
}


