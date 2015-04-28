/* Moment Tensor Inversion */
/* Uses previously determined Green's functions in D. Helmbergers format*/
#include     <stdlib.h>

#include"tdmt_invb.h"

 void gaussj();
 int correlate(),fclose();
 FILE *fopen(), *par,*fd, *fd1, *fd2, *fd3, *out;

main()
 {
 int i,j,l,N,Np,Z,k,ntr,nsta,vsize,nn,*npts,wflag=0;
 int depth,cnt1, cnt2, cnt3, np2,QUAL,plotflag,WCNT;
 float *W, *tmpp,*tmp,*dt, cormax, mindist, dist,E,VR,junk;
 double **AIV,**AJ, **B;
 struct MOMENT M;
 struct DATA *ss;
 struct GREEN *gg;
 char infile[50],line[50];
 /**Variables needed for implementation of R Urhammers MT decomposition**/
 /**Algorythms**/
 char d_axis[3];
 int d_mtrep;
 double d_mf[6],d_mt[3][3],d_d[3],d_v[3][3],d_miso[3][3],d_mdc[3][3],
	d_mclvd[3][3],d_m0,d_mw,d_plunge[3],d_azimuth[3],d_strike[3],
	d_slip[3],d_dip[3],d_pcdc,d_pcclvd,d_pciso;
 float gfscale=1.0e+20; /*Dyne cm*/
 /**************/
 float Mo, Mw, Strike, Rake, Dip, Pdc, Piso, Pclvd;
 float St2, Rk2, Dp2;
 int isoflag=M.isoflag=5;

par=fopen("mt_inv.in","r");
out=fopen("mt_inv_redi.out","a");

fscanf(par,"%d %d %d %d\n",&nsta,&depth,&wflag,&plotflag);
fprintf(out,"Depth=%d\n",depth);
fprintf(stderr,"Depth=%d\n",depth);

AIV=(double **)malloc(sizeof(double *)*5);
for(i=0 ; i < 5 ; i++)
 AIV[i]=(double *)malloc(sizeof(double)*5);

 npts=(int *)malloc(sizeof(int)*10);
 dt  =(float *)malloc(sizeof(float)*10);


B=(double **)malloc(sizeof(double *)*5);
for(i=0 ; i < 5; i++)
 B[i]=(double *)malloc(sizeof(double)*1);


ss=(struct DATA *)malloc(sizeof(struct DATA)*(2*nsta));
gg=(struct GREEN *)malloc(sizeof(struct GREEN)*(2*nsta));



  mindist=100000.0;
  WCNT=0;
  for(i=0 ; i < nsta ; i++)
   {
   fscanf(par,"%s %f %f %d %d\n",infile,&dist,&(ss[i].azi),&Z,&Np);
   WCNT += Np;
   strcpy(ss[i].name,infile);
   ss[i].azi *= PI/180.0;
   fd=fopen(infile,"r");
   fgets(line,100,fd);
   sscanf(line,"%d",&N);
   fgets(line,100,fd);
   fgets(line,100,fd);
   fgets(line,100,fd);
   sscanf(line,"%d %f",&vsize,&junk);
   fclose(fd);

   tmp=(float *)malloc(3*vsize*sizeof(float));

   readhelm(infile,&ntr,npts,dt,tmp);      /*Open and Read Seismograms*/
   nn=npts[0];

   if(mindist > dist) mindist = dist;
   ss[i].dist=dist;
   ss[i].np=nn;
   ss[i].dt=dt[0];
   ss[i].nn=Np;
   ss[i].zz=Z;


   /*Allocate data structure arrays*/
   ss[i].t=(float *)malloc(sizeof(float)*vsize*2);
   ss[i].r=(float *)malloc(sizeof(float)*vsize*2);
   ss[i].z=(float *)malloc(sizeof(float)*vsize*2);

   for(j=0 ; j < nn ; j++)
    {
    ss[i].t[j]=tmp[j];
    ss[i].r[j]=tmp[j + nn];
    ss[i].z[j]=tmp[j + 2*nn];
    }
   free(tmp);
   }

   /*Allocate Memory for Station Weights*/
   WCNT *= 3;
   W=(float *)malloc(sizeof(float)*WCNT);
         for(j=0; j < WCNT ; j++)
             W[j]=1.0;

   /*Allocate Memory for A matrix*/
   AJ=(double **)malloc(sizeof(double *)*5);
         for(j=0 ; j < 5 ; j++)
             AJ[j]=(double *)malloc(sizeof(double)*WCNT);

   for(i=0 ; i < nsta ; i++)
   {
   fscanf(par,"%s %d %d\n",infile,&Z,&Np);
   fd=fopen(infile,"r");
   fgets(line,100,fd);
   sscanf(line,"%d",&N);
   fgets(line,100,fd);
   fgets(line,100,fd);
   fgets(line,100,fd);
   sscanf(line,"%d %f",&vsize,&junk);
   fclose(fd);

   tmp=(float *)malloc(8*vsize*sizeof(float));

   readhelm(infile,&ntr,npts,dt,tmp);    /*Open and Read Green's Func*/
   nn=npts[0];

   gg[i].np=nn;
   gg[i].dt=dt[0];
   gg[i].nn=Np;
   gg[i].zz=Z;

   /*Allocate Greens Function structure arrays*/
   gg[i].u1=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u2=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u3=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u4=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u5=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u6=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u7=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u8=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u9=(float *)malloc(sizeof(float)*vsize*2);
   gg[i].u10=(float *)malloc(sizeof(float)*vsize*2);

   for(j=0 ; j < nn ; j++)
    {
    gg[i].u1[j]=tmp[j];
    gg[i].u2[j]=tmp[j + nn];
    gg[i].u3[j]=tmp[j + 2*nn];
    gg[i].u4[j]=tmp[j + 3*nn];
    gg[i].u5[j]=tmp[j + 4*nn];
    gg[i].u6[j]=tmp[j + 5*nn] * (-1.0);/*Note the vertical GF's are*/ 
    gg[i].u7[j]=tmp[j + 6*nn] * (-1.0);/*flipped in earqt1.f and TW's*/
    gg[i].u8[j]=tmp[j + 7*nn] * (-1.0);/* Blackbox.f DVH conv. z + down*/
  if(isoflag==6)
      {
      gg[i].u9[j]=tmp[j + 8*nn];           /*Radial   exp*/
      gg[i].u10[j]=tmp[j + 9*nn];          /*Vertical exp note polarity*/
      }
  if(isoflag==5)
      {
      gg[i].u9[j]=0.0;
      gg[i].u10[j]=0.0;
      }
    }
   free(tmp);
   }

   /* Cross-Correlation to obtain zero shift*/
   for(i=0; i < nsta; i++)
     {
     N =ss[i].np;
     Np=ss[i].nn;
     Z =ss[i].zz;
     np2 = (int)(log10((float)N)/log10(2.0) + 0.5);
     np2=2<<(np2-1);
     if(Z == 0)
       ss[i].zz=correlate(ss,gg,i,np2);/*Compute cross-correlation*/
                                       /*Return zshift for largest cor.*/


     }



/*Construct distance 1/R weighting*/
if(wflag==1 && (nsta >= 1))
  {
  fprintf(stderr,"Station Information\n");
  fprintf(out,"Station Information\n");
  l=0;
  for(i=0; i < nsta; i++)
     {
     cormax = ss[i].dist / mindist;
     fprintf(stderr,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
     fprintf(out,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
     N = ss[i].nn;
     for(j=0; j < 3*N; j++)
        W[l++]=cormax;

     }
  }
       


   /* INVERSION ROUTINE */

    for(i=0 ; i < 5 ; i++)                /*Normalize AtA and AIV matix*/
     for(l=0 ; l < 5 ; l++)
       AIV[i][l]=0.0;

    for(i=0 ; i < 5 ; i++)
      B[i][0]=0.0;

    cnt1=cnt2=cnt3=0;
    for(i=0; i < nsta; i++)
       {
       Np=ss[i].nn;
       Z =gg[i].zz;
       cnt1=cnt2 = cnt3;
       cnt2 += Np;
       cnt3 += 2*Np;
       for(j=Z; j < Z+Np; j++)                 /*Index over time*/
	 {
         /*Mxx term*/
	 AJ[0][cnt1]        = (double)(0.5*sin(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[0][cnt2]        = (double)(0.5*(gg[i].u5[j] - cos(2*ss[i].azi)*gg[i].u3[j]));
	 AJ[0][cnt3]        = (double)(0.5*(gg[i].u8[j] - cos(2*ss[i].azi)*gg[i].u6[j]));

         /*Myy term*/
	 AJ[1][cnt1]        = (double)((-0.5)*sin(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[1][cnt2]        = (double)(0.5*(gg[i].u5[j] + cos(2*ss[i].azi)*gg[i].u3[j]));
	 AJ[1][cnt3]        = (double)(0.5*(gg[i].u8[j] + cos(2*ss[i].azi)*gg[i].u6[j]));

         /*Mxy term*/
	 AJ[2][cnt1]        = (double)((-1.0)*cos(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[2][cnt2]        = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u3[j]);
	 AJ[2][cnt3]        = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u6[j]);

         /*Mxz term*/
	 AJ[3][cnt1]        = (double)((-1.0)*sin(ss[i].azi)*gg[i].u2[j]);
	 AJ[3][cnt2]        = (double)(       cos(ss[i].azi)*gg[i].u4[j]);
	 AJ[3][cnt3]        = (double)(       cos(ss[i].azi)*gg[i].u7[j]);

         /*Myz term*/
	 AJ[4][cnt1]        = (double)(       cos(ss[i].azi)*gg[i].u2[j]);
	 AJ[4][cnt2]        = (double)(       sin(ss[i].azi)*gg[i].u4[j]);
	 AJ[4][cnt3]        = (double)(       sin(ss[i].azi)*gg[i].u7[j]);

	 cnt1++;
	 cnt2++;
	 cnt3++;
	 }
	 }




    for(i=0 ; i < 5 ; i++)                /*Compute AtA                */
     for(j=0 ; j < 5 ; j++)
      for(k=0 ; k < cnt3 ; k++)
	AIV[i][j] += AJ[i][k]* AJ[j][k] * (double)W[k];


    cnt1=cnt2=cnt3=0;
    tmp=(float *)malloc(10*N*vsize*sizeof(float));
    for(j=0; j < nsta; j++)
      {
      l=0;
      Z =ss[j].zz;
      Np=ss[j].nn;
      cnt1=cnt2 = cnt3;
      cnt2 += Np;
      cnt3 += 2*Np;
      for(i=Z ; i < Np+Z ; i++)
        {
        tmp[cnt1]        = ss[j].t[i];
        tmp[cnt2]        = ss[j].r[i];
        tmp[cnt3]        = ss[j].z[i];
	cnt1++;
	cnt2++;
	cnt3++;
        }
       }

    for(i=0 ; i < 5 ; i++)               /* Calculate Righthand Side */
      for(j=0 ; j < cnt3 ; j++)
         B[i][0] += AJ[i][j] * (double)tmp[j] * (double)W[j];


    minvdbl(AIV,B,5,1);                     /* Determine Solution Vector */

    M.mxx=(float)B[0][0];
    M.myy=(float)B[1][0];
    M.mxy=(float)B[2][0];
    M.mxz=(float)B[3][0];
    M.myz=(float)B[4][0];
    M.mzz=-1.0*(M.mxx + M.myy);

/*Call Bobs MT decomposition routines*/
/*The minus one is needed to map Helmbergers convention into Aki's*/
/*Jost and Hermann (1989) state that AKI's convention is -1*LANGSTONS*/
d_mtrep=1;
d_mt[0][0] = (double)(-1.0*gfscale*B[0][0]);
d_mt[0][1] = (double)(-1.0*gfscale*B[2][0]);
d_mt[0][2] = (double)(-1.0*gfscale*B[3][0]);
d_mt[1][0] = (double)(-1.0*gfscale*B[2][0]);
d_mt[1][1] = (double)(-1.0*gfscale*B[1][0]);
d_mt[1][2] = (double)(-1.0*gfscale*B[4][0]);
d_mt[2][0] = (double)(-1.0*gfscale*B[3][0]);
d_mt[2][1] = (double)(-1.0*gfscale*B[4][0]);
d_mt[2][2] = -1.0*(d_mt[0][0] + d_mt[1][1]);

fprintf(out,"Mxx=%.3f\nMxy=%.3f\nMxz=%.3f\nMyy=%.3f\nMyz=%.3f\nMzz=%.3f\n",
       d_mt[0][0]/gfscale,d_mt[0][1]/gfscale,d_mt[0][2]/gfscale,d_mt[1][1]/gfscale,
       d_mt[1][2]/gfscale,d_mt[2][2]/gfscale);

m0dcf_(&d_mtrep,d_mf,d_mt,d_d,d_v,d_miso,d_mdc,d_mclvd,
       &d_m0,&d_mw,d_axis,d_plunge,d_azimuth,d_strike,d_dip,
       d_slip,&d_pcdc,&d_pcclvd,&d_pciso);

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

fprintf(stderr,"Mo=%g\nMw=%.1f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
       Mo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);
fprintf(out,"Mo=%g\nMw=%.1f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f ; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
       Mo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);

fitcheck(ss,gg,W,M,Mo,nsta,isoflag,&E,&VR);  /*Function to compute vr and flag bad stations*/

if(plotflag==1)
  mt_plot(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,d_mt,Pdc,Pclvd,Piso,Mo,Mw,E,VR);
fprintf(out,"Variance=%.3e\n",E);
fprintf(out,"VarRed=%.3e\n",VR);
fprintf(stderr,"Var/Pdc=%.3e\n",E/Pdc);
fprintf(out,"Var/Pdc=%.3e\n",E/Pdc);
if(VR < 20.0) QUAL=0;
if(VR > 20.0 && VR < 40.0) QUAL=1;
if(VR > 40.0 && VR < 60.0) QUAL=2;
if(VR > 60.0 && VR < 80.0) QUAL=3;
if(VR > 80.0) QUAL=4;
fprintf(out,"Quality=%d\n",QUAL);
fprintf(stderr,"Quality=%d\n",QUAL);





}

