/*Copyright (c) Douglas Dreger
Berkeley Seismological Laboratory
University of California, Berkeley */
#include"tdmtinv_iso.h"
#include <math.h>

fitcheck(ss,gg,W,M,Mo,nsta,degree,var,vred)
 struct MOMENT M;
 struct DATA  *ss;
 struct GREEN *gg;
 int nsta,degree;
 float *W, Mo, *var, *vred;
   {
   int i,j,Z,Zd,Zg,Np,cnt;
   float Dpower, Dtot, Etmp, E, Etot,VAR,DVAR, WSUM, Az, Mscl;

   Mscl = Mo/1.0e+20;
   WSUM=VAR=DVAR=Dtot=Etot=0.0;
   cnt=0;
   for(i=0; i < nsta; i++)
      {
      Dpower=0.0;
      Etmp  =0.0;
      E     =0.0;
      Zd=ss[i].zz;
      Zg=gg[i].zz;
      Np=ss[i].nn;
      Az=ss[i].azi;

      // Make sure that we cut the arrays to the
      // Z shift needed.
      // Zg in this case is always 0
      Z = 0;
      if ( abs(Zd) > 0 ) {
          Z = abs(Zd);
          Zd = Z;
          Zg = 0;
      }
      if ( Zd < 0 ) {
          Z = abs(Zd);
          Zd = 0;
          Zg = Z;
      }

      for(j=0; j < Np-Z; j++)
	 {
	 Etmp = ss[i].t[Zd+j] - (M.mxx*0.5*gg[i].u1[j+Zg]*sin(2*Az)
			       - M.myy*0.5*gg[i].u1[j+Zg]*sin(2*Az)
			       - M.mxy*gg[i].u1[j+Zg]*cos(2*Az)
			       - M.mxz*gg[i].u2[j+Zg]*sin(Az)
			       + M.myz*gg[i].u2[j+Zg]*cos(Az));
	 E += Etmp*Etmp;
         Etmp = ss[i].r[Zd+j] - (M.mxx*0.166667*gg[i].u5[j+Zg] - M.mxx*0.5*gg[i].u3[j+Zg]*cos(2*Az) + M.mxx*0.3333*gg[i].u9[j+Zg]
			       + M.myy*0.166667*gg[i].u5[j+Zg] + M.myy*0.5*gg[i].u3[j+Zg]*cos(2*Az) + M.myy*0.3333*gg[i].u9[j+Zg]
                	       + M.mzz*0.3333*gg[i].u9[j+Zg] - M.mzz*0.33333*gg[i].u5[j+Zg]
			       - M.mxy*gg[i].u3[j+Zg]*sin(2*Az)
			       + M.mxz*gg[i].u4[j+Zg]*cos(Az)
			       + M.myz*gg[i].u4[j+Zg]*sin(Az));
	 E += Etmp*Etmp;
         Etmp = ss[i].z[Zd+j] - (M.mxx*0.166667*gg[i].u8[j+Zg] - M.mxx*0.5*gg[i].u6[j+Zg]*cos(2*Az) +M.mxx*0.3333*gg[i].u10[j+Zg]
			       + M.myy*0.166667*gg[i].u8[j+Zg] + M.myy*0.5*gg[i].u6[j+Zg]*cos(2*Az) +M.myy*0.3333*gg[i].u10[j+Zg]
		 	       + M.mzz*0.3333*gg[i].u10[j+Zg] - M.mzz*0.33333*gg[i].u8[j+Zg] 
			       - M.mxy*gg[i].u6[j+Zg]*sin(2*Az)
			       + M.mxz*gg[i].u7[j+Zg]*cos(Az)
			       + M.myz*gg[i].u7[j+Zg]*sin(Az));
	 E += Etmp*Etmp;
	 Dpower += ss[i].t[Zd+j]*ss[i].t[Zd+j];
	 Dpower += ss[i].r[Zd+j]*ss[i].r[Zd+j];
	 Dpower += ss[i].z[Zd+j]*ss[i].z[Zd+j];
	 cnt++;
	 }
	 WSUM += W[3*cnt-1];
	 Etot += E;
	 VAR += W[3*cnt-1]*E;
	 Dtot += Dpower;
	 DVAR += W[3*cnt-1]*Dpower;
	 E /= Dpower;
	 ss[i].vr = (1.0 - E)*100.0;
         fprintf(stderr,"Station(%d)=%f  %g\n",i,ss[i].vr,Dpower);
      }
     *var = Etot/(3.0*(float)cnt - (float)degree - 1.0);
      fprintf(stderr,"VAR=%g\n",*var);
      Etot /= Dtot;
      *vred = (1.0-Etot)*100.0;
      fprintf(stderr,"VR=%.2f  (UNWEIGHTED)\n",*vred);
      VAR /= WSUM;
      DVAR /= WSUM;
      VAR /= DVAR;
      VAR = (1.0-VAR)*100.0;
      fprintf(stderr,"VR=%.2f  (WEIGHTED)\n",VAR);
      *vred=VAR;


   }/*fitcheck end*/

