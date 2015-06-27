#include<math.h>
#include<stdio.h>
#include<string.h>
#include<fcntl.h>

main(ac,av)
int ac;
char **av;
  {
  int i,n,ntr,*npts, nn;
  int ihd[40], fdt, fdr, fdz;
  float A[6], *dt, pi=3.14159, *tan,*rad,*ver, *tmp;
  float fhd[70], Az,mxx,mxy,mxz,myy,myz,mzz;
  float Mo=1.0e+20;
  char infile[120],outname[120], name[120];
  char chd[8][24];

  for(i=0;i<40;i++) ihd[i]=-12345;
  for(i=0;i<70;i++) fhd[i]=-12345.00;
  for(i=0;i<8;i++)  sprintf(chd[i],"-12345  -12345  -12345  ");
  /*Set essential sac parameters*/
  ihd[35]=1;                  /*Sets file to evenly spaced*/
  ihd[15]=1;                  /*Sets file type to Timeseries*/
  ihd[6]=6;                   /*Variable Name Internal */
  fhd[5]=0.0;		      /*B variable*/


  setpar(ac,av);
  mstpar("in","s",infile);
  mstpar("out","s",outname);
  mstpar("azimuth","f",&Az);
  mstpar("mxx","f",&mxx);
  mstpar("mxy","f",&mxy);
  mstpar("mxz","f",&mxz);
  mstpar("myy","f",&myy);
  mstpar("myz","f",&myz);
  mstpar("mzz","f",&mzz);
  getpar("moment","f",&Mo);
  endpar(ac,av);

  Az *= pi/180.0;
  
  tan=(float *)malloc(sizeof(float)*1*8000);
  rad=(float *)malloc(sizeof(float)*1*8000);
  ver=(float *)malloc(sizeof(float)*1*8000);
  tmp=(float *)malloc(sizeof(float)*10*8000);
  npts=(int *)malloc(sizeof(int)*10);
  dt  =(float *)malloc(sizeof(float)*10);

  Mo /= 1.0e+20;

  readhelm(infile,&ntr,npts,dt,tmp);

  nn=npts[0];
  fprintf(stderr,"nn=%d\n",nn);
  ihd[9]=nn;
  fhd[0]=dt[0];

  for (i=0; i < nn; i++)   /*Apply the -1 correction to the zss,zds and zdd comps, zexp ok*/
      {
      tmp[i+5*nn] *= -1.0; 
      tmp[i+6*nn] *= -1.0; 
      tmp[i+7*nn] *= -1.0; 
      }

  for (i=0; i < nn; i++)
      {
      tan[i]=(mxx*0.5*tmp[i]*sin(2*Az)
		- myy*0.5*tmp[i]*sin(2*Az)
		- mxy*tmp[i]*cos(2*Az)
		- mxz*tmp[i+nn]*sin(Az)
		+ myz*tmp[i+nn]*cos(Az));

      rad[i]= (mxx*0.5*tmp[i+4*nn]
		 - mxx*0.5*tmp[i+2*nn]*cos(2*Az)
		  + mxx*0.3333*tmp[i+8*nn]
		   + myy*0.5*tmp[i+4*nn]
		    + myy*0.5*tmp[i+2*nn]*cos(2*Az)
		     + myy*0.3333*tmp[i+8*nn]
		      + mzz*0.3333*tmp[i+8*nn]
		       - mxy*tmp[i+2*nn]*sin(2*Az)
			+ mxz*tmp[i+3*nn]*cos(Az)
			 + myz*tmp[i+3*nn]*sin(Az));

	ver[i]= (mxx*0.5*tmp[i+7*nn]
		 - mxx*0.5*tmp[i+5*nn]*cos(2*Az)
		  + mxx*0.3333*tmp[i+9*nn]
		   + myy*0.5*tmp[i+7*nn]
		    + myy*0.5*tmp[i+5*nn]*cos(2*Az)
		     + myy*0.3333*tmp[i+9*nn]
		      + mzz*0.3333*tmp[i+9*nn]
		       - mxy*tmp[i+5*nn]*sin(2*Az)
			+ mxz*tmp[i+6*nn]*cos(Az)
			 + myz*tmp[i+6*nn]*sin(Az));

      }

sprintf(name,"%s.tan",outname);
fdt=open(name,O_WRONLY | O_CREAT | O_TRUNC, 0644);
sprintf(name,"%s.rad",outname);
fdr=open(name,O_WRONLY | O_CREAT | O_TRUNC, 0644);
sprintf(name,"%s.ver",outname);
fdz=open(name,O_WRONLY | O_CREAT | O_TRUNC, 0644);

write(fdt,fhd,70*4);  /*Write Sac Float Field*/
write(fdt,ihd,40*4);  /*Write Sac Int   Field*/
write(fdt,chd,24*8);  /*Write Sac Char. Field*/
write(fdt,tan,ihd[9]*4); /*Write timeseries file*/
write(fdr,fhd,70*4);  /*Write Sac Float Field*/
write(fdr,ihd,40*4);  /*Write Sac Int   Field*/
write(fdr,chd,24*8);  /*Write Sac Char. Field*/
write(fdr,rad,ihd[9]*4); /*Write timeseries file*/
write(fdz,fhd,70*4);  /*Write Sac Float Field*/
write(fdz,ihd,40*4);  /*Write Sac Int   Field*/
write(fdz,chd,24*8);  /*Write Sac Char. Field*/
write(fdz,ver,ihd[9]*4); /*Write timeseries file*/

}/*END*/
