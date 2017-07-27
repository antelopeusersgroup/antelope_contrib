#include<math.h>
#include<stdio.h>
#include<string.h>
#include<fcntl.h>
//#include<stdio.h>

main(ac,av)
int ac;
char **av;
  {
  int i,n,ntr,*npts, nn;
  int ihd[40], fdt, fdr, fdz;
  float A[6], *dt, pi=3.14159, *tan,*rad,*ver, *tmp;
  float fhd[70], azimuth, strike, rake, dip;
  float Mo=1.0e+20, isoMo=0.0;
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
  mstpar("azimuth","f",&azimuth);
  mstpar("strike","f",&strike);
  mstpar("rake","f",&rake);
  mstpar("dip","f",&dip);
  getpar("moment","f",&Mo);
  getpar("isomoment","f",&isoMo);
  endpar(ac,av);
  
  tan=(float *)malloc(sizeof(float)*1*4000);
  rad=(float *)malloc(sizeof(float)*1*4000);
  ver=(float *)malloc(sizeof(float)*1*4000);
  tmp=(float *)malloc(sizeof(float)*10*4000);
  npts=(int *)malloc(sizeof(int)*10);
  dt  =(float *)malloc(sizeof(float)*10);

  strike = azimuth - strike;
  fprintf(stderr,"str=%.2f  rake=%.2f  dip=%.2f\n",strike, rake, dip);
  strike *= pi/180.0;
  rake *= pi/180.0;
  dip *= pi/180.0;
  
  Mo /= 1.0e+20;
  isoMo /=1.0e+20;
  A[0]=sin(2.0*strike)*cos(rake)*sin(dip) + 0.5*cos(2.0*strike)*sin(rake)*sin(2.0*dip); 
  A[1]=cos(strike)*cos(rake)*cos(dip) - sin(strike)*sin(rake)*cos(2.0*dip);
  A[2]=0.5*sin(rake)*sin(2.0*dip);
  A[3]=cos(2.0*strike)*cos(rake)*sin(dip) - 0.5*sin(2.0*strike)*sin(rake)*sin(2.0*dip); 
  A[4]=sin(strike)*cos(rake)*cos(dip) + cos(strike)*sin(rake)*cos(2.0*dip);
  A[4] *= -1.0;

fprintf(stderr,"A1=%f A2=%f A3=%f A4=%f A5=%f\n",A[0],A[1],A[2],A[3],A[4]);

  readhelm(infile,&ntr,npts,dt,tmp);
  if(ntr != 10)
    {
    fprintf(stderr,"GF file not correct length\n");
    exit(-1);
    }

  nn=npts[0];
  fprintf(stderr,"nn=%d\n",nn);
  ihd[9]=nn;
  fhd[0]=dt[0];

  for (i=0; i < nn; i++)
      {
      tan[i]=Mo*(A[3]*tmp[i] + A[4]*tmp[i+nn]);
      rad[i]=Mo*(A[0]*tmp[i+2*nn] + A[1]*tmp[i+3*nn] + A[2]*tmp[i+4*nn])
	    + isoMo*tmp[i+8*nn];
      ver[i]=-1.0*Mo*(A[0]*tmp[i+5*nn] + A[1]*tmp[i+6*nn] + A[2]*tmp[i+7*nn])
	    + isoMo*tmp[i+9*nn];
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
