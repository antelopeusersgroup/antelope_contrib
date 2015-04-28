#include<sys/file.h>
#include<stdio.h>
#include <math.h>

/*convert headerless binary file (fromHelm.c generated) to SAC binary file */
main(ac,av)
 int  ac;
 char **av;
 {
 int i, npts, year, jday, hour, min, sec, msec, ihd[40];
 float dt, B=0.0, E, fhd[70], *tr, evla, evlo, stla, stlo, dist;
 float azi, bazi, cmpaz, cmpinc;
char chd[8][24], ename[16], sname[8];


/* Initialize Header */
for(i=0;i<40;i++) ihd[i]=-12345;
for(i=0;i<70;i++) fhd[i]=-12345.00;
for(i=0;i<8;i++)  sprintf(chd[i],"-12345  -12345  -12345  ");

/* Fixing the length of the the chars to avoid
 * a "abort 6" error on my system. Juan Reyes
 *   sprintf(ename,"-12345  -12345  ");
 *   sprintf(sname,"-12345  ");
 */
sprintf(ename,"-12345  -12345 ");
sprintf(sname,"-12345 ");

/* Set Essential Parameters */
ihd[35]=1;                  /*Sets file to evenly spaced*/
ihd[15]=1;                  /*Sets file type to Timeseries*/
ihd[6]=6;                   /*Variable Name Internal */


setpar(ac,av);
mstpar("npts","d",&npts);
  ihd[9]=npts;
mstpar("dt","f",&dt);
  fhd[0]=dt;
if(getpar("stime","f",&B))
  fhd[5]=B;
if(getpar("year","d",&year))
   ihd[0]=year;
if(getpar("jday","d",&jday))
   ihd[1]=jday;
if(getpar("hour","d",&hour))
   ihd[2]=hour;
if(getpar("min","d",&min))
   ihd[3]=min;
if(getpar("sec","d",&sec))
   ihd[4]=sec;
if(getpar("msec","d",&msec))
   ihd[5]=msec;

getpar("ename","s",ename);         /*Event   name*/
getpar("sname","s",sname);         /*Station name*/

if(getpar("cmpaz","f",&cmpaz))
   fhd[57]=cmpaz;
if(getpar("cmpinc","f",&cmpinc))
   fhd[58]=cmpinc;

if(getpar("stla","f",&stla))       /*Station lat and lon.*/
   {
   fhd[31]=stla;
   fprintf(stderr,"%f\n",fhd[31]);
   }
if(getpar("stlo","f",&stlo))
   fhd[32]=stlo;
if(getpar("evla","f",&evla))          /*Event lat and lon.*/
   fhd[35]=evla;
if(getpar("evlo","f",&evlo))
   fhd[36]=evlo;
if(getpar("dist","f",&dist))
   fhd[50]=dist;
if(getpar("azi","f",&azi))
   fhd[51]=azi;
if(getpar("bazi","f",&bazi))
   fhd[52]=bazi;
endpar();
sprintf(chd[0],"%-8s%-16s",sname,ename);
fhd[6]=B+fhd[0]*(ihd[9]-1);        /*Set E variable*/
tr=(float *)malloc(sizeof(float)*npts);
read(0,tr,npts*sizeof(float));

write(1,fhd,70*4);  /*Write Sac Float Field*/
write(1,ihd,40*4);  /*Write Sac Int   Field*/
write(1,chd,24*8);  /*Write Sac Char. Field*/
write(1,tr,ihd[9]*4); /*Write timeseries file*/
}


