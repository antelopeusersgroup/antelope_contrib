#include<sys/file.h>
#include<fcntl.h>
#include<stdio.h>
#include <math.h>

/*convert SAC binary to headerless binary file (fromHelm.c generated) */
/*or Helmberger ascii */
main(ac,av)
 int ac;
 char **av;
 {
 int i, npts, ihd[40],fd1, fd2, binflag=1;
 float dt, fhd[70], *tr;
 char chd[8][24], in[100], out[100], line[100];

setpar(ac,av);
mstpar("in","s",in);
mstpar("out","s",out);
getpar("binflag","d",&binflag);
endpar();

fd1=open(in,O_RDONLY,0644);
fd2=open(out,O_WRONLY | O_CREAT | O_TRUNC,0644);


read(fd1,fhd,70*4);  /*Read Sac Float Field*/
read(fd1,ihd,40*4);  /*Read Sac Int   Field*/
read(fd1,chd,24*8);  /*Read Sac Char. Field*/
npts=ihd[9];
dt=fhd[0];
fprintf(stderr,"npts=%d   dt=%f\n",npts,dt);

tr=(float *)malloc(sizeof(float)*npts);
read(fd1,tr,npts*sizeof(float));

write(fd2,tr,npts*4); /*Write timeseries file*/

if(binflag != 1)      /*Make Helmberger File*/
  {
  fprintf(stderr,"Making Helmberger Ascii\n");
  sprintf(line,"mkHelm ntr=1 dt=%f nt=%d < %s > %s.helm",dt,npts,out,out);
  system(line);
  sprintf(line,"rm %s",out);
  system(line);
  }

}


