/* 
This program enables one to make certain types of synthetic traces, aka the
SAC funcgen command, and put them in a CSS3.0 database (wfdisc).

Usage: funcgen type toff width nfactor sta chan time samprate nsamp database dir filename

where

  type = type of function to be generated
         "zeros"  means entire trace will be exact zeros
         "delta"  means a 1-point delta function will be inserted at the toff 
                  point of the trace
         "boxcar" means that a boxcar-shape will start at toff and have 
                  width seconds and be 0's elsewhere
         "triangle" means that a triangle-shape will start at toff and 
                  have width seconds and be 0's elsewhere
         "sine"   means a 1/width Hz sine wave will be created
         "impulse" means a f = 1/width Hz and h = 0.7 seismometer response pulse
                  will be generated and started at tstart
         "noise"  means a white-noise sequence drawn from a normal density with
                  zero mean and unit standard deviation will be created
  toff = offset (s), relative to time, at which synthetic wave starts
  width = width (s) of boxcar or triangle, or natural period in case of impulse
  nfactor = for adding random noise to any type (except "xeros" or "noise")
            This factor is the ratio of rms of additive noise to peak of signal.
  sta = station name 
  chan = channel name
  time = epoch time of start of trace
  samprate = sample rate (#/second) of trace
  nsamp = # of samples in trace
  database = database name for output data
  dir = directory in which to write trace
  dfile = filename of trace
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"

#define pi 3.14159
int stats(float x[], int npts, int k, float *mean, float *stdev, float *skew, float *kurtosis);

int main(int argc, char *argv[])
{
  unsigned int irand;
  int      isamp,wsamp,exists,iret,jdate,nsamp,wfid,itype,i,half;
  long int nw;
  double   samprate,time,endtime,toff,width;
  float    *s,*n,del,nfactor,mean,stdv,dummy,h;
  char     sta[7],chan[9],*database,*type;
  char     dir[65],dfile[33],outfile[100];
  Dbptr    db,dbw;
  FILE     *fp;
 
  database = malloc(80);
  type = malloc(20);

  if (argc < 13) 
  {
    printf("Usage: %s type toff width nfactor sta chan time samprate nsamp database dir filename\n",argv[0]);
    return 1;
  }

  type = argv[1];
  sscanf(argv[2],"%lf",&toff);
  sscanf(argv[3],"%lf",&width);
  sscanf(argv[4],"%f",&nfactor);
  strcpy(sta,argv[5]);
  strcpy(chan,argv[6]);
  sscanf(argv[7],"%lf",&time);
  sscanf(argv[8],"%lf",&samprate);
  sscanf(argv[9],"%d",&nsamp);
  database = argv[10];
  strcpy(dir,argv[11]);
  strcpy(dfile,argv[12]);
  s = malloc(nsamp*sizeof(float));
  n = malloc(nsamp*sizeof(float));

/*Open the output database.*/

  exists = 1;
  if ( (fp = fopen(database,"r")) == NULL)
    exists = 0;
  else
    fclose(fp);

/*It exists, so check if it can be opened and written to.*/

  if (exists)
  {
    if (dbopen(database,"r+",&db) < 0)
    {
      complain(0,"Could not open existing database for writing.\n");
      return 1;
    }
  }
  else
  {
    if (dbcreate(database,"css3.0",0,0,0) != 0)
    {
      complain(0,"Could not create new database.\n");
      return 1;
    }
    else
    {
      if (dbopen(database,"r+",&db) != 0)
      {
        complain(0,"Could not open new database for writing.\n");
        return 1;
      }
    }
  }

  dbw = dblookup(db,0,"wfdisc",0,0);
  iret = dbquery(dbw,dbRECORD_COUNT,&nw);
  printf("nw = %ld\n",nw);
  wfid = dbnextid(db,"wfid");

/*Create the synthetic trace.*/
  if (strcmp(type,"zeros"    ) == 0) itype = 1;
  if (strcmp(type,"delta"    ) == 0) itype = 2;
  if (strcmp(type,"boxcar"   ) == 0) itype = 3;
  if (strcmp(type,"triangle" ) == 0) itype = 4;
  if (strcmp(type,"sine"     ) == 0) itype = 5;
  if (strcmp(type,"impulse"  ) == 0) itype = 6;
  if (strcmp(type,"noise"    ) == 0) itype = 7;

/*Generate a zero-mean, unit rms, random noise series, whether used or not.*/
  irand = std_now()-1200000000;
  srand(irand);        /* sets the RNG with a random start integer*/
  for (i=0;i<nsamp;i++)
  {
/*    Use a trick to make sure some abnormal number is not generated.*/
      n[i] = 10.0;
      while (fabs(n[i]) > 9.9) n[i] = sqrt(-2.0*log(1.0 - 
        ((float) rand())/RAND_MAX))*cos(6.2832 * ((float) rand())/RAND_MAX);
  }
  del = 1/samprate;
  printf("itype = %d\n",itype);
  switch (itype) 
  {
    case 1:
      /*This creates a series of zeroes.*/
      for (i=0;i<nsamp;i++) s[i] = 0.0;
      break;
    case 2:
      /*This creates a delta function.*/
      for (i=0;i<nsamp;i++) s[i] = 0.0;
      isamp = toff*samprate;
      s[isamp] = 1.0;
      if (nfactor > 0)
      { 
        for (i=0;i<nsamp;i++) s[i] = s[i] + nfactor*n[i];
      }
      break;
    case 3:
      /*This creates a boxcar function.*/
      for (i=0;i<nsamp;i++) s[i] = 0.0;
      wsamp = width*samprate;
      /*Make it odd number of points, with center point splitting the boxcar.*/
      if (wsamp%2 == 0) wsamp = wsamp + 1;
      isamp = toff*samprate;
      for (i=isamp;i<isamp+wsamp;i++) s[i] = 1.0;
      if (nfactor > 0)
      { 
        for (i=0;i<nsamp;i++) s[i] = s[i] + nfactor*n[i];
      }
      break;
    case 4:
      /*This creates a triangle function.*/
      for (i=0;i<nsamp;i++) s[i] = 0.0;
      wsamp = width*samprate;
      /*Make it odd number of points, with peak at center point.*/
      if (wsamp%2 == 0) wsamp = wsamp + 1;
      half = wsamp/2;
      isamp = toff*samprate;
      for (i=isamp;i<isamp+half;i++) s[i] = 1.0*(i-isamp)/(float)half;
      s[isamp+half] = 1.0;
      for (i=isamp+half+1;i<isamp+wsamp;i++) s[i] = 1.0*(isamp+wsamp-i-1)/(float)half;
      if (nfactor > 0)
      { 
        for (i=0;i<nsamp;i++) s[i] = s[i] + nfactor*n[i];
      }
      break;
    case 5:
      /*This creates a 1/width Hz sine wave.*/ 
      for (i=0;i<nsamp;i++) s[i] = sin(2*pi*i*del/width);
      if (nfactor > 0)
      { 
        for (i=0;i<nsamp;i++) s[i] = s[i] + nfactor*n[i];
      }
      break;
    case 6:
      /*This creates an exponentially decaying sine wave, like a seismometer
        delta response (damping = 0.7).*/
      h = 0.7;
      for (i=0;i<nsamp;i++) s[i] = 0.0;
      isamp = toff*samprate;
      for (i=0;i<nsamp-isamp;i++) 
        s[i+isamp] = sin(2*pi*sqrt(1-h*h)*i*del/width)*exp(-0.7*2*pi*i*del/width);
      if (nfactor > 0)
      { 
        for (i=0;i<nsamp;i++) s[i] = s[i] + nfactor*n[i];
      }
      break;
    case 7:
      /*This creates a random white-noise series.*/
      for (i=0;i<nsamp;i++) s[i] = n[i];
      iret = stats(s,nsamp,2,&mean,&stdv,&dummy,&dummy);
      printf("mean,stdv = %f %f\n",mean,stdv);
      break;
    default:
      printf("Input 'type' does not match any options.\n");
      return 1;
  }

/*Write out the new trace.*/
  memcpy(outfile,"\0",1);
  strcat(outfile,dir);
  strcat(outfile,"/");
  strcat(outfile,dfile);
  printf("outfile = %s\n",outfile);
  if((fp=fopen(outfile,"w")) == NULL)
  {
    fprintf(stderr,"Can't open %s\n",outfile);
    return 1;
  }
  fwrite(s,sizeof(float),(unsigned int)nsamp,fp);
  fclose(fp);

/*Add entry to wfdisc. Note that, due to problem with dbaddv, we force the
  adding of the record with dbputv after a null record is created.*/
  dbw.record = dbaddnull(dbw);
  jdate = yearday(time);
  endtime = time + (nsamp - 1)/samprate;
  if (dbputv(dbw,0,
        "sta", sta,
        "chan", chan,
        "time", time,
        "endtime", endtime,
        "jdate", jdate,
        "nsamp", nsamp,
        "samprate", samprate,
        "datatype", "u4",
        "dir", dir,
        "dfile", dfile,
        "foff", 0,
        "wfid",wfid,
        NULL) < 0)
  {
      complain(0,"Could not add record to wfdisc file.\n");
      return 1;
  }

  dbclose(db);

} /*End main routine.*/
