/*
This program computes the array response function in k-space for a given 
frequency.

Usage: arf freq kmax nk arrayname refsta dbname dir dfile

where
       freq = frequency at which array response is computed (Hz)
       kmax = maximum slowness in s/km
       nk = number of increments in slowness from -kmax to kmax
            (should be a odd integer to ensure slowness = 0 is (nk+1)/2 point)
       arrayname = name of array
       refsta = station which will be the reference point (x,y = 0,0)
       dbname = database name (the prefix used for the database)
       dir = directory in which dfile will be placed
       dfile = file name to hold computed array response

Notes: 

1) ".arr" is appended in program to arrayname to make file name for file
   containing array parameters.
2) The database (dbname) must already exist (at least the descriptor file).
3) The program creates a "filter" field for the fkgrid record on the basis of
   the center frequency (freq).  This field is part of the fkgrid primary key
   and will enable multiple array response maps to be indexed in one fkgrid 
   table even though the rest of the primary key, given by fields sta, chan, 
   and time:endtime, are identical.

*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 999 
#define rad2dg 57.2958
#define dg2km 111.1
#define pi 3.14159

int main(int argc, char *argv[])
{
  int      i,j,k,nsta,nk,noff,nwin,ireturn;
  long int ns;
  double   lat[MXSTA],lon[MXSTA],rx[MXSTA],ry[MXSTA];
  double   reflat,reflon,kx,ky,rsum,isum,freq,val,vmin,vmax,dummy;
  double   kmin,kmax,kinc,tstart,tend,slen,wlen;
  double   distance,azimuth,semin,semax,snmin,snmax;
  float    *valp;
  char     refsta[7],asta[7],searchExpr[80],afreq[17];
  char     *dir,*dfile,*fullpath,*arrayname,*arrayfile,*dbname,*filter;
  char     sta[MXSTA][7],chan[MXSTA][9];
  FILE     *fp,*fout;
  Dbptr    db,dba,dbs;

  arrayname = malloc(80);
  arrayfile = malloc(84);
  fullpath = malloc(98);
  dbname = malloc(80);
  dir = malloc(65);
  dfile = malloc(33);
  filter = malloc(31);

  if (argc < 9)
  {
    printf("Usage: %s freq kmax nk arrayname refsta dbname dir dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&freq);
  sscanf(argv[2],"%lf",&kmax);
  sscanf(argv[3],"%d",&nk);
  arrayname = argv[4];
  sscanf(argv[5],"%s",refsta);
  dbname = argv[6];
  dir = argv[7];
  dfile = argv[8];

/*If the output data file exists, the program will append to it.*/
/*If it does not exist, create it.*/
  memcpy(fullpath,"\0",1);
  strcpy(fullpath,dir);
  strcat(fullpath,"/");
  strcat(fullpath,dfile);
  printf("output file = %s\n",fullpath);
  if((fout=fopen(fullpath,"ab"))==NULL)
  {
    if((fout=fopen(fullpath,"wb"))==NULL)
    {
      fprintf(stderr,"Can't open %s\n",fullpath);
      return 1;
    }
  }

/*Open the array file and get array info.*/
  memcpy(arrayfile,"\0",1);
  strcpy(arrayfile,arrayname);
  strcat(arrayfile,".arr");
  if ( (fp = fopen(arrayfile,"r")) == NULL)
  {
    complain(0,"Could not open array file.\n");
    return 1;
  }

  nsta = 0;
  while(fscanf(fp, "%s %s %lf", sta[nsta],chan[nsta],&dummy) != EOF)
  {
    nsta = nsta + 1;
  }
  printf("# stations to be used for array response = %d\n",nsta);
  fclose(fp);
  printf("-2 nsta = %d\n", nsta);

  if (dbopen(dbname,"r+",&db) < 0)
  {
    complain(0,"Could not open database.\n");
    return 1;
  }
  printf("-1 nsta = %d\n", nsta);
  dbs  = dblookup(db,0,"site",0,0);
  printf("0 nsta = %d\n", nsta);
  dbquery(dbs,dbRECORD_COUNT,&ns);
  printf("1 nsta = %d\n", nsta);
  printf("# stations in database = %ld\n",ns);

/*Find reference station and put all station coordinates relative to it.*/
  if (ns == 0) 
  {
    printf("No site records found in database.\n");
    return 1;
  }
  for(dbs.record=0;dbs.record<ns;dbs.record++)
  {
    dbgetv(dbs,NULL,"sta",asta,"lat",&reflat,"lon",&reflon,NULL);
    if (strcmp(refsta,asta) == 0) break;
  }
  printf("2 nsta = %d\n", nsta);
  printf("refsta,reflat,reflon = %s %f %f\n",refsta,reflat,reflon);
/*Get coordinates for array stations relative to reference station.*/
  printf("3 nsta = %d\n", nsta);
  for (j=0;j<nsta;j++)
  {
    sprintf( searchExpr, "sta =~ /%s/",sta[j]);
    dba = dbsubset(dbs,searchExpr,NULL);
    dba.record = 0;
    dbgetv(dba,NULL,"lat",&lat[j],"lon",&lon[j],NULL);
/*  Note: dist returns distance in radians and azimuth in radians.*/
    dist(reflat/rad2dg,reflon/rad2dg,lat[j]/rad2dg,lon[j]/rad2dg,&distance,&azimuth);
    distance = distance*rad2dg*dg2km;
    rx[j]=distance*cos(pi/2 - azimuth);
    ry[j]=distance*sin(pi/2 - azimuth);
    printf("%d %s %7.2lf %7.2lf\n",j+1,sta[j],rx[j],ry[j]);
  }

  valp = malloc(nk*nk*sizeof(float));
  vmin =  1.0e10;
  vmax = -1.0e10;
  kmin = -kmax;
  kinc = (kmax - kmin)/(nk - 1); 

  for(j=0;j<nk;j++)
  {
    ky=kmin+(double)j*kinc;
    for(i=0;i<nk;i++)
    {
      kx=kmin+(double)i*kinc;
      rsum=0.0;
      isum=0.0;
      for(k=0;k<nsta;k++)
      {
        rsum+=cos(2.0*3.1415926*freq*(kx*rx[k]+ky*ry[k]));
        isum+=sin(2.0*3.1415926*freq*(kx*rx[k]+ky*ry[k]));
      }
      rsum/=(double)nsta;
      isum/=(double)nsta;
      val=sqrt(rsum*rsum+isum*isum);
      if(val<vmin) vmin=val;
      if(val>vmax) vmax=val;
/*    Need to only store float, not double.*/
      valp[j*nk+i] = val;
    }
  }
/*Write entire grid to file as one binary matrix.*/
  noff = ftell(fout);
  fwrite(valp,sizeof(float),(unsigned int)nk*nk,fout);
  printf("grid min,max = %f %f\n",vmin,vmax);

/*Define some variables for compatibility with regular slowness map output.*/
  memcpy(filter,"\0",1);
  strcat(filter,"BWZ ");
  sprintf(afreq,"%10.6f",freq);
  strcat(filter,afreq);
  strcat(filter," 0 ");
  strcat(filter,afreq);
  strcat(filter," 0");
  tstart = 0.0;
  tend = 0.0;
  slen = 0.0;
  wlen = 0.0;
  nwin = 1;
/*Write parameters for this slowness grid to fkgrid table.*/
  semin = kmin;
  semax = kmax;
  snmin = kmin;
  snmax = kmax;
  ireturn =  dbaddv(db,"fkgrid",
           "sta",arrayname,
           "refsta",refsta,
           "chan",chan[0],
           "time",tstart,
           "endtime",tend,
           "twin",slen,
           "dtime",wlen,
           "nt",nwin,
           "semin",semin,
           "semax",semax,
           "snmin",snmin,
           "snmax",snmax,
           "ne",nk,
           "nn",nk,
           "filter",filter,
           "datatype","u4",
           "foff",noff,
           "dir",dir,
           "dfile",dfile,
           NULL);
  if (ireturn == dbINVALID)
  {
    complain(0,"Could not add record to fkgrid file.\n");
    dbclose(db);
    fclose(fout);
    return 1;
  }
/*Force the record to be written in case of duplicate key error.*/
  if (ireturn < dbINVALID)
  {
    db = dblookup(db,0,"fkgrid",0,0);
    db.record = dbaddnull(db);
    ireturn =  dbputv(db,"fkgrid",
           "sta",arrayname,
           "refsta",refsta,
           "chan",chan[0],
           "time",tstart,
           "endtime",tend,
           "twin",slen,
           "dtime",wlen,
           "nt",nwin,
           "semin",semin,
           "semax",semax,
           "snmin",snmin,
           "snmax",snmax,
           "ne",nk,
           "nn",nk,
           "filter",filter,
           "datatype","u4",
           "foff",noff,
           "dir",dir,
           "dfile",dfile,
           NULL);
  }
  return 0;
}
