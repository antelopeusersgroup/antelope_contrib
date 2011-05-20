/*
This program computes the array response function in k-space for a given 
frequency.

Usage: arf freq kmax nk arrayname refsta dbname dfile

where
       freq = frequency at which array response is computed (Hz)
       kmax = maximum slowness in s/km
       nk = number of increments in slowness from -kmax to kmax
            (should be a odd integer to ensure slowness = 0 is (nk+1)/2 point)
       arrayname = name of array
       refsta = station which will be the reference point (x,y = 0,0)
       dbname = database name (the prefix used for the database)
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

#define MXSTA 500
#define rad2dg 57.2958
#define dg2km 111.1
#define pi 3.14159

int main(int argc, char *argv[])
{
  int    i,j,k,nsta,ns,nk,noff,nwin,ireturn;
  double lat[MXSTA],lon[MXSTA],rx[MXSTA],ry[MXSTA];
  double reflat,reflon,kx,ky,rsum,isum,freq,val,vmin,vmax,dummy,factor;
  double kmin,kmax,kinc,tstart,tend,slen,wlen;
  double distance,azimuth,semin,semax,snmin,snmax;
  float  *valp;
  char   refsta[7],asta[7],searchExpr[80],afreq[10];
  char   *dfile,*arrayname,*arrayfile,*dbname,*filter;
  char   sta[MXSTA][7],chan[MXSTA][9];
  FILE   *fp,*fout;
  Dbptr  db,dba,dbs;

  arrayname = malloc(9);
  arrayfile = malloc(13);
  dbname = malloc(80);
  dfile = malloc(80);
  filter = malloc(31);

  if (argc < 8)
  {
    printf("Usage: %s freq kmax nk arrayname refsta dbname dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&freq);
  sscanf(argv[2],"%lf",&kmax);
  sscanf(argv[3],"%d",&nk);
  arrayname = argv[4];
  sscanf(argv[5],"%s",refsta);
  dbname = argv[6];
  dfile = argv[7];

/*If the output data file exists, the program will append to it.*/
/*If it does not exist, create it.*/
  if((fout=fopen(dfile,"ab"))==NULL)
  {
    if((fout=fopen(dfile,"wb"))==NULL)
    {
      fprintf(stderr,"Can't open %s\n",dfile);
      return 1;
    }
  }

/*Open the array file and get array info.*/
  memcpy(arrayfile,"\0",1);
  strcpy(arrayfile,arrayname);
  strcat(arrayfile,".arr");
  if ( (fp = fopen(arrayfile,"r")) == NULL)
  {
    elog_complain(0,"Could not open array file.\n");
    return 1;
  }

  nsta = 0;
  while(fscanf(fp, "%s %s %lf", sta[nsta],chan[nsta],&dummy) != EOF)
  {
    nsta = nsta + 1;
  }
  printf("# stations to be used for array response = %d\n",nsta);
  fclose(fp);

  if (dbopen(dbname,"r+",&db) < 0)
  {
    elog_complain(0,"Could not open database.\n");
    return 1;
  }

  dbs  = dblookup(db,0,"site",0,0);

/*Find reference station and put all station coordinates relative to it.*/
  dbquery(dbs,dbRECORD_COUNT,&ns);
  for(dbs.record=0;dbs.record<ns;dbs.record++)
  {
    dbgetv(dbs,NULL,"sta",asta,"lat",&reflat,"lon",&reflon,NULL);
    if (strcmp(refsta,asta) == 0) break;
  }
  printf("refsta,reflat,reflon = %s %f %f\n",refsta,reflat,reflon);
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

  if((fp=fopen(dfile,"w"))==NULL)
  {
    fprintf(stderr,"Can't open %s\n",dfile);
    return 1;
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
      val=20.0*log10(val);
      if(val<vmin) vmin=val;
      if(val>vmax) vmax=val;
/*    Need to only store float, not double.*/
      valp[j*nk+i] = val;
    }
  }
/*Write entire grid to file as one binary matrix.*/
  noff = ftell(fout);
  fwrite(valp,sizeof(float),nk*nk,fout);
  printf("grid min,max = %f %f\n",vmin,vmax);

/*Define some variables for compatibility with regular slowness map output.*/
  memcpy(filter,"\0",1);
  strcat(filter,"BW ");
  sprintf(afreq,"%7.3f",freq);
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
           "datatype","t4",
           "foff",noff,
           "dir",".",
           "dfile",dfile,
           0);
  if (ireturn == dbINVALID)
  {
    elog_complain(0,"Could not add record to fkgrid file.\n");
    dbclose(db);
    fclose(fout);
    return 1;
  }
/*Force the record to be written in case of duplicate key error.*/
  if (ireturn < dbINVALID)
  {
    db = dblookup(db,0,"fkgrid",NULL,NULL);
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
           "datatype","t4",
           "foff",noff,
           "dir",".",
           "dfile",dfile,
           0);
  }
  return 0;
}
