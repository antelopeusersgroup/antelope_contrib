/* 
This program makes an grid of values in frequency-vs-wavenumber (f-k) space
as an array response to a plane wave of given velocity and azimuth crossing 
the array.  
It is meant to assess the beaming sidelobes of a linear array (or a spatial
array transformed to linear in response to a plane propagating wave from a 
given source external to the array.  Two types of arrays are possible:

1) a linear array with uniform spacing
2) an arbitrary spatial array (could be all on a line with arbitrary spacing)

In the case of a truly linear array, the inter-station spacing is required to be 
uniform.  The spatial array can be uniform or non-uniform, in which case the actual
stations are projected to the back-azimuth line to get the inter-station distances.
The output grid is arranged such that wavenumber is the horizontal axis and 
frequency is the vertical axis.  This program is meant for a quick and easy look at
f-k response to signals of a given velocity. 

Usage: kfresp sr kmax nk fmax nf kneg nl vel baz spacing channame arrayname refsta dbname dir dfile

where

  sr   = desired sampling rate (<= actual sampling rate).  This increases 
         performance when looking at low-frequency signals relative to original
         sampling rate.
  kmax = maximum slowness (1/km)  
  nk   = # of slownesses from -kmax to kmax.  This defines # columns in map.
         Note that, if kneg = 0 below, then the range is 0 to kmax.
  fmax = maximum frequency (Hz)  
  nf   = # of frequencies from 0.0 to fmax.  This defines # rows in map.
  kneg = flag to compute negative k part of grid or not 
         (1 = compute, 0 = do not)
  nl   = number of stations in linear array (set = 0 for real array)
  vel  = velocity of assumed wave (km/s)
  baz  = assumed back azimuth of wave (degrees clockwise from north)
         setting nl > 0 causes a linear array, so baz is ignored
  spacing = for nl > 0, the inter-station distance in km 
         if nl = 0 was entered, this is ignored
  channame = channel name to be assigned to F-K grid  (maximum of 8 
             characters). arrayname and channame should be a unique pair of 
             grid identifiers
  arrayname = name of array
              not used if nl > 0 -- enter a dummy name
  refsta = station code of reference station in array (must match one record
           in array file; not used if nl > 0, so enter a dummy name)
  dbname = database name (the prefix used for the database)
  dir = directory to hold output dfile
  dfile = file name to hold computed power in wavenumber-frequency space

Notes: 

1) ".arr" is appended in program to arrayname to make file name for file
   containing array parameters. This file consists of rows (sta,chan,delay)
   where "delay" is some known delay at the station (usually 0.0) and  
   "distkm" is the distance (km) along the array.  Normally, the first 
   station is at 0.0, but this may be a finite number, as in the case of 
   Green's functions at different distances.
2) The database (dbname) must already exist (at least the descriptor file).
*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 100 
#define MXPTS 36000 /*enough for 1 hour at 10 sps*/
#define pi    3.1416
#define twopi 6.2832
#define rd2dg 57.296
#define dg2km 111.2

int     do_grid(int,double,
        double,double,double,int,
        double,double,double,int,
        int,double*,double*);
int     make_beam_simple(int,double,double,double,float*);

int     npts;
double  vel,delay[MXSTA],distkm[MXSTA],azimdg[MXSTA];
float   *s[MXSTA*MXPTS];
FILE    *fout;

int main(int argc, char *argv[])
{
  int      nsta,whiten,ireturn,i,j,nk,nf,kneg,nl,ns;
  long int nwin,nkd,nfd;
  double   kmin,kmax,kinc,slen,sr,del,baz,spacing,distance,azimuth;
  double   reflat,reflon,slat[MXSTA],slon[MXSTA],rx[MXSTA],ry[MXSTA];
  double   fmin,fmax,finc,zero,minamp,maxamp,ppower;
  double   tstart,tend;
  char     channame[9],searchExpr[200];
  char     asta[7],refsta[7],sta[MXSTA][7],chan[MXSTA][9];
  char     *arrayfile,*arrayname,*dbname,*dfile,*dir,*fullpath;
  Dbptr    db,dbs,dba;
  FILE     *fp;
 
  arrayname = malloc(80);
  arrayfile = malloc(84);
  fullpath  = malloc(98);
  dbname    = malloc(80);
  dfile     = malloc(33);
  dir       = malloc(65);

  if (argc < 17) 
  {
    printf("Usage: %s sr kmax nk fmax nf kneg nl vel baz spacing channame arrayname refsta dbname dir dfile\n",argv[0]);
    return 1;
  }

  printf("Program kfresp\n");
  sscanf(argv[1],"%lf",&sr);
  sscanf(argv[2],"%lf",&kmax);
  sscanf(argv[3],"%d",&nk);
  sscanf(argv[4],"%lf",&fmax);
  sscanf(argv[5],"%d",&nf);
  sscanf(argv[6],"%d",&kneg);
  sscanf(argv[7],"%d",&nl);
  sscanf(argv[8],"%lf",&vel);
  sscanf(argv[9],"%lf",&baz);
  sscanf(argv[10],"%lf",&spacing);
  sscanf(argv[11],"%s",channame);
  arrayname = argv[12]; 
  sscanf(argv[13],"%s",refsta);
  dbname = argv[14];
  dir    = argv[15];
  dfile  = argv[16];
  printf("dbname,dir,dfile = %s %s %s\n",dbname,dir,dfile);

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

/*Open the station database and get required info on the array.*/
  printf("database = %s\n",dbname);
  if (dbopen(dbname,"r+",&db) < 0)
  {
    complain(0,"Could not open database for array.\n");
    return 1;
  }

  if (nl > 0) 
  {
    nsta = nl;
    for (j=0;j<nsta;j++) 
    {
      strcpy(sta[j],refsta);
      distkm[j] = j*spacing;
      azimdg[j] = 0.0;
    }
  }
  else
  {
/*  Open the array file and get array info.*/
    memcpy(arrayfile,"\0",1);
    strcpy(arrayfile,arrayname);
    strcat(arrayfile,".arr");
    if ( (fp = fopen(arrayfile,"r")) == NULL)
    {
      complain(0,"Could not open array file.\n");
      return 1;
    }
    nsta = 0;
    while(fscanf(fp, "%s %s %lf", sta[nsta],chan[nsta],&delay[nsta]) != EOF)
    {
      nsta = nsta + 1;
    }
    printf("# stations to be used for plot = %d\n",nsta);
    fclose(fp);
    dbs  = dblookup(db,0,"site",0,0);
/*  Find reference station and put all station coordinates relative to it.*/
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
      dbgetv(dba,NULL,"lat",&slat[j],"lon",&slon[j],NULL);
/*    Note: dist returns distance in radians and azimuth in radians.*/
      dist(reflat/rd2dg,reflon/rd2dg,slat[j]/rd2dg,slon[j]/rd2dg,&distance,&azimuth);
      distance = distance*rd2dg*dg2km;
      rx[j]=distance*cos(pi/2 - azimuth);
      ry[j]=distance*sin(pi/2 - azimuth);
/*    Compute the projected distance along the back-azimuth line.*/
      distkm[j] = -(distance*cos(azimuth-baz/rd2dg)); 
      azimdg[j] = azimuth*rd2dg;
      dbfree(dba);
    }
  }
  for (j=0;j<nsta;j++) printf("%d %s %6.2lf %6.1lf\n",j+1,sta[j],distkm[j],azimdg[j]);

/*Define the F-K grid parameters fully.*/ 
  if (kneg) 
    kmin = -kmax;
  else
    kmin = 0.0;
  kinc = (kmax - kmin)/(nk - 1);
  fmin = 0.0;
  finc = fmax/(nf - 1);
  printf("kmin,kmax,kinc,fmin,fmax,finc = %f %f %f %f %f %f\n",
  kmin,kmax,kinc,fmin,fmax,finc);

/*Initialize the signals.*/
  slen = 1/finc;
  npts = slen*sr + 1;
  printf("slen,npts = %f %d\n",slen,npts);
  for (j=0;j<nsta;j++)
  {
    s[j]  = malloc(npts*sizeof(float));
    for (i=0;i<npts;i++) s[j][i] = 0.0;
  }

/*do_grid will compute and write the grid.*/
  whiten = 0;
  del = 1/sr;
  if (do_grid(nsta,del,kmin,kinc,kmax,nk,fmin,finc,fmax,nf,whiten,&minamp,&maxamp) == 1)
  {
    printf("Problem in computing grid.\n");
    return 1;
  }

/*Write parameters for this K-F grid to fkgrid table.*/
  tstart = 0.0;
  tend = 0.0;
  zero  = 0.0;
  nwin = 1;
  ppower = log10(maxamp) + 10.;   /* Make sure power is positive.*/
  printf("sta = %s\n", arrayname);
  printf("chan = %s\n", channame);
  printf("ppower = %f\n", ppower);
  nkd = nk;
  nfd = nf;
  ireturn = dbaddv(db,"fkgrid",
         "sta",arrayname,
         "chan",channame,
         "time",tstart,
         "endtime",tend,
         "twin",slen,
         "filter","BWZ 0.0 0 0.0 0",
         "dtime",zero,
         "nt",nwin,
         "ppower",ppower,
         "semin",kmin,
         "semax",kmax,
         "snmin",fmin,
         "snmax",fmax,
         "ne",nkd,
         "nn",nfd,
         "datatype","u4",
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
    ireturn = dbputv(db,"fkgrid",
         "sta",arrayname,
         "chan",channame,
         "time",tstart,
         "endtime",tend,
         "twin",slen,
         "filter","BWZ 0.0 0 0.0 0",
         "dtime",zero,
         "nt",nwin,
         "ppower",ppower,
         "semin",kmin,
         "semax",kmax,
         "snmin",fmin,
         "snmax",fmax,
         "ne",nkd,
         "nn",nfd,
         "datatype","u4",
         "dir",dir,
         "dfile",dfile, 
         NULL);
  }

  fclose(fout);
  dbclose(db);

/*End main routine.*/
}


int do_grid(int nsta,double del,
            double klow,double kinc,double khigh,int nk,
            double flow,double finc,double fhigh,int nf,
            int whiten, double *minamp, double *maxamp)
{
  int    i,j,l;
  double val,k,f,mina,maxa;
  float  *sbeam,*valp,amax;

  sbeam  = malloc(npts*sizeof(float));
  valp   = malloc(nk*nf*sizeof(float));

  printf("nk,nf = %d %d\n",nk,nf);
  mina= 1.0e20;
  maxa= 0.0;

  for (j=0;j<nf;j++)       
/*j is increasing row index over frequency*/
/*grid is laid out in one long vector, row by row*/
  {
    f = flow + j*finc;
    for (i=0;i<nk;i++)     /*i is column index over wavenumber*/
    {
      k = klow + i*kinc;
      make_beam_simple(nsta,del,k,f,sbeam);
      val = 0.0;
      for (l=0;l<npts;l++) if (fabs(sbeam[l]) > val) val = fabs(sbeam[l]);
      if (val > maxa) maxa = val;
      if (val < mina) mina = val;
/*    Need to only store float, not double.*/
      valp[j*nk+i] = val;
    }
    if (whiten)
/*  Normalize the row values to have a maximum of 1.*/
    {
      amax = 0.0;
      for (i=0;i<nk;i++) if (valp[j*nk+i] > amax) amax = valp[j*nk+i];
      if (amax > 0.0) for (i=0;i<nk;i++) valp[j*nk+i] = valp[j*nk+i]/amax;
    }
  }
/*Write entire grid to file as one binary matrix.*/
  fwrite(valp,sizeof(float),(unsigned int)nk*nf,fout);
  printf("wrote grid: minamp,maxamp = %f %f\n",mina,maxa);
  free(sbeam);
  free(valp);
  *minamp=mina;
  *maxamp=maxa;

  return 0;
}


int make_beam_simple(int nbeam,double del,double k,double f,float *sout)
/* This is an adaptation of make_beam in kfgrid to handle the simple case
   where we beam sine waves of varying phase.*/
{
  int    i,j;
  double t,sint,cost,sumsin,sumcos,term;

  if (k <  0.0)
    term = f/vel + k;
  else
    term = f/vel - k;

  for (i=0;i<npts;i++) 
  {
    t = i*del;
    sint = sin(twopi*f*t);
    cost = cos(twopi*f*t);
    sumsin = 0.0;
    sumcos = 0.0;
    for (j=0;j<nbeam;j++)
    {
      sumsin = sumsin + sin(twopi*term*distkm[j]);
      sumcos = sumcos + cos(twopi*term*distkm[j]);   
    }
    sout[i] = (sint*sumcos + cost*sumsin)/nbeam;
  } 

  return 0;
}
