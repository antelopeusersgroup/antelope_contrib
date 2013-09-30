/* 
This program, a modification of kfgrid, makes the grid in phase velocity versus
period space (t-c) instead.  Only positive phase velocity is analyzed.  This 
map is specific to dispersion analysis in seismic wave propagation in the earth.
The input array is assumed to be linear.  The program uses Antelope databases.
The spatial sampling can be uniform or non-uniform.  It is meant to analyze
dispersive signals across linear arrays.  The output grid is arranged such 
that period is the horizontal axis and phase velocity is the vertical axis. 
Thus, constant v is represented by a horizontal line in the plot.  This program
is meant for a quick and easy look at t-c response and uses inefficient time-
domain algorithms wherein the traces are shifted and summed for each t-c point.
The absolute maximum of the resulting beam then becomes a value in the t-c grid.

Usage: tcgrid tstart slen sr tmax nt cmax nc channame arrayname whiten dbname dir dfile

where

  tstart = start time of signal in epoch seconds
  slen = signal length to be analyzed in seconds
  sr = desired sampling rate (<= actual sampling rate).  This increases 
       performance when looking at low-frequency signals relative to original
       sampling rate.
  tmax = maximum period (seconds)  
  nt = # of periods from 0 to tmax.  This defines # columns in map.
  cmax = maximum phase velocity (km/s)  
  nc = # of phase velocities from 0.0 to cmax.  This defines # rows in map.
  channame = channel name to be assigned to T-C grid  (maximum of 8 
             characters). arrayname and channame should be a unique pair of 
             grid identifiers
  arrayname = name of array
  whiten = flag to whiten the spectrum across period (inverse frequency)
           0 = no whitening
           1 = whiten such that peak over phase velocity for each period = 1.0
  dbname = database name (the prefix used for the database)
  dir = directory to hold output dfile
  dfile = file name to hold computed power in T-C space

Notes: 

1) ".arr" is appended in program to arrayname to make file name for file
   containing array parameters. This file consists of rows (sta,chan,delay,
   distkm) where "delay" is some known delay at the station (usually 0.0) and  
   "distkm" is the distance (km) along the array.  Normally, the first 
   station is at 0.0, but this may be a finite number, as in the case of 
   Green's functions at different distances.
2) The database (dbname) must already exist (at least the descriptor file).
3) Nyquist frequency is determined from argument "sr".  The number of 
   frequencies is simply slen*sr/2, where slen' is slen rounded up to next
   higher power of 2.  This number should be ideally less than or equal to 
   1024, which is about the pixel width of many display monitors.
*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 100 
#define MXPTS 36000 /*enough for 1 hour at 10 sps*/

int     do_grid(int,double,
        double,double,double,int,
        double,double,double,int,
        int,double*,double*);
int     make_beam(int,double,double,double,float*);
double  get_shift(double,double,double,double);
double  sign(double);
int     get_stat(float*,int,double*,double*);
int     trgetwf(Dbptr, Arr **,float **,int *,double,double,double *,double *,
        int *,int *,void *);
int     trfilter_segs(int , int *, double *, float **, char *);

int     nsta,npts;
double  delay[MXSTA],distkm[MXSTA];
float   *s[MXSTA*MXPTS],*sf[MXSTA*MXPTS];
FILE    *fout;

int main(int argc, char *argv[])
{
  int      whiten,ireturn,nbytes,i,j,nt,nc;
  long int nw,nz;
  int      iret,idec,nwin,nbeam;
  int      mpts[MXSTA];
  double   tmin,tmax,tinc,rdec,slen,sr,sro,del,delo;
  double   tstart,tend,ts[MXSTA],te[MXSTA];
  double   mean,stdv,calib,cmin,cmax,cinc,fnyq,zero,minamp,maxamp,ppower;
  char     channame[9],searchExpr[200],corner[7];
  char     sta[MXSTA][7],chan[MXSTA][9];
  char     *arrayfile,*arrayname,*dbname,*dfile,*dir,*decfilter,*fullpath;
  Dbptr    db,dbw,dbz;
  FILE     *fp;
 
  arrayname = malloc(80);
  arrayfile = malloc(84);
  fullpath  = malloc(98);
  dbname    = malloc(80);
  dfile     = malloc(33);
  dir       = malloc(65);
  decfilter = malloc(40);

  if (argc < 14) 
  {
    printf("Usage: %s tstart slen sr tmax nt cmax nc channame arrayname whiten dbname dir dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&tstart);
  sscanf(argv[2],"%lf",&slen);
  sscanf(argv[3],"%lf",&sr); 
  sscanf(argv[4],"%lf",&tmax); 
  sscanf(argv[5],"%d",&nt); 
  sscanf(argv[6],"%lf",&cmax); 
  sscanf(argv[7],"%d",&nc); 
  sscanf(argv[8],"%s",channame);
  arrayname = argv[9]; 
  printf("arrayname = %s\n",arrayname);
  sscanf(argv[10],"%d",&whiten);
  dbname = argv[11];
  dir    = argv[12];
  dfile  = argv[13];
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
  while(fscanf(fp, "%s %s %lf %lf", sta[nsta],chan[nsta],&delay[nsta],&distkm[nsta]) != EOF)
  {
    nsta = nsta + 1;
  }
  printf("# stations to be used for plot = %d\n",nsta);
  fclose(fp);

/*Open the database and get required info on the array.*/

  printf("input database = %s\n",dbname);
  if (dbopen(dbname,"r+",&db) < 0)
  {
    complain(0,"Could not open database.\n");
    return 1;
  }

  dbw  = dblookup(db,0,"wfdisc",0,0);
  dbquery(dbw,dbRECORD_COUNT,&nw);
  printf("nw = %ld\n",nw);

/*Get the waveforms.*/
  tend = tstart + slen;
  printf("tstart,tend = %f %f\n",tstart,tend);

  nbeam = 0;
  for (j=0;j<nsta;j++)
  {
/*  Not all stations in the array may have a recording.  Before starting, just 
    fill the waveforms with zeroes.  If any waveforms do not have full length 
    when returned from trgetwf, they will be filled in with zeroes also.*/
    s[j]  = malloc(MXPTS*sizeof(float));
    sf[j] = malloc(MXPTS*sizeof(float));
    for (i=0;i<MXPTS;i++) s[j][i] = 0.0;
    sprintf( searchExpr, "sta =~ /%s/ && chan =~ /%s/ && time <= %lf && endtime >= %lf",sta[j],chan[j],tstart,tend);
    dbz = dbsubset(dbw,searchExpr,NULL);
    dbquery(dbz,dbRECORD_COUNT,&nz);
    if (nz == 0) 
    { 
      complain(0,"No wfdisc records found for station %s.\n",sta[j]);
      continue;
    }
    dbz.record = 0;
    dbgetv(dbz,NULL,"calib",&calib,"samprate",&sro,NULL);
    if (calib == 0.0) calib = 1.0;
    nbytes =  MXPTS*sizeof(float);
/*  Note: trgetwf does NOT apply calibration factor, so we will.*/
    iret=trgetwf(dbz,NULL,&s[j],&nbytes,tstart,tend,&ts[j],&te[j],&mpts[j],0,0);
    if (iret != 0)
    {
      complain(0,"error in getting data for station %s\n",sta[j]);
      continue;
    }
    for (i=0;i<mpts[j];i++) s[j][i] = s[j][i]*calib;
    nbeam = nbeam + 1;
/*  printf("%2d %4s %15.3f %15.3f %5d\n",j,sta[j],ts[j],te[j],mpts[j]);*/
  }
  printf("retrieved waveforms\n");
  printf("# stations having waveforms to beam = %d\n",nbeam);
  if (nbeam == 0) 
  {
    complain(0,"No waveforms available for this time -- cannot continue.\n");
    return 1;
  }

/*Now do resampling, if needed.  Find the original sampling rate first.  The
  resampling is done in place.*/

  del = 1/sr;
  fnyq = sr/2;
  for(j=0;j<nsta;j++)
  {
    npts = (tend - tstart)*sro + 1;
    delo = 1/sro;
    rdec = (sro/sr);
    if (rdec - floor(rdec) > 0.5) 
     idec = ceil(rdec);
    else
      idec = floor(rdec);
/*  printf("delo,del,rdec,idec = %f %f %f %d\n",delo,del,rdec,idec);*/
/*  Check old and new sample rates.*/
    if (fabs(rdec - idec) > 0.01) 
    {
      complain(0,"Old sample rate over new sample rate != integer -- cannot continue.\n");
      complain(0,"Old sample rate = %f; new sample rate = %f\n",sro,sr);
      return 1;
    }
    if (idec == 0) 
    {
      complain(0,"Old sample rate over new sample rate < 1 -- cannot resample.\n");
      complain(0,"Old sample rate = %f; new sample rate = %f\n",sro,sr);
      return 1;
    }        

/*  Check if new rate requires decimation; if so, do it.*/
    if (idec > 1) 
    {
/*    Filter first to prevent aliasing.*/
      strcpy(decfilter,"\0");
      strcat(decfilter,"BWZ 0.0 0 ");
      sprintf(corner,"%6.2f",0.8*sr/2);
      strcat(decfilter,corner);
      strcat(decfilter," 4");
      iret = trfilter_segs(1,&mpts[j],&delo,&s[j],decfilter);
      if (iret != 0)
      {
        complain(0,"error in filtering waveform %d = %d\n",j,iret);
        return 1;
      }
      npts = (npts-1)/idec + 1;  /*new number of points*/
      for(i=0;i<npts;i++) s[j][i] = s[j][i*idec];
    }
  }
/*Last npts will be that used from now on; all waveforms should be same length.*/
  printf("filtered waveforms\n");
  printf("# points in signal = %d\n",npts);

/*Remove mean of traces and divide by stdv to equalize them.*/
  for(j=0;j<nsta;j++)
  { 
    iret = get_stat(s[j],npts,&mean,&stdv);
/*  printf("j,trace mean,stdv = %d %f %f\n",j,mean,stdv);*/
    for(i=0;i<npts;i++) s[j][i] = (s[j][i] - mean)/stdv;
  }

/*Define the T-C grid parameters fully.*/ 
  tmin = 0.0;
  tinc = tmax/(nt - 1);
  cmin = 0.0;
  cinc = cmax/(nc - 1);

/*do_grid will compute and write the grid.*/
  if (do_grid(nbeam,del,tmin,tinc,tmax,nt,cmin,cinc,cmax,nc,whiten,&minamp,&maxamp) == 1)
  {
    fprintf(stderr,"Problem in computing grid.\n");
    return 1;
  }

/*Write parameters for this T-C grid to fkgrid table.*/
  zero  = 0.0;
  nwin = 1;
  ppower = log10(maxamp) + 10.;   /* Make sure power is positive.*/
  printf("sta = %s\n", arrayname);
  printf("chan = %s\n", channame);
  printf("ppower = %f\n", ppower);
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
         "semin",tmin,
         "semax",tmax,
         "snmin",cmin,
         "snmax",cmax,
         "ne",nt,
         "nn",nc,
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
         "semin",tmin,
         "semax",tmax,
         "snmin",cmin,
         "snmax",cmax,
         "ne",nt,
         "nn",nc,
         "datatype","u4",
         "dir",dir,
         "dfile",dfile, 
         NULL);
  }

  fclose(fout);
  dbclose(db);

/*End main routine.*/
}

int do_grid(int nbeam,double del,
            double tlow,double tinc,double thigh,int nt,
            double clow,double cinc,double chigh,int nc,
            int whiten, double *minamp, double *maxamp)
{
  int    i,j,l,iret;
  double val,c,t,mina,maxa,sr,fnyq;
  float  *sbeam,*valp,amax;
  float  freq1,freq2;
  char   *filter;
  char   afreq[9];

  sbeam  = malloc(npts*sizeof(float));
  valp   = malloc(nt*nc*sizeof(float));
  filter = malloc(40);

  sr = 1/del;
  fnyq = sr/2;
  mina= 1.0e20;
  maxa=-1.0e20;

  for (i=1;i<nt;i++)       
/*i is increasing column index over period*/
/*j is increasing row index over velocity*/
/*grid is laid out in one long vector, row by row, starting at bottom*/
  {
    t = tlow + i*tinc;
    /*Filter the traces in a narrow-band with phaseless filter.*/
    freq1 = 1/((i+1)*tinc);
    if (i == 1)
      freq2 = 2*freq1;
    else
      freq2 = 1/((i-1)*tinc);
    if (freq2 > fnyq) freq2 = fnyq;
    strcpy(filter,"\0");
    strcpy(afreq,"\0");
    strcat(filter,"BWZ ");
    if (freq1 <= 0.0)
    {
      strcat(filter,"0.0");
      strcat(filter," 0 ");
    }
    else
    {
      sprintf(afreq,"%8.4f",freq1);
      strcat(filter,afreq);
      strcat(filter," 4 ");
    }
    sprintf(afreq,"%8.4f",freq2);
    strcat(filter,afreq);
    strcat(filter," 4");
/*  printf("t,f = %f %f; filter = %s\n",t,1/t,filter);*/
    for (l=0;l<nsta;l++) 
    {
      memcpy(sf[l],s[l],npts*4);
      iret = trfilter_segs(1,&npts,&del,&sf[l],filter); 
    }
    for (j=1;j<nc;j++)
    {
      c = clow + j*cinc;
      make_beam(nbeam,del,t,c,sbeam);
      val = 0.0;
      for (l=0;l<npts;l++) if (fabs(sbeam[l]) > val) val = fabs(sbeam[l]);
      if (val > maxa) maxa = val;
      if (val < mina) mina = val;
/*    Need to only store float, not double.*/
      valp[j*nt+i] = val;
    }
  }
/*Make the first column (zero period) same as second.*/
  for (j=1;j<nc;j++) valp[j*nt] = valp[j*nt+1];
/*Make the first row (zero velocity) same as second.*/
  for (i=0;i<nt;i++) valp[i] = valp[nt+i];

  printf("finished grid\n");

  if (whiten > 0)
  {
/*  Normalize the column values to have a maximum of 1.*/
    for (i=0;i<nt;i++)
    {
      amax = 0.0;
      for (j=0;j<nc;j++) if (valp[j*nt+i] > amax) amax = valp[j*nt+i];
      if (amax > 0.0) for (j=0;j<nc;j++) valp[j*nt+i] = valp[j*nt+i]/amax;
    }
  }

/*Write entire grid to file as one binary matrix.*/
  fwrite(valp,sizeof(float),(unsigned int)nt*nc,fout);
  printf("wrote grid: minamp,maxamp = %f %f\n",mina,maxa);
  free(sbeam);
  free(valp);
  free(filter);
  *minamp=mina;
  *maxamp=maxa;

  return 0;
}

/*
* Input t in seconds and c in km/s; output will be time shift in seconds.
* If, for instance, c is positive, meaning signal is propagating to increasing
* positive distance, then for dist positive the signal is moved back in time
* at a station with dist, relative to that at recorded at dist = 0.
* Therefore, a negative c means advance the signal, or shift it backward in 
* time.  The "delay" is an empirical (or estimated) delay at each station due
* to inhomogeneity (it may equal zero).  It is defined such that a positive
* delay means the signal is later than expected, relative to the plane wave; 
* and so tshift is adjusted downward.  Positive delay means the signal arrives 
* after it is expected; negative delay means it arrives before.
*/
double get_shift(double t, double c, double dist, double delay)
/* Note: t not used, but kept as holdover from get_shift in kfgrid.c*/
{
  double tshift;

  tshift = -(1/c)*dist;         /*uniform propagation shift*/
  tshift = tshift - delay;      /*inhomogeneity term*/

  return tshift;
}


double sign(double x)
{
  if(x>=0)
      return 1.0;
  else 
      return -1.0;
}


int make_beam(int nbeam,double del,double t,double c,float *sout)
{
  int i,j,sind;
  double slen,tshift,dx,fraction;

  slen = (npts-1)*del;

  for (i=0;i<npts;i++) sout[i] = 0.0;

  for (j=0;j<nsta;j++)
  {
    if (c == 0.0)
      tshift = 0.0;
    else
      tshift = get_shift(t,c,distkm[j],delay[j]);
/*  Don't use when shift is larger than trace length.*/
    if (fabs(tshift) > slen) continue;
    sind = (int)((-tshift)/del)+1;
    dx = tshift - del*(double)((int)(tshift/del));
    fraction = dx/del;

    if( sind < 0 )
    {
      for (i=-sind;i<npts;i++)
      {
        sout[i] += sf[j][sind+i] +
	fraction*(sf[j][sind+i+1] - sf[j][sind+i]); 
      }
    } 
    if( sind >= 0 )
    {
      for (i=0;i<(npts-sind-1);i++)
      {
        sout[i] += sf[j][sind+i] +
	fraction*(sf[j][sind+i+1] - sf[j][sind+i]); 
      }
    } 
  } 

  for (i=0;i<npts;i++) sout[i] /= nbeam;

  return 0;
}


int get_stat(float *dat,int ndat,double *mean,double *stdv)
{
  int i;
  double sum;

  sum=0.0;
  for(i=0;i<ndat;i++) sum+=dat[i];

  *mean=sum/(double)ndat;

  sum=0.0;
  for(i=0;i<ndat;i++) sum+=(*mean-dat[i])*(*mean-dat[i]);

  *stdv=sqrt(sum/(double)(ndat-1));

  return 0;
}
