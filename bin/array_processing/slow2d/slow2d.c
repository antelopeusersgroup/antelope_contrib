/* 
Extensive mods from original GAP version in November 2006 to allow this
program to work with Antelope databases.  All GMT output was eliminated in 
favor of generic output.  All SAC file handling was eliminated because the 
BRTT routines can take care of signal read/process/write functions.

Usage: slow2d tstart slen wlen filter sr kmax nk ampfunc beamtype arrayname refsta dbname dfile

where

  tstart = start time of signal in epoch seconds
  slen = signal length to be analyzed in seconds
  wlen = window length to analyze per slowness grid  (wlen <= slen; if equal,
         then only one window will be analyzed.  The window is moved forward
         with no overlap until tstart+slen is reached.)
  filter = filter specification in BRTT syntax, like "BW 0.5 4 8.0 4" (Must be
           put in quotes.)
  sr = desired sampling rate (<= actual sampling rate).  This increases 
       performance when looking at low-frequency signals relative to original
       sampling rate.
  kmax = maximum slowness (s/km)  (This will be used to define minimum
         slowness as kmin = -kmax and is applied to both x and y directions.)
  nk = # of slownesses from kmin to kmax.  (This should usually be an odd
       integer so that 0 Hz falls on (kmax - kmin)/(nk - 1)
  ampfunc = flag to select how amplitude is computed from beam
            1 = simple peak value (absolute)
            2 = average of absolute values in beam window (L1)
            3 = root-mean-square of values in beam window (L2)
  beamtype = type of beam to be formed in slowness space
            1 = linear
            2 = 2nd root
            3 = 3rd root
            4 = 4th root
            5 = incoherent
  channame = channel name to be assigned to slowness grid  (maximum of 8 
             characters). arrayname and channame should be a unique pair of 
             grid identifiers
  arrayname = name of array
  refsta = name of station to which beam will be referenced
  dbname = database name (the prefix used for the database)
  dfile = file name to hold computed power vs. kx,ky maps

Notes: 

1) ".arr" is appended in program to arrayname to make file name for file
   containing array parameters.  The station delay is used from the array
   file.  Ordinarily, one would set this to all zeros because it would be
   specific to a given wavenumber vector if it were not zeros.  This is a
   station delay, to be combined with the geometrical delay when shifting 
   the waveforms.  Using non-zero delays would optimize the beam at some
   wavenumber vector for which the delays were developed.
2) The database (dbname) must already exist (at least the descriptor file).

*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 30
#define MXPTS 1728000 /*enough for 1 day at 20 sps*/
#define rad2dg 57.2958
#define dg2km 111.19
#define pi 3.14159

double* dalloc(int);
double  get_shift(double,double,double,double,double);
int     do_grid(double,int,
        double,double,double,int,
        double,double,double,int,
        double*,double*);
int     make_beam(double,int,double,double,float*);
int     make_beam_root(double,int,double,double,int,float*);
int     make_beam_inc(double,int,double,double,float*);
int     make_beam_wrap(double,int,double,double,float*);
void    do_hilbert(float*,int);
double  sign(double);
int     slow_conv(double,double,double*,double*);
int     get_stat(float*,int,double*,double*);
double  get_amp(float*);
int     trgetwf(Dbptr, Arr **,float **,int *,double,double,double *,double *,
        int *,int *,void *);
int     trfilter_segs(int , int *, double *, float **, char *);
int     check2(int);
void    fft_(int *,float *,float *);

int     nsta,nbeam,spts,bflag,sflag;
double  rx[MXSTA],ry[MXSTA],delay[MXSTA];
float   *s[MXSTA],*hilb,*env;
FILE    *fout;

int main(int argc, char *argv[])
{
  int    ireturn,n,len,nbytes,i,j,index,inid,ns,nk,nkx,nky;
  int    nw,nz,iret,idec,npts,nwin,noff;
  int    mpts[MXSTA];
  double kxmin,kxmax,kxinc,kymin,kymax,kyinc,kmin,kmax,kinc,rdec,
         kopt,thetaopt,kxbest,kybest,slen,wlen,lat[MXSTA],lon[MXSTA],
         ang,kxax,kyax,scov,reflon,reflat,sr,sro,distance,azimuth,
         del,delo,tstart,tend,ts[MXSTA],te[MXSTA],wstart,wend,mean,stdv,calib,
         semin,semax,snmin,snmax,factor,fnyq,zero;
  char   asta[7],channame[9],refsta[7],searchExpr[200],corner[7];
  char   sta[MXSTA][7],chan[MXSTA][9];
  char   *arrayfile,*arrayname,*dbname,*dfile,*filter,*decfilter;
  Dbptr  db,dba,dbse,dbses,dbi,dbj,dbs,dbw,dbz,dbfk;
  FILE   *fp;
 
  arrayname = malloc(80);
  arrayfile = malloc(84);
  dbname    = malloc(80);
  dfile     = malloc(80);
  filter    = malloc(40);
  decfilter = malloc(40);
  hilb      = malloc(2*MXPTS*sizeof(float));
  env       = malloc(MXPTS*sizeof(float));

  printf("argc = %d\n",argc);
  if (argc < 15) 
  {
    printf("Usage: %s tstart slen wlen filter sr kmax nk ampfunc beamtype channame arrayname refsta dbname dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&tstart);
  sscanf(argv[2],"%lf",&slen);
  sscanf(argv[3],"%lf",&wlen);
  filter = argv[4]; 
  sscanf(argv[5],"%lf",&sr); 
  sscanf(argv[6],"%lf",&kmax); 
  sscanf(argv[7],"%d",&nk); 
  sscanf(argv[8],"%d",&sflag); 
  sscanf(argv[9],"%d",&bflag); 
  sscanf(argv[10],"%s",channame);
  arrayname = argv[11]; 
  printf("arrayname = %s\n",arrayname);
  sscanf(argv[12],"%s",refsta);
  dbname = argv[13];
  dfile = argv[14];


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
  while(fscanf(fp, "%s %s %lf", sta[nsta],chan[nsta],&delay[nsta]) != EOF)
  {
    nsta = nsta + 1;
  }
  printf("# stations to be used for beam = %d\n",nsta);
  fclose(fp);

/*Open the database and get required info on the array.*/

  printf("input database = %s\n",dbname);
  if (dbopen(dbname,"r+",&db) < 0)
  {
    elog_complain(0,"Could not open database.\n");
    return 1;
  }

  dbi  = dblookup(db,0,"instrument",0,0);
  dbse = dblookup(db,0,"sensor",0,0);
  dbs  = dblookup(db,0,"site",0,0);
  dbw  = dblookup(db,0,"wfdisc",0,0);

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

/*Get the waveforms for beam window (slen).*/
  tend = tstart + slen;
  printf("tstart,tend = %f %f\n",tstart,tend);

  nbeam = 0;
  for (j=0;j<nsta;j++)
  {
/*  Not all stations in the array may have a recording.  Before starting, just 
    fill the waveforms with zeroes.  If any waveforms do not have full length 
    when returned from trgetwf, they will be filled in with zeroes also.*/
    s[j] = malloc(MXPTS*sizeof(float));
    for (i=0;i<MXPTS;i++) s[j][i] = 0.0;
    sprintf( searchExpr, "sta =~ /%s/ && chan =~ /%s/ && time <= %lf && endtime >= %lf",sta[j],chan[j],tstart,tend);
    dbz = dbsubset(dbw,searchExpr,NULL);
    dbquery(dbz,dbRECORD_COUNT,&nz);
    if (nz == 0) 
    { 
      elog_complain(0,"No wfdisc records found for station %s.\n",sta[j]);
      continue;
    }
    dbz.record = 0;
    dbgetv(dbz,NULL,"calib",&calib,NULL);
    if (calib == 0.0) calib = 1.0;
    nbytes =  MXPTS*sizeof(float);
/*  Note: trgetwf does NOT apply calibration factor, so we will.*/
    iret=trgetwf(dbz,NULL,&s[j],&nbytes,tstart,tend,&ts[j],&te[j],&mpts[j],0,0);
    if (iret != 0)
    {
      elog_complain(0,"error in getting data for station %s\n",sta[j]);
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
    elog_complain(0,"No waveforms available for this time -- cannot continue.\n");
    return 1;
  }

/*Now do resampling, if needed.  Find the original sampling rate first.  The
  resampling is done in place.*/

  for(j=0;j<nsta;j++)
  {
    sprintf( searchExpr, "sta =~ /%s/ && chan =~ /%s/ && time <= %lf && endtime >= %lf",sta[j],chan[j],tstart,tend);
    dbses = dbsubset(dbse,searchExpr,NULL);
    dbses.record = 0;
    dbgetv(dbses,NULL,"inid",&inid,NULL);
    sprintf( searchExpr, "inid == %d",inid);
    dbi = dbsubset(dbi,searchExpr,NULL);
    dbi.record = 0;
    dbgetv(dbi,NULL,"samprate",&sro,NULL);

    npts = (tend - tstart)*sro + 1;
    delo = 1/sro;
    del = 1/sr;
    rdec = (sro/sr);
    if (rdec - floor(rdec) > 0.5) 
     idec = ceil(rdec);
    else
      idec = floor(rdec);
/*  printf("idec = %d\n",idec);*/
/*  Check old and new sample rates.*/
    if (fabs(rdec - idec) > 0.01) 
    {
      elog_complain(0,"Old sample rate over new sample rate != integer -- cannot continue.\n");
      elog_complain(0,"Old sample rate = %f; new sample rate = %f\n",sro,sr);
      return 1;
    }
    if (idec == 0) 
    {
      elog_complain(0,"Old sample rate over new sample rate < 1 -- cannot resample.\n");
      elog_complain(0,"Old sample rate = %f; new sample rate = %f\n",sro,sr);
      return 1;
    }        

/*  Check if new rate requires decimation; if so, do it.*/
    if (idec > 1) 
    {
/*    Filter first to prevent aliasing.*/
      del = 1/sro;
      fnyq = 1.0/(2.0*del);
      strcpy(decfilter,"\0");
      strcat(decfilter,"BW 0.0 0 ");
      sprintf(corner,"%6.2f",0.8*fnyq);
      strcat(decfilter,corner);
      strcat(decfilter," 4");
      iret = trfilter_segs(1,&mpts[j],&del,&s[j],decfilter);
      if (iret != 0)
      {
        elog_complain(0,"error in filtering waveform %d = %d\n",j,iret);
        return 1;
      }
      npts = (npts-1)/idec + 1;  /*new number of points*/
      for(i=0;i<npts;i++) s[j][i] = s[j][i*idec];
    }
  }
/*Last npts will be that used from now on;all waveforms should be same length.*/
  printf("filtered waveforms\n");
  printf("# points in signal = %d\n",npts);

/*Remove mean of traces.*/
  for(j=0;j<nsta;j++)
  { 
    iret = get_stat(s[j],npts,&mean,&stdv);
/*  printf("j,trace mean,stdv = %d %f %f\n",j,mean,stdv);*/
    for(i=0;i<npts;i++) s[j][i] = s[j][i] - mean;
  }

/*Filter the data prior to beaming if requested.*/
  if (strlen(filter) > 0)
  {
    for(j=0;j<nsta;j++)
    {
      del = 1/sr;
      iret = trfilter_segs(1,&npts,&del,&s[j],filter);
      if (iret != 0)
      {
        elog_complain(0,"error in filtering waveform %d = %d\n",j,iret);
        return 1;
      }
    }
  }

/*If doing incoherent beam, transform traces to Hilbert envelope.*/

  if (bflag < 5) goto beaming;

/*If series is not a power of two, determine next highest power of two.*/
  if((n=check2(spts))==0)
  {
    n=(int)(log10((double)spts)/log10(2.0))+1;
  }

  for(j=0;j<nsta;j++)
  {
/*  Fill with zeros to power of two and then interlace zeroes to make it
    hilb appear as a complex array.*/
    len = (int)pow(2.0,n);
    for(i=spts;i<len;i++)
      s[j][i]=0.0;
    for(i=0;i<len;i++)
    {
      hilb[i*2]  =s[j][i];
      hilb[i*2+1]=0.0;
    }

    do_hilbert(hilb,len);

/*  The Hilbert envelope is the modulus of the real and imag parts.*/
    for(i=0;i<spts;i++)
    {
      s[j][i]=sqrt(hilb[i*2]*hilb[i*2]+hilb[i*2+1]*hilb[i*2+1]);
    }
  }

beaming:

/*Define the slowness grid parameters.*/ 
  kmin  = -kmax;
  kinc = (kmax - kmin)/(nk - 1);
  kxmin = kmin;
  kymin = kmin;
  kxmax = kmax;
  kymax = kmax;
  kxinc = kinc;
  kyinc = kinc;
  nkx   = nk;
  nky   = nk;

/*Determine number of points in the moving window.*/
  spts = wlen/del + 1;

/*Start outer loop on window, moving it ahead and forming the slowness
  grid until the end of the data is reached.*/

  wstart = tstart;
  wend = wstart + wlen;
  index = (wstart - tstart)/del;

  while (wend <= tend)
  {
    printf("wstart,wend = %15.3lf %15.3lf\n",wstart,wend);
/*  do_grid will write the grid, so find byte offset of file now.*/
    noff = ftell(fout);

    if(do_grid(del,index,
               kxmin,kxinc,kxmax,nkx,
               kymin,kyinc,kymax,nky,
               &kxbest,&kybest) == 1)
    {
      fprintf(stderr,"Problem in computing grid.\n");
      return 1;
    }

    nwin = 1;

    slow_conv(kxbest,kybest,&kopt,&thetaopt);
    printf("Best ray parameter is: %lf (s/deg) \n"
           "Best backazimuth is: %lf (deg) \n"
           "Best x-slowness is: %lf (s/deg) \n"
           "Best y-slowness is: %lf (s/deg) \n",
            kopt,thetaopt,kxbest,kybest);
/*  Write parameters for this slowness grid to fkgrid table.*/
    semin = kmin;
    semax = kmax;
    snmin = kmin;
    snmax = kmax;
    kopt  = kopt;
    zero  = 0.0;
    printf("sta = %s\n", arrayname);
    printf("chan = %s\n", channame);
    printf("wstart = %f\n", wstart);
    printf("wend = %f\n", wend);
    ireturn = dbaddv(db,"fkgrid",
           "sta",arrayname,
           "refsta",refsta,
           "chan",channame,
           "time",wstart,
           "endtime",wend,
           "twin",wlen,
           "dtime",zero,
           "nt",nwin,
           "azimuth",thetaopt,
           "slo",kopt,
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
/*  Force the record to be written in case of duplicate key error.*/
    if (ireturn < dbINVALID)
    {
      db = dblookup(db,0,"fkgrid",NULL,NULL);
      db.record = dbaddnull(db);
      ireturn = dbputv(db,"fkgrid",
           "sta",arrayname,
           "refsta",refsta,
           "chan",channame,
           "time",wstart,
           "endtime",wend,
           "twin",wlen,
           "dtime",zero,
           "nt",nwin,
           "azimuth",thetaopt,
           "slo",kopt,
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
/*End outer loop on window.  Advance the window by its length.*/
    wstart = wstart + wlen;
    wend = wstart + wlen;
    index = (wstart - tstart)/del;
  }

  fclose(fout);
  dbclose(db);

/*End main routine.*/
}

int do_grid(double del,int index, 
            double kxlow,double kxinc,double kxhigh,int nkx,
            double kylow,double kyinc,double kyhigh,int nky,
            double *bkx,double *bky)
{
	int i,j;
	double minamp,maxamp,val,bestkx,bestky,kx,ky;
	float *sbeam,*valp;

        sbeam = malloc(MXPTS*sizeof(float));

        valp = malloc(nkx*nky*sizeof(float));

        printf("nkx,nky = %d %d\n",nkx,nky);
        minamp= 1.0e20;
        maxamp=-1.0e20;

        for(j=0;j<nky;j++)
        {
          ky=kylow+(double)j*kyinc;
          for(i=0;i<nkx;i++)
          {
            kx=kxlow+(double)i*kxinc;
            make_beam_wrap(del,index,kx,ky,sbeam);
            val=get_amp(sbeam);
            if(val>maxamp)
            {
                maxamp=val;
                bestkx=kx;
                bestky=ky;
            }
            if(val<minamp) minamp=val;
/*          Need to only store float, not double.*/
            valp[j*nkx+i] = val;
          }
        }
/*      Write entire grid to file as one binary matrix.*/
        fwrite(valp,sizeof(float),nkx*nky,fout);
        printf("beam min,max = %f %f\n",minamp,maxamp);
	free(sbeam);
        free(valp);
	*bkx=bestkx; 
        *bky=bestky;

        return 0;
}

int slow_conv(double kx,double ky,double *kmag,double *baz)
{

  *kmag=sqrt(kx*kx+ky*ky);	

  if(fabs(kx)<1.0e-6)
  {
    if(ky>0.0)
    {
      *baz=90.0-57.295*(3.1415926/2.0);
    } 
    else 
    {
      *baz=90.0-57.295*(3.0*3.1415926/2.0);
    }
  } 
  else 
  {
    *baz=90.0-57.296*atan2(ky,kx);
  }
  if(*baz<0.0){ *baz+=360.0; }
}

/*
* Enter kx, ky in s/km, output will be time shift in seconds
* If, for instance, kx and ky are positive, meaning signal is coming from
* the northeast, then for rx,ry both positive the signal is advanced at a
* station with coordinates (rx,ry), relative to that at recorded at (0,0).
* Therefore, a positive t means delay the signal, or shift it forward in time.
* The "delay" is an empirical (or estimated) delay at each station due
* to inhomogeneity (it may equal zero).  It is defined such that a positive
* delay means the signal is later than expected, relative to the plane wave; 
* and so t is adjusted downward.  Positive delay means the signal arrives 
* after it is expected; negative delay means it arrives before.
*/
double get_shift(double kx,double ky,double rx,double ry,double delay)
{
        double t;

        t=kx*rx + ky*ry;     /*uniform propagation shift*/
        t=t-delay;                   /*inhomogeneity terms*/

        return t;
}

void do_hilbert(float *hilb,int len)
{
  int i;
  float expsign;

  expsign = -1.0;
  fft_(&len,hilb,&expsign);

/*Double all values of positive frequency part of transform (except 0 freq)*/
  for(i=2;i<=len+1;i++)
    hilb[i]=2*hilb[i];

/*Set all values of negative frequency part of transform to zero.*/
  for(i=len+2;i<len*2;i++)
    hilb[i]=0.0;

  expsign = +1.0;
  fft_(&len,hilb,&expsign);

  return;
}


double sign(double x)
{
  if(x>=0)
      return 1.0;
  else 
      return -1.0;
}


int make_beam(double del,int index,double kx,double ky,float *sout)
{
	int i,j,sind;
	double tshift,dx,fraction;

	for(i=0;i<spts;i++)
        {
	    sout[i] = 0.0;
	}

	for(j=0;j<nsta;j++)
        {
            tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
            sind=(int)((-tshift)/del)+1;
	    dx=tshift-del*(double)((int)(tshift/del));
            fraction = dx/del;

            if( sind < 0 )
            {
                for(i=-sind;i<spts;i++)
                {
                    sout[i]+=s[j][sind+i+index] +
			fraction*(s[j][sind+i+index+1] - s[j][sind+i+index]); 
                }
            } 
            if( sind >= 0 )
            {
                for(i=0;i<(spts-sind-1);i++)
                {
                    sout[i]+=s[j][sind+i+index] +
			fraction*(s[j][sind+i+index+1] - s[j][sind+i+index]); 
                }
            } 
        }

        for(i=0;i<spts;i++)
        {
            sout[i]/=nbeam;
        }

	return 0;
}

int make_beam_root(double del,int index,double kx,double ky,int nr,float *sout)
{
        int i,j,sind;
        double tshift,dx,v1;

        for(i=0;i<spts;i++)
        {
            sout[i]=0.0;
        }

        for(j=0;j<nsta;j++)
        {
            tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
	    sind=(int)((-tshift)/del)+1;
            dx=tshift-del*(double)((int)(tshift/del));

            if( sind<0 )
            {
                for(i=-sind;i<spts;i++)
                {
		    v1=s[j][sind+i+index] +
                        (dx/del)*(s[j][sind+i+index+1]-s[j][sind+i+index]);
                    sout[i]+=sign(v1)*pow(fabs(v1),1.0/(double)nr);
                }
            } 
            if( sind>=0 )
            {
                for(i=0;i<(spts-sind-1);i++)
                {
		    v1=s[j][sind+i+index] +
                        (dx/del)*(s[j][sind+i+index+1]-s[j][sind+i+index]);
                    sout[i]+=sign(v1)*pow(fabs(v1),1.0/(double)nr);
                }
            } 
        }

        for(i=0;i<spts;i++)
        {
            sout[i]/=nbeam;
        }

        for(i=0;i<spts;i++)
        {
            sout[i]=sign(sout[i])*pow(fabs(sout[i]),(double)nr);
        }

	return 0;
}

int make_beam_inc(double del,int index,double kx,double ky,float *sout)
{
        int i,j,sind;
        double tshift,dx;

        for(i=0;i<spts;i++)
        {
            sout[i]=0.0;
        }

        for(j=0;j<nsta;j++)
        {
            tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
	    sind=(int)((-tshift)/del)+1;
            dx=tshift-del*(double)((int)(tshift/del));

            if( sind<0 )
            {
                for(i=-sind;i<spts;i++)
                {
                    sout[i]+=s[j][sind+i+index] +
                       (dx/del)*(s[j][sind+i+index+1]-s[j][sind+i+index]);
                }
            } 
            if( sind>=0 )
            {
                for(i=0;i<(spts-sind-1);i++)
                {
                    sout[i]+=s[j][sind+i+index] +
                       (dx/del)*(s[j][sind+i+index+1]-s[j][sind+i+index]);
                }
            } 
        }

        for(i=0;i<spts;i++)
        {
            sout[i]/=nbeam;
        }

        return 0;
}

/*
* Check global beamflag to see which type of beamforming to perform
*/
int make_beam_wrap(double del,int index,double kx,double ky,float *sout)
{
        switch(bflag){
            case 1:
                if(make_beam(del,index,kx,ky,sout)==1)
                {
                  elog_complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 2:
                if(make_beam_root(del,index,kx,ky,2,sout)==1)
                {
                  elog_complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 3:
                if(make_beam_root(del,index,kx,ky,3,sout)==1)
                {
                  elog_complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 4:
                if(make_beam_root(del,index,kx,ky,4,sout)==1)
                {
                  elog_complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 5:
                if(make_beam_inc(del,index,kx,ky,sout)==1)
                {
                  elog_complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            default:
                elog_complain(0,"Bad beam type selected\n");
                exit(1);
        }
        return 0;
}


int get_stat(float *dat,int ndat,double *mean,double *stdv)
{
	int i;
	double sum;

	sum=0.0;
	for(i=0;i<ndat;i++)
        {
	    sum+=dat[i];
	}

	*mean=sum/(double)ndat;

	sum=0.0;
	for(i=0;i<ndat;i++)
        {
	    sum+=(*mean-dat[i])*(*mean-dat[i]);
	}

	*stdv=sqrt(sum/(double)(ndat-1));

	return 0;
}

/*
* Get amplitude from appropriate time window
* of the beam. The usr chooses the method.
*/
double get_amp(float *sbeam)
{
        int i; 
        double val;

        if(sflag==1)
        {
            val=0.0;
            for(i=0;i<spts;i++)
            {
                if(fabs(sbeam[i])>val) val=fabs(sbeam[i]);
            }
        } 
        else if(sflag==2)
        {
            val=0.0; 
            for(i=0;i<spts;i++)
            {
                val+=fabs(sbeam[i]);
            }
            val/=(double)spts;
        } 
        else if(sflag==3)
        {
            val=0.0;
            for(i=0;i<spts;i++)
            {
                val+=sbeam[i]*sbeam[i];
            }
            val=sqrt(val/(double)spts);
        } 
        else 
        {
            elog_complain(0,"Bad choice for amplitude metric\n");
            exit(1);
        }
        return val;
}

/*Returns 0 if not power of two, otherwise log2*/
int check2(int n)
{
        int val,cnt=0;

        if(n<1){
            return(0);
        }

        val=n;
        while((val%2)==0){
            val/=2;
            if(val==1){
                return(cnt+1);
            }
            cnt++;
        }
        return(0);
}
