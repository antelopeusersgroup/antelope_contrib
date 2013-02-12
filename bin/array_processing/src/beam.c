/* 
This program will make one specific beam trace, as prescribed on command line.

Usage: beam tstart slen filter sr slow baz ampfunc beamtype delaytype channame arrayname refsta dbin dbout dir dfile

where

  tstart = start time of signal in epoch seconds
  slen = signal length to be beamed in seconds
  filter = filter specification in BRTT syntax, like "BW 0.5 4 8.0 4" (Must be
           put in quotes.)
  sr = desired sampling rate (<= actual sampling rate).  This increases 
       performance when looking at low-frequency signals relative to original
       sampling rate.
  slow = beam slowness (s/km) 
  baz = azimuth to which beam is directed (incoming signal azimuth) in 
           degrees CW from north
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
  delaytype = 0 means use delays in .arr file in addition to geometry delays
            = 1 means the .arr file delays are the complete delays to apply
  channame = channel name to be assigned to beam trace (maximum of 8 characters)
             arrayname and channame should be a unique pair of trace identifiers
  arrayname = name of array, for instance YMSZ; this will be the prefix of the
              .arr file to be read to get array info (maximum of 6 characters)
  refsta = name of station to which beam will be referenced
  dbin = input database name
  dbout = output database name
  dir = directory in which dfile will be put
  dfile = name of beam waveform file.  This file will be written in the current
          directory.

Note: ".arr" is appended in program to arrayname to make the file name for the 
      file containing array parameters.
*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 30
#define MXPTS 65536
#define rad2dg 57.2958
#define dg2km 111.1
#define pi 3.14159

double  get_shift(double,double,double,double,double);
int     make_beam(double,int,double,double,float*);
int     make_beam_root(double,int,double,double,int,float*);
int     make_beam_inc(double,int,double,double,float*);
int     make_beam_wrap(double,int,double,double,float*);
void    do_hilbert(float*,int);
double  sign(double);
int     get_stat(float*,int,double*,double*);
double  get_amp(float*);
int     trgetwf(Dbptr, Arr **,float **,int *,double,double,double *,double *,
        int *,int *,void *);
int     trfilter_segs(int , int *, double *, float **, char *);
int     check2(int);
int     trputwf(Dbptr,float *);
void    fft(float data[],int,int);

int     nsta,nbeam,spts,bflag,sflag,dflag;
double  rx[MXSTA],ry[MXSTA],delay[MXSTA];
float   *s[MXSTA],*hilb;

int main(int argc, char *argv[])
{
  int      n,len,nbytes,i,j,index,ns,iret,idec,mpts[MXSTA];
  long int nz,nw;
  double   slow,baz,rdec,slen,lat[MXSTA],lon[MXSTA],reflon,reflat,sr,sro,
           distance,azimuth,del,delo,tstart,tend,ts[MXSTA],te[MXSTA],kx,ky,
           mean,stdv,calib,fnyq;
  float    *sbeam;
  char     asta[7],refsta[7],channame[9],searchExpr[200],corner[7];
  char     sta[MXSTA][7],chan[MXSTA][9];
  char     *arrayfile,*arrayname,*dbin,*dbout,*filter,*decfilter,*dir,*dfile;
  Dbptr    db,dba,dbse,dbi,dbs,dbw,dbz;
  FILE     *fp;
 
  arrayname = malloc(80);
  arrayfile = malloc(84);
  dbin      = malloc(80);
  dbout     = malloc(80);
  filter    = malloc(20);
  decfilter = malloc(20);
  dir       = malloc(65);
  dfile     = malloc(33);
  hilb      = malloc(2*MXPTS*sizeof(float));
  sbeam     = malloc(MXPTS*sizeof(float));

  if (argc < 17) 
  {
    printf("Usage: %s tstart slen filter sr slow baz ampfunc beamtype delaytype channame arrayname refsta dbin dbout dir dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&tstart);
  sscanf(argv[2],"%lf",&slen);
  filter = argv[3]; 
  sscanf(argv[4],"%lf",&sr); 
  sscanf(argv[5],"%lf",&slow); 
  sscanf(argv[6],"%lf",&baz); 
  sscanf(argv[7],"%d",&sflag); 
  sscanf(argv[8],"%d",&bflag); 
  sscanf(argv[9],"%d",&dflag); 
  sscanf(argv[10],"%s",channame); 
  arrayname = argv[11]; 
  sscanf(argv[12],"%s",refsta);
  dbin = argv[13];
  dbout = argv[14];
  dir = argv[15];
  dfile = argv[16];

/*Open the array file and get array info.*/
  printf(" arrayname = %s\n", arrayname);
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
  printf("# stations to be used for beam = %d\n",nsta);
  fclose(fp);

/*Open the input database and get required info on the array.*/

  if (dbopen(dbin,"r+",&db) < 0)
  {
    complain(0,"Could not open input database.\n");
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
    when returned from trgetwf, they will be filled in with zeroes also.  */
    s[j] = malloc(MXPTS*sizeof(float));
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

  for(j=0;j<nsta;j++)
  {
    spts = (tend - tstart)*sro + 1;
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
        complain(0,"error in filtering waveform %d = %d\n",j,iret);
        return 1;
      }
      for(i=0;i<spts;i++) s[j][i] = s[j][i*idec];
    }
  }
/*Last spts will be that used from now on;all waveforms should be same length.*/
  printf("retrieved all waveforms\n");
  printf("# points in beam = %d\n",spts);

/*Remove mean of traces.*/
  for(j=0;j<nsta;j++)
  { 
    iret = get_stat(s[j],spts,&mean,&stdv);
/*  printf("j,trace mean,stdv = %d %f %f\n",j,mean,stdv);*/
    for(i=0;i<spts;i++) s[j][i] = s[j][i] - mean;
  }

/*Filter the data prior to beaming if requested.*/
  if (strlen(filter) > 0)
  {
    for(j=0;j<nsta;j++)
    {
      del = 1/sr;
      iret = trfilter_segs(1,&spts,&del,&s[j],filter);
      if (iret != 0)
      {
        complain(0,"error in filtering waveform %d = %d\n",j,iret);
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
    len = (int)pow(2.0,(double)n);
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
/*Close input database now.*/
  dbclose(db);
/*Open the output database.*/
  if (dbopen(dbout,"r+",&db) < 0)
  {
    complain(0,"Could not open output database.\n");
    return 1;
  }
  dbw  = dblookup(db,0,"wfdisc",0,0);

/*Initialize the beam trace.*/
  for (i=0;i<spts;i++)
  {
    sbeam[i] = 0.0;
  }
  printf("intialized beam\n");

/*Now make beam trace.*/

  index = 0;
  kx = slow*cos(pi/2 - baz/rad2dg);
  ky = slow*sin(pi/2 - baz/rad2dg);
  printf("slow,baz,kx,ky = %f %f %f %f\n",slow,baz,kx,ky);
  make_beam_wrap(del,index,kx,ky,sbeam);
  iret = get_stat(sbeam,spts,&mean,&stdv);
  printf("mean, stdv of beam data = %f %f\n",mean,stdv);
 
/*Write out the beam and create wfdisc record for dbout.*/

  dbquery(dbw,dbRECORD_COUNT,&nw);
  iret = dbaddv(dbw, 0,
               "sta", arrayname,
               "chan",channame,
               "time", tstart,
               "endtime", tend,
               "jdate", yearday(tstart),
               "nsamp", spts,
               "samprate", sr,
               "datatype", "u4",
               "dir", dir,
               "dfile", dfile,
               "foff", 0,
               NULL);
  if (iret < 0)
  {
    complain(0,"Could not write record to wfdisc table.\n");
    return 1;
  }
/*Write the trace to file specified in new wfdisc record.*/
  dbw.record = nw;
  iret = trputwf(dbw,sbeam);
  if (iret < 0)
  {
    complain(0,"Could not write beam data file.\n");
    return 1;
  }

  dbclose(db);

/*End main routine.*/
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

        t=(kx*rx + ky*ry);     /*uniform propagation shift*/
        t=t-delay;                   /*inhomogeneity terms*/

        return t;
}

void do_hilbert(float *hilb,int len)
{
  int i;
  int isign;

  isign = -1;
  fft(hilb,len,isign);

/*Double all values of positive frequency part of transform (except 0 freq)*/
  for(i=2;i<=len+1;i++)
    hilb[i]=2*hilb[i];

/*Set all values of negative frequency part of transform to zero.*/
  for(i=len+2;i<len*2;i++)
    hilb[i]=0.0;

  isign = +1;
  fft(hilb,len,isign);

  return;
}


double sign(double x)
{
        if(x>=0){
            return 1.0;
        } else {
            return -1.0;
        }
}


int make_beam(double del,int index,double kx,double ky,float *sout)
{
	int i,j,sind;
	double tshift,dx;

	for(i=0;i<spts;i++)
        {
	    sout[i]=0.0;
	}

	for(j=0;j<nsta;j++)
        {
            if (dflag == 0) tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
            if (dflag == 1) tshift=-delay[j];
            sind=(int)((-tshift)/del)+1;
	    dx=tshift-del*(double)((int)(tshift/del));

            if( sind<0 )
            {
                for(i=-sind;i<spts;i++)
                {
                    sout[i]+=s[j][sind+i+index] +
			(dx/del)*(s[j][sind+i+index+1] - s[j][sind+i+index]); 
                }
            } 
            if( sind>=0 )
            {
                for(i=0;i<(spts-sind-1);i++)
                {
                    sout[i]+=s[j][sind+i+index] +
			(dx/del)*(s[j][sind+i+index+1] - s[j][sind+i+index]); 
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
            if (dflag == 0) tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
            if (dflag == 1) tshift = -delay[j];
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
            if (dflag == 0) tshift=get_shift(kx,ky,rx[j],ry[j],delay[j]);
            if (dflag == 1) tshift = -delay[j];
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
                  complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 2:
                if(make_beam_root(del,index,kx,ky,2,sout)==1)
                {
                  complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 3:
                if(make_beam_root(del,index,kx,ky,3,sout)==1)
                {
                  complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 4:
                if(make_beam_root(del,index,kx,ky,4,sout)==1)
                {
                  complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            case 5:
                if(make_beam_inc(del,index,kx,ky,sout)==1)
                {
                  complain(0,"Problem making beam\n");
                  exit(1);
                }
                break;
            default:
                complain(0,"Bad beam type selected\n");
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
            complain(0,"Bad choice for amplitude metric\n");
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
