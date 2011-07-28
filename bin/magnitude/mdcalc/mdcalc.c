/*
This program emulates the computation of coda-duration magnitude MD by the
PICKEM program at UNR Seismological Laboratory.  PICKEM was run interactively,
and the analyst got immediate feedback on coda-length calculation at each
station and could change the coda-start pick according to the result.  This
has been automated in the current program due to lack of a communication
method with dbpick.  Thus, this program is a post-processor.

The algorithm basically fits an exponential function to the coda, starting
at the point picked by the analyst.  The correlation coefficient of the fit
is computed, with r > 0.90 being acceptable, according to PICKEM practice.
The magnitude is computed from all the individual coda lengths according to

    md = 2.65*log10[Sum(ti)/n]-1.70

where ti are the coda lengths determined for n stations.  Note that no
distance correction is involved.  This formula evolved from comparison of
md and ml for events with ml > 3 in Nevada and eastern California.

This program looks for C1 or C2 picks in the arrival table.
A "C1" pick by the analyst marks the start of the coda-length estimation
window.  An exponentially decaying function is fit to the data beyond "C1".
Due to the fact that interactive processing is eliminated, the program will
do a search around the time of this "C1" phase to get a coda estimate with
a correlation coef. > 0.90.  The search is performed forwards by increments
which depend on event size.  Event size must be given on the command line if
other than small -- S, M, L are the 3 allowable inputs.  For larger events,
this increases the coda length to be fit.  This option is for compatibility
with PICKEM and anything other than S, the default, is not expected to be
used but rarely.  For any event large enough to do M or L, a standard ML
magnitude should be available.

A "C2" pick by the analyst specifies the end of the coda.  The coda length is
then merely the C2 time minus the P time.  No computations are involved.
This program will compute MD for events in the Antelope origin table using
available data as indicated by arrival picks in the arrival table.

Only components specified in the parameter file (default = mdcalc.pf) are used
in the calculation even though "C" picks can be made on any component.
Former practice with PICKEM at UNRSL was to use only vertical components.

Usage: mdcalc -p pfile database orid size

-p pfile = parameter file name [optional, "mdcalc" is default]
database = database name (for example, "reno")
orid     = orid of event to be processed  (for example, "43769")
size     = earthquake size ("S"|"M"|"L", with default = "S" if not given)

David von Seggern 02/10/2001
*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"
#include "arrays.h"

#define debug 0

static void usage ()
{
  fprintf (stderr, "usage: mdcalc [-p pfname] db orid [eqsize]\n");
  exit (1) ;
}

int main(int argc, char *argv[])
{
  char		eqsize;
  char		afile[80],adir[80];
  char		searchExpr[80];
  char		phase[9];
  char		*line,*asta,*achan,*database,*instype;
  char		pfname[128];
  char		sta[299][5],chan[299][5];
  int		nsta,iter,ireturn,iaccept,i,npts,ier,iread,len,nsamp;
  int		noff,noff_P,noff_C;
  int		orid,no,na,nw,ns,nc,nmd,is_P,is_C1,is_C2,index;
  int           itrgetz,ndur;
  int		jstart,jend,coda_end,jerr;
  int		offset,window;
  int		offsets = 3;
  int		windows = 3;
  int		maxiter = 5;
  float		xnoise,corrcoef;
  float 	xfactor = 4.0;
  float		*zdata;
  float         adata[50000];
  double	samprate;
  double	wtime,codalen;
  double	otime,etime,atime,starttime,endtime,t1,t2,Ptime,C1time,C2time;
  double	offset_P,offset_C;
  double        avemd,md,dur,sumdur,avedur;
  double	elat,elon,depth,adist;
  double 	pat,sat,pdiff,sdiff;
  double	slat[299],slon[299],delta[299],distance[299],azimuth[299];
  Dbptr		db,dbo,dbs,dba,dbw,dbaa,dbww;
  Pf		*pf;
  Tbl		*tbl;
  FILE		*outfile;

  database = malloc(80);
  achan = malloc(4);
  asta = malloc(5);

  if (debug == 1) printf("argc = %d\n",argc);
  if (argc < 3) 
  {
    printf("Usage: %s [-p pfname] database orid [eqsize]\n", argv[0]);
    return 1;
  }

  strcpy (pfname, "mdcalc");
  for (argv++,argc--; argc>0; argv++,argc--) 
  {
    if (**argv != '-') break;
    if (!strcmp(*argv, "-p")) 
    {
      argv++; argc--;
      if (argc < 1) 
      {
        elog_complain(0, "Need argument for -p\n");
        usage();
        return 1;
      }
      strcpy (pfname, *argv);
    }
    else 
    {
      elog_complain(0, "Unrecognized argument '%s'.\n", *argv);
      usage();
      return 1;
    }
  }

  if (argc < 1) 
  {
    elog_complain(0, "Need dbname argument.\n");
    usage();
    return 1;
  }
  database = *argv;
  if (debug == 1) printf("database = %s\n",database);

  argv++; argc--;
  if (argc < 1) 
  {
    elog_complain(0, "Need orid argument.\n");
    usage();
    return 1;
  }
  orid = atoi(*argv);
  printf("MD will be computed for orid = %d\n",orid);

/*Get optional earthquake size argument. */
/*Small (S) size is default -- can be medium (M) or large (L) if indicated.*/
  eqsize = 'S';
  argv++; argc--;
  if (argc == 1) 
  {
    sscanf(*argv,"%c",&eqsize);
    if ( (eqsize != 'S')  && (eqsize != 'M')  && (eqsize != 'L') )  
    {
      elog_complain(0, "EQ size must be 'S', 'M', or 'L'.\n");
      usage();
      return 1;
    }
  } 
  if (eqsize == 'S') codalen =  15;
  if (eqsize == 'M') codalen =  45;
  if (eqsize == 'L') codalen = 300;
  if (debug == 1) printf("eqsize,codalen = %c %f \n",eqsize,codalen);

/*Open a file for ASCII output of md calculations.*/
  outfile = fopen("md.out","a");
  if (outfile == NULL)
  {
    printf("Cannot open the file md.out.\n");
    return 1;
  }

  if (dbopen(database,"r+",&db) < 0)
  {
    elog_complain(0,"Could not open database.\n");
    return 1;
  }

/*Read mdcalc parameter file */

  if (pfread (pfname, &pf) < 0) 
  {
    elog_complain(0, "pfread(%s) error.\n", pfname);
    exit (1);
  }
  tbl = pfget_tbl(pf,"stachans");
  if (tbl == NULL)
  {
    elog_complain(0, "pfget_tbl error.\n");
    exit (1);
  }
  nsta = maxtbl(tbl);
  for (i=0; i<nsta; i++) 
  {
    line = (char *) gettbl (tbl, i);
    ireturn = sscanf (line, "%s %s", sta[i], chan[i]);
    if (ireturn < 2) 
    {
      elog_complain(0, "Cannot parse parameter file line: %s", line);
      return;
    }
  }
/*Get station lat,lon pairs.*/
  for (i=0; i<nsta; i++)
  {
    strcpy(asta,sta[i]);
    dbs = dblookup(db,0,"site","sta",asta);
    dbgetv(dbs,0,"lat",&slat[i],"lon",&slon[i],0);
/*  dbfree(dbs);*/
  }

  dbo = dblookup(db,0,"origin",0,0);
  dba = dblookup(db,0,"arrival",0,0);
  dbw = dblookup(db,0,"wfdisc",0,0);

/*Get the event for the given orid.*/
  sprintf(searchExpr, "orid == %d", orid);
  if (debug == 1) printf("searchExpr = %s\n",searchExpr);
  dbo = dbsubset(dbo,searchExpr,NULL);
  dbquery(dbo,dbRECORD_COUNT, &no);
  if ( no == 0 )
  {
    printf("Did not find event with orid = %d\n",orid);
    return 1;
  }

  dbo.record = 0;
  dbgetv(dbo,NULL,"time",&otime,"mb",&md,"lat",&elat,"lon",&elon,
           "depth",&depth,NULL);
  if (debug == 1) printf("event: %s %f %f %f\n",strtime(otime),elat,elon,md);
  sumdur = 0;
  ndur = 0;

/*Loop on all stations in parameter file.*/

  for (index=0; index<nsta; index++)
  {
    strcpy(asta,sta[index]);
    if (debug) printf("Processing station %s\n",asta);
    strcpy(achan,chan[index]);
/*  Compute distance and azimuth of station from epicenter.*/
    dist(rad(elat),rad(elon),rad(slat[index]),rad(slon[index]),
      &delta[index],&azimuth[index]);
    distance[index] = deg(delta[index])*111.2;       /*convert to km*/
    azimuth[index] = deg(azimuth[index]);               /*convert to degrees*/

/*  Get any arrivals for this station for this event.
    Set the search window from origin time to time of 1.5 km/s waves.  This 
    window may be so long that occasionally arrivals from another event are
    included.  P and S times can be checked against predicted times, but this
    is not possible for coda times C1 or C2.
    If window is too short, set to at least 10 s.
*/
    etime = otime + distance[index]/1.5;
    if (etime - otime < 10.) etime = otime + 10.;
    if (debug == 1) printf("otime,etime = %f %f\n",otime,etime);
    sprintf(searchExpr,
      "sta == '%s' && time > %17.5lf && time < %17.5lf",
      asta,otime,etime);
    dbaa = dbsubset(dba,searchExpr,NULL);
    dbquery(dbaa,dbRECORD_COUNT, &na);
    if (na == 0) goto nextsta;
    printf("Station %s: Found %d arrivals.\n",asta,na);

/*  Examine the arrivals; determine if P and C picks exist.*/
    is_C1 = 0;
    is_C2 = 0;
    is_P = 0;
    C1time = 0.0;
    C2time = 0.0;
    Ptime = 0.0;
    for (dbaa.record = 0 ; dbaa.record < na ; dbaa.record++)
    {
      dbgetv(dbaa,NULL,"iphase",phase,"time",&atime,NULL);
      if (debug == 1) printf("phase, time = %s %f\n",phase,atime);
      if (strncmp(phase,"P",1) == 0)
      {
        adist = deg(delta[index]);
        pat = pphasetime(adist,depth); 
        pdiff = fabs(atime - otime - pat);
        if (pdiff < 2) 
        {
          is_P = 1;
          Ptime = atime;
        }
      }
      if (strncmp(phase,"C1",1) == 0)
      {
        adist = deg(delta[index]);
        sat = sphasetime(adist,depth); 
        sdiff = fabs(atime - otime - sat);
/*      Assume C1 pick will come within 10 sec of S unless an M or L event.*/
        if (sdiff < 10 || eqsize == 'M' || eqsize == 'L') 
        {
          is_C1 = 1;
          C1time = atime;
        }
      }
      if (strncmp(phase,"C2",1) == 0)
      {
        is_C2 = 1;
        C2time = atime;
      }
    }
    if (!is_P || !(is_C1 || is_C2)) goto nextsta;
    printf("station %s: found P and C arrivals.\n",asta);

/*  If the coda pick is C2, it's simple.  Take C2 time - P time.
    if (is_C2) 
    {
      dur = C2time - Ptime; 
      if (dur < 0) goto nextsta;
      fprintf(outfile,"%s %s %7.2f\n", strtime(otime),asta,dur);
      sumdur = sumdur + dur;
      ndur = ndur + 1;
      goto nextsta;
    }
    
/*  Get the seismograms corresponding to these arrivals.  The begin and end
    time must bracket the C1 time.*/
    sprintf(searchExpr,
      "sta == '%s' && chan == '%s' && time < %17.5lf && endtime > %17.5lf",
      asta,achan,C1time,C1time);
    dbww = dbsubset(dbw,searchExpr,NULL);
    dbquery(dbww,dbRECORD_COUNT, &ns);
    if (debug == 1) printf("Found %d seismograms.\n",ns);
    if (ns < 1) goto nextsta;

/*  Get the start time and sample rate.*/
    dbww.record = 0;
    dbgetv(dbww,NULL,"time",&wtime,"nsamp",&nsamp,
        "samprate",&samprate,NULL);
    if (debug == 1) printf("wfdisc start time, sample rate = %f %f\n",wtime,samprate);

/*  Use P and C times for MD computation window.*/
    starttime = Ptime - 4.5;
    endtime = C1time + codalen + maxiter*(codalen/15) + windows/2;
    if (debug == 1) printf("start,end time of retrieved segment =%f %f\n",starttime,endtime);
    dbww.record = 0;
    zdata = NULL;
    ireturn = trgetwf(dbww,NULL,&zdata,0,starttime,endtime,&t1,&t2,&npts,0,0);
    if (debug == 1) printf("npts = %d\n",npts);
    if (ireturn != 0) 
    {
      printf("Problem reading channel.\n");
      goto nextsta;
    }
    else
    {
      itrgetz = 1;
      if (debug == 1) printf("Read data successfully.\n");
    }
    offset_P = Ptime - starttime;
    noff_P= offset_P * samprate;
    if (debug == 1) printf("offset(sec) to P, noff, npts = %f %d %d\n",offset_P,noff_P,npts);
    offset_C = C1time - starttime;
    noff_C = offset_C * samprate;
    if (debug == 1) printf("offset(sec) to C, noff, npts = %f %d %d\n",offset_C,noff_C,npts);
    memcpy(adata,zdata,4*npts);
/*  printf("%f\n",adata[0]);*/
    offset = offsets*samprate;
    window = windows*samprate;
    rms_(&adata,&offset,&window,&xnoise); 
    if (debug == 1) printf("noise = %f\n",xnoise);

    iter = 0;
    retry:
    iter = iter + 1;
    if (iter > maxiter ) goto nextsta;
/*  Here is way to repeatedly move the start time of the coda until a good
    result is obtained (corrcoef > 0.9).*/
    jstart = noff_C + (iter-1)*(codalen/15)*samprate;
    jend = jstart + codalen*samprate - window/2 - 1;
    if (debug == 1) printf("iter,jstart,jend = %d %d %d\n",iter,jstart,jend);
    if (jend > npts) 
    {
      printf("jend > npts: %d %d\n",jend,npts);
      goto retry;
    }
    coda_(&adata,&jstart,&jend,&xnoise,&xfactor,&coda_end,&corrcoef,&jerr);
    if (debug == 1) printf("jerr,coda_end = %d %d\n",jerr,coda_end);
    if (jerr != 0) goto nextsta;
    printf("iter,corrcoef = %d %f\n",iter,corrcoef);
    if (corrcoef < 0.9) goto retry;

    dur = (coda_end - noff_P)/samprate; 
    if (dur <= 0) goto nextsta;
    printf("duration = %7.2f\n",dur);
    fprintf(outfile,"%s %s %7.2f\n", strtime(otime),asta,dur);
    sumdur = sumdur + dur;
    ndur = ndur + 1;

    nextsta:
    if (itrgetz == 1) free(zdata);
    dbfree(dbaa);
  }

  if (ndur > 0)
  {
    avedur = sumdur/ndur;
    avemd = 2.65*log10(avedur) - 1.70;
    printf("average MD = %5.2f from %d stations\n",avemd,ndur);
    dbputv(dbo,NULL,"mb",avemd,NULL);
  }

  fclose(outfile);
  return 0;
}
