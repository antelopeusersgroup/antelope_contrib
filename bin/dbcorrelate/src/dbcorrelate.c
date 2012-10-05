/*Program to read in a list of orid pairs and find the matching phases to be
  cross-correlated for each event pair and then compute the cross-correlation.
  It assumes "event_pairs" has been run.  That preprocessor makes a list of
  origin pairs and their respective julian dates, in freeform, thus

  orid1 orid2 jdate1 jdate2 

  For each matching arrival found for a given pair, the waveforms are 
  cross-correlated to get a delay time estimate for the events, to be used in 
  relocation of the events, such as in HYPODD.

  If the database is broken out into years and days under the some directory,
  then the database name must be uniform over all years and days such that

  dbpath/yyyy/jjj

  is the relative path to a daily database.  

  Note: There may be multiple wfdisc entries that go with a given phase.  
  Some logic is used to choose the first adequate entry in such cases because 
  only one waveform can be used in the cross-correlation.

  Usage: dbcorrelate database dbpath flag winlen laglen cccmin infile outfile

  database: specifies the database name
  dbpath:   specifies the full path to database
  flag:     (1 or 0) describes the database configuration:
             1 = database is broken out into yyyy/jjj subdirectories
             0 = database is not broken out, just one database in dbpath
  winlen:   length (seconds) of the waveforms to be correlated.
  laglen:   maximum length of waveform offset (seconds) in correlation
  cccmin:   minimum acceptable correlation value (about 0.7)
  infile:   input file (created with "event_pairs" program)
  outfile:  output file in which results are written
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"
#include "coords.h"
#include "tr.h"

#define rtd 57.2958      /*radians to degrees*/
#define maxpts 501       /*longest cross-correlation window possible in points*/
#define backoff 0.0      /*amount (seconds) to shift back from arrival time*/
#define debug 0		 /*set to 1 if debug output needed*/

/* The prototypes are important -- do not remove.*/
int get_tau(float y1[],float y2[],double t1,double t2,int npts,
float laglen,double sr,double *tau,float *ccc,float *cccp);
int nint(double x);

int main(int argc, char *argv[])
{

  int         datasize,iret,flag,dbequal,nmatch,nccc,npts,isr1,isr2;
  int         orid1,orid2;
  long int    no1,nw1,nz1,nz2,npts1,npts2;
  int         jdate1,jdate2,year1,year2,jday1,jday2;
  double      depth1,depth2,lat1,lat2,lon1,lon2,otime1,otime2,atime1,atime2;
  double      sr1,sr2,sr,tau,otdiff,atdiff;
  double      tstart,tend,tstart1,tend1,tstart2,tend2;
  float       cccmin,ccc,cccp,winlen,laglen;
  float       *y1,*y2,y1d[maxpts],y2d[maxpts]; 
  char        astring[100],filename[80];
  char        *database,*dbpath,*database1,*database2;
  char        *path1,*path2;
  char        sta1[7],sta2[7],phase1[9],phase2[9],chan1[9],chan2[9];
  char        dir1[65],dir2[65],dfile1[33],dfile2[33];
  char        join_string[200],searchExpr[200];
  Dbptr       db1,db2,dbo1,dbo2,dba1,dba2,dbr1,dbr2,dbw1,dbw2,dbz1,dbz2;
  Dbptr       dboj1,dboj2,dbaj1,dbaj2,dbrj1,dbrj2;
  FILE        *efileptr,*ifileptr,*ofileptr,*sfileptr;

  database  = malloc(80);
  dbpath    = malloc(80);
  database1 = malloc(80);
  database2 = malloc(80);
  path1     = malloc(80);
  path2     = malloc(80);

  if (argc < 9)
  {
    printf(" usage: dbcorrelate database dbpath flag winlen laglen cccmin infile outfile\n");
    return 0;
  }

  datasize  = maxpts;

  strcpy(database,argv[1]);
  strcpy(dbpath,argv[2]);
  sscanf(argv[3],"%d",&flag);
  if (debug) printf("database,flag = %s %d\n",database,flag);

  sscanf(argv[4],"%f",&winlen);
  sscanf(argv[5],"%f",&laglen);
  sscanf(argv[6],"%f",&cccmin);
  
  strcpy(filename,argv[7]);
  if ((ifileptr = fopen(filename,"r")) == NULL)
  {
    printf("Error: Cannot open input file\n");
    exit(0);
  }

/*Note: next two files opened as "append".*/

/*Open a file for main output information.*/
  strcpy(filename,argv[8]);
  if ((ofileptr = fopen(filename,"a")) == NULL)
  {
    printf("Error: Cannot open output file\n");
    exit(0);
  }

/*Open a file for summary information.*/
  if ((sfileptr = fopen("summary.txt","a")) == NULL)
  {
    printf("Error: Cannot open summary file\n");
    exit(0);
  }

/*Open a file for error information.*/
  if ((efileptr = fopen("errors.txt","a")) == NULL)
  {
    printf("Error: Cannot open errors file\n");
    exit(0);
  }

  if (flag == 0) 
/*Both databases are the same for all orids and so it is opened once.*/
  {
    dbequal = 1;
    strcpy(database1,database);
    if (dbopen(database1,"r",&db1) < 0)
    {
      printf("Could not open database 1.\n");
      return 1;
    }
    dbo1 = dblookup( db1, NULL, "origin", NULL, NULL);
    dbquery( dbo1, dbRECORD_COUNT, &no1);
    if (debug) printf("no1 = %ld\n",no1);
    dba1 = dblookup( db1, NULL, "assoc", NULL, NULL);
    dbr1 = dblookup( db1, NULL, "arrival", NULL, NULL);
    dbw1 = dblookup( db1, NULL, "wfdisc", NULL, NULL);
    dbquery( dbw1, dbRECORD_COUNT, &nw1);
    if (debug) printf("nw1 = %ld\n",nw1);
  }
  
  if (debug) printf("database1 = %s\n",database1);

/*Loop over origin pairs, finding common stations and phases for each pair.*/
  while (fgets(astring,100,ifileptr) != NULL)
  {
    sscanf(astring," %d %d %d %d",&orid1,&orid2,&jdate1,&jdate2);
    printf("orid1,orid2,jdate1,jdate2 = %8d %8d %8d %8d\n",orid1,orid2,jdate1,jdate2);

    year1 = jdate1/1000;
    jday1 = jdate1 - year1*1000;
    year2 = jdate2/1000;
    jday2 = jdate2 - year2*1000;

/*  Determine the relative paths to the two daily databases if flag = 1.*/
    if (flag == 1)
    {
      if (year1 == year2 && jday1 == jday2)
        dbequal = 1;
      else
        dbequal = 0;
      sprintf(path1,"%s/%4d/%03d/",dbpath,year1,jday1);
      sprintf(path2,"%s/%4d/%03d/",dbpath,year2,jday2);
      strcat(path1,database);
      strcat(path2,database);
      strcpy(database1,path1);
      strcpy(database2,path2);
      if (debug) printf("database1,database2 = %s %s\n",database1,database2);
/*    Open the two databases.*/
      if (dbopen(database1,"r",&db1) < 0)
      {
        printf("Could not open database 1.\n");
        nz1 = 0;
        nz2 = 0;
        goto summary;
      }
      dbo1 = dblookup( db1, NULL, "origin", NULL, NULL);
      if (&dbo1 == NULL)
      {
        nz1 = 0;
        nz2 = 0;
        dbclose(db1);
        if (dbequal == 0) dbclose(db2);
        goto summary;
      }
      dba1 = dblookup( db1, NULL, "assoc", NULL, NULL);
      dbr1 = dblookup( db1, NULL, "arrival", NULL, NULL);
      dbw1 = dblookup( db1, NULL, "wfdisc", NULL, NULL);
      if (dbequal == 0)
/*    databases not on same day, so need to open 2nd one*/ 
      {
        if (dbopen(database2,"r",&db2) < 0)
        {
          printf("Could not open database 2.\n");
          nz1 = 0;
          nz2 = 0;
          dbclose(db1);
          goto summary;
        }
        dbo2 = dblookup( db2, NULL, "origin", NULL, NULL);
        if (&dbo2 == NULL)
        {
          nz1 = 0;
          nz2 = 0;
          dbfree(dbo1);
          dbclose(db1);
          if (dbequal == 0) dbclose(db2);
          goto summary;
        }
        dba2 = dblookup( db2, NULL, "assoc", NULL, NULL);
        dbr2 = dblookup( db2, NULL, "arrival", NULL, NULL);
        dbw2 = dblookup( db2, NULL, "wfdisc", NULL, NULL);
      }
      if (debug) printf("Opened databases.\n");
    } 

/*  Match the stations and phases to get correlation candidates for this 
    event pair.*/

    nmatch = 0;
    nccc = 0;

    sprintf(searchExpr,"orid == %d",orid1);
    dboj1 = dbsubset(dbo1,searchExpr,NULL);
    dbaj1 = dbjoin(dboj1,dba1,0,0,0,0,0);
    dbrj1 = dbjoin(dbaj1,dbr1,0,0,0,0,0);
    sprintf(join_string,"arrival.sta == wfdisc.sta && arrival.chan == wfdisc.chan && arrival.time > wfdisc.time && arrival.time < wfdisc.endtime");
    dbz1 = dbtheta(dbw1,dbrj1,join_string,0,0);
    dbquery( dbz1, dbRECORD_COUNT, &nz1);

    if (debug) printf("orid1,nz1 = %d %ld\n",orid1,nz1);

    sprintf(searchExpr,"orid == %d",orid2);
    sprintf(join_string,"arrival.sta == wfdisc.sta && arrival.chan == wfdisc.chan && arrival.time > wfdisc.time && arrival.time < wfdisc.endtime");
    if (dbequal == 0)
    {
      dboj2 = dbsubset(dbo2,searchExpr,NULL);
      dbaj2 = dbjoin(dboj2,dba2,0,0,0,0,0);
      dbrj2 = dbjoin(dbaj2,dbr2,0,0,0,0,0);
      dbz2 = dbtheta(dbw2,dbrj2,join_string,0,0);
    }
    else
    {
      dboj2 = dbsubset(dbo1,searchExpr,NULL);
      dbaj2 = dbjoin(dboj2,dba1,0,0,0,0,0);
      dbrj2 = dbjoin(dbaj2,dbr1,0,0,0,0,0);
      dbz2 = dbtheta(dbw1,dbrj2,join_string,0,0);
    }
    dbquery( dbz2, dbRECORD_COUNT, &nz2);
    if (debug) printf("orid2,nz2 = %d %ld\n",orid2,nz2);

    if (nz1 == 0 || nz2 == 0) goto freedb;

    for (dbz1.record=0;dbz1.record<nz1;dbz1.record++)
    {
      dbgetv(dbz1,NULL,"origin.time",&otime1,"lat",&lat1,"lon",&lon1,
                       "depth",&depth1,
                       "sta",sta1,"arrival.chan",chan1,"iphase",phase1,
                       "arrival.time",&atime1,"dir",dir1,"dfile",dfile1,
                       "samprate",&sr1,NULL);
      if (debug) printf("%s %s %s\n",sta1,chan1,phase1);

/*    Use only P and S phases.*/
      if (strncmp(phase1,"P",1) != 0 && strncmp(phase1,"S",1) != 0) continue;

/*    Search other table for exact match.*/
      for (dbz2.record=0;dbz2.record<nz2;dbz2.record++)
      {
        dbgetv(dbz2,NULL,"origin.time",&otime2,"lat",&lat2,"lon",&lon2,
                         "depth",&depth2,
                         "sta",sta2,"chan",chan2,"iphase",phase2,
                         "arrival.time",&atime2,"dir",dir2,"dfile",dfile2,
                         "samprate",&sr2,NULL);
        if (debug) printf("%s %s %s %s %s %s\n",sta1,chan1,phase1,sta2,chan2,phase2);
/*      Use only P and S phases.*/
        if(strncmp(phase2,"P",1) != 0 && strncmp(phase2,"S",1) != 0) continue;
/*      Match station and phase.*/
        if (strcmp(sta1,sta2) != 0 || strcmp(phase1,phase2) != 0) continue;
/*      For S phases, make sure components match.*/
        if (strcmp(chan1,chan2) != 0) continue;

/*      Match is made, so process the waveforms.*/
        nmatch++;
        if (debug) printf("match: sta,phase = %s %s\n",sta1,phase1);
        if (debug) printf("%s %s %s %s\n",dir1,dfile1,dir2,dfile2);

        tstart = atime1 - backoff;
        tend = atime1 + winlen - backoff;
        if (debug) printf("tstart,tend = %15.3f %15.3f\n",tstart,tend);
        iret = trgetwf(dbz1,NULL,&y1,0,tstart,tend,&tstart1,&tend1,&npts1,0,0);
        if (iret != 0)
        {
          fprintf(efileptr,"error in getting data from %s %s\n",dir1,dfile1);
          goto breakout;
        }
/*      Make sure the desired window is returned, to within a sample point.*/
        if (fabs(tstart - tstart1) >= 1/sr1) goto breakout;
        if (debug) printf("tstart1,tend1,npts1 = %15.3f %15.3f %ld\n",tstart1,tend1,npts1);

        tstart = atime2 - backoff;
        tend = atime2 + winlen - backoff;
        if (debug) printf("tstart,tend = %15.3f %15.3f\n",tstart,tend);
        iret = trgetwf(dbz2,NULL,&y2,0,tstart,tend,&tstart2,&tend2,&npts2,0,0);
        if (iret != 0) 
        {
          fprintf(efileptr,"error in getting data from %s %s\n",dir2,dfile2);
          goto breakout;
        }
/*      Make sure the desired window is returned, to within a sample point.*/
        if (fabs(tstart - tstart2) >= 1/sr2) goto breakout;
        if (debug) printf("tstart2,tend2,npts2 = %15.3f %15.3f %ld\n",tstart2,tend2,npts2);
        
/*      Make sure sample rates are the same.  This addresses a situation at NSL
        where the analog channels went from 100 to 50 sps in 2003.  This catches
        similar problems with other networks if they occur.*/
        isr1 = nint(sr1);
        isr2 = nint(sr2);
        if (isr1 != isr2) goto breakout;
        sr = sr1;
        if (npts1 > npts2)
          npts = npts2;
        else
          npts = npts1;

        memcpy(y1d,y1,4*npts);
        memcpy(y2d,y2,4*npts);
        if (debug) printf("y1d[0],y2d[0] = %f %f\n",y1d[0],y2d[0]);

        iret= get_tau(y1d,y2d,tstart1,tstart2,npts,laglen,sr,&tau,&ccc,&cccp);
        if (debug) printf("get_tau return = %d\n",iret);
        if (iret != 0) goto breakout;
        if (debug) printf("tau,ccc = %15.3f %5.2f\n",tau,ccc);

        if (fabs(ccc) >= cccmin) 
        {
          nccc++;
          otdiff = otime2 - otime1;
          atdiff = atime2 - atime1;
          fprintf(ofileptr,"%-6s %1s %8d%8d%8d%8d%8.3f%11.3e%15.3f%15.3f%15.3f\n",
             sta1,phase1,orid1,jdate1,orid2,jdate2,ccc,cccp,tau,otdiff,atdiff);
        }

breakout:
/*      Because a match has been made, exit the inner loop.*/
        break;

      }       /* end loop on arrivals of second event*/

    }       /* end loop on arrivals of first event*/

freedb:

    printf("nmatch,nccc = %d %d\n",nmatch,nccc);

    dbfree(dbz2);
    dbfree(dboj2);
    dbfree(dbaj2);
    dbfree(dbrj2);
    if (dbequal == 0) 
    {
      dbfree(dbo2);
      dbfree(dba2);
      dbfree(dbr2);
      dbfree(dbw2);
      dbclose(db2);
    }

    if (flag == 1)
    {
      dbfree(dbo1);
      dbfree(dba1);
      dbfree(dbr1);
      dbfree(dbw1);
    }
    dbfree(dbz1);
    dbfree(dboj1);
    dbfree(dbaj1);
    dbfree(dbrj1);
    if (flag == 1) dbclose(db1);

summary:

    fprintf(sfileptr," %8d %8d %3ld %3ld %3d %3d\n",orid1,orid2,nz1,nz2,nmatch,nccc);
    fflush(sfileptr);
    fflush(ofileptr);

  } /* end loop on event pairs */

  if (flag == 0) dbclose(db1);
  fclose(ifileptr);
  fclose(ofileptr);
  fclose(sfileptr);
}
