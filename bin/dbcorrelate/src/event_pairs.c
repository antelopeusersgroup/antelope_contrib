/*Program to determine the best events to use in cross-correlation of 
  waveforms.

  usage: event_pairs database xdifmax ydifmax zdifmax maxlinks outfile

  where 

    database = database name to get data from
    xdifmax  = maximum x (E-W) distance within which to accept event 
               pairs for correlation
    ydifmax  = maximum y (N-S) distance within which to accept event 
               pairs for correlation
    zdifmax  = maximum z distance within which to accept event pairs 
               for correlation
    maxlinks = sets an upper cutoff on the number of events to be 
               associated to a given event. This limits the links 
               created in a dense volume of earthquakes.  If maxlinks = 0,
               then no upper limit is placed on the number of links.  This
               may be OK for a small cluster of events, but will be unwise
               for large datasets.  There is no minimum links parameter; 
               while it is true that there may be isolated events with 
               only one or two links, a decision on whether or not to use 
               such poorly linked events can be postponed until HYPODD runs.  
    outfile =  name of ASCII file to hold the results

    NOTE! In order for this program to work properly, the origin table must 
          have been time sorted.
*/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db.h"
#include "coords.h"

#define rtd 57.2958

int main(int argc, char *argv[])
{

  Dbptr       db,dbo,dbop,dbos;
  int         orid1,orid2;
  long int    no,nos;
  int         jdate1,jdate2;
  int         nlinks,irec,maxlinks;
  double      depth1,depth2,lat1,lat2,lon1,lon2,otime1,otime2;
  double      xdif,ydif,zdif;
  char        *database,filename[80];
  char        searchExpr[200],sortExpr[100];
  FILE        *ofileptr;
  Tbl         *t;

  database = malloc(80);

  if (argc < 6)
  {
    printf(" usage: event_pairs database xdifmax ydifmax zdifmax maxlinks outfile\n");
    return 0;
  }

  database = strcpy(database,argv[1]);
  if (dbopen(database,"r+",&db) < 0)
  {
    printf("Could not open database.\n");
    return 1;
  }
  dbo = dblookup( db, NULL, "origin", NULL, NULL);
  dbquery(dbo,dbRECORD_COUNT,&no);
  printf("# origins   = %ld\n",no);

  strcpy(filename,argv[6]);
  if ((ofileptr = fopen(filename,"w")) == NULL)
  {
    printf("Error: Cannot open output file\n");
    exit(0);
  }

  xdif = atof(argv[2]);
  ydif = atof(argv[3]);
  zdif = atof(argv[4]);
  maxlinks = atoi(argv[5]);

/*Loop over events, finding likely other events to use in correlation.*/
  irec = 0;
  while (irec < no - 1)
  {
    nlinks = 0;
/*  printf("origin record = %d\n",irec);*/
    dbo.record = irec;
    dbgetv( dbo, NULL, "orid", &orid1, "time", &otime1, "lat", &lat1, 
           "lon", &lon1, "depth", &depth1, "jdate", &jdate1, NULL);

    sprintf(searchExpr,"orid == %d",orid1);

/*  Prepare a sorted table of nearest origins to the current one.  Use lat,lon
    and a looser depth criterion due to known problems with depth accuracy.
    Events will be taken sequentially from the sorted table, up to a certain
    maximum number of events, to ensure that the closest events are used.
    First subset from full origin table only those events which are in 
    reasonable proximity to current event.  Add one more constraint that 
    only events with higher orid numbers will be used.  This will avoid 
    correlating, for instance, events 5 and 4 when 4 and 5 are already done.
    It also prevents correlating a given event with itself.  THIS ASSUMES
    THE ORIGIN TABLE HAS SEQUENTIALLY INCREASING ORID'S.  
*/ 
    sprintf(searchExpr,"abs(%10.4f - lat)*111.11 < %8.4f && abs(%10.4f - lon)*cos((%10.4f + lat)/2)*111.11 < %8.4f && abs(%6.2f - depth) < %8.4f && time > %f",lat1,ydif,lon1,lat1,xdif,depth1,zdif,otime1);
    dbos = dbsubset(dbo,searchExpr,NULL);
    dbquery(dbos,dbRECORD_COUNT,&nos);
/*  printf("# near origins found = %d\n",nos);*/
    if (nos == 0) 
    {
      irec = irec + 1;
      continue;
    }
    sprintf(sortExpr,"distance(lat,lon,%10.4f,%10.4f)*111.1+abs(depth - %6.2f)/2",lat1,lon1,depth1);
/*  printf("%s\n",sortExpr);*/
    t = strtbl(sortExpr,NULL);
    dbop = dbsort(dbos,t,0,0);

    nlinks = 0;

/*  Start inner loop on origins in new table, stopping at maxlinks (or EOF).*/

    for (dbop.record=0;dbop.record<nos;dbop.record++)
    {
      if (maxlinks != 0 && nlinks == maxlinks) break;
      dbgetv( dbop, NULL, "orid", &orid2, "time", &otime2, "lat", &lat2, 
             "lon", &lon2, "depth", &depth2, "jdate", &jdate2, NULL);

      nlinks = nlinks + 1;
      printf("orid1,orid2 =  %8d %8d\n",orid1,orid2);
      fprintf(ofileptr," %8d %8d %8d %8d\n",orid1,orid2,jdate1,jdate2);

    }	   /* end inner loop on event pairs */

    dbfree(dbop);
    dbfree(dbos);
    fflush(ofileptr);

    irec = irec + 1;

  }     /* end outer loop on event pairs */

  dbclose(db);
  fclose(ofileptr);
}
