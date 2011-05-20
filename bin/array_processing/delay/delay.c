/*
This computes the delays (seconds) to be applied to optimally beam a wave
with a given back azimuth and slowness.  A positive delay means the wave is
behind wrt the reference station (arrives later); a negative delay means it 
is ahead (arrives earlier).

Usage: delay slow baz delaytype arrayname refsta dbname dfile

where
      slow = slowness of wave (s/km)
      baz = back azimuth of wave, as seen by array (degrees cw from north) 
      delaytype = 1 means use delays in .arr file in addition to geometry delays
                = 0 means the delays are computed from the array geometry only
      arrayname = name of array
      refsta = station which will be the reference point (delay = 0,0)
      dbname = database name (the prefix used for the database)
      dfile = file name for where results will be stored

Note: ".arr" is appended in program to arrayname to make file name for file
      containing array parameters.
*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 500
#define rad2dg 57.2958
#define dg2km 111.19
#define pi 3.14159

int main(int argc, char *argv[])
{
  int    j,nsta,ns,delaytype;
  double lat[MXSTA],lon[MXSTA],rx,ry;
  double reflat,reflon,kx,ky;
  double slow,baz,distance,azimuth,shift,delay[MXSTA];
  char   refsta[7],asta[7],searchExpr[80];
  char   *arrayname,*arrayfile,*dbname,*dfile;
  char   sta[MXSTA][7],chan[MXSTA][9];
  FILE   *fp;
  Dbptr  db,dba,dbs;

  arrayname = malloc(9);
  arrayfile = malloc(13);
  dbname = malloc(80);
  dfile = malloc(80);

  if (argc < 8)
  {
    printf("Usage: %s slow baz delaytype arrayname refsta dbname dfile\n",argv[0]);
    return 1;
  }

  sscanf(argv[1],"%lf",&slow);
  sscanf(argv[2],"%lf",&baz);
  sscanf(argv[3],"%d",&delaytype);
  arrayname = argv[4];
  sscanf(argv[5],"%s",refsta);
  dbname = argv[6];
  dfile = argv[7];

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
  fclose(fp);
  printf("# of stations = %d\n",nsta);

  if (dbopen(dbname,"r",&db) < 0)
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

  kx = slow*cos((90 - baz)/rad2dg);
  ky = slow*sin((90 - baz)/rad2dg);

  printf("computing time delays for a wave at array = %s\n",arrayname);
  printf("refsta,reflat,reflon = %s %f %f\n",refsta,reflat,reflon);
  printf("slowness(s/km), back azimuth(dg cw from north)   = %f %f\n",slow,baz);
  printf("x and y are Cartesian coordinates (+x is east; +y is north)\n");
  printf("positive delay means wave arrives later; negative means sooner\n");
  printf("output file has: station  channel  delay(s)\n");

  if ( (fp = fopen(dfile,"w")) == NULL)
  {
    elog_complain(0,"Could not open output file.\n");
    return 1;
  }

  for (j=0;j<nsta;j++)
  {
    sprintf( searchExpr, "sta =~ /%s/",sta[j]);
    dba = dbsubset(dbs,searchExpr,NULL);
    dba.record = 0;
    dbgetv(dba,NULL,"lat",&lat[j],"lon",&lon[j],NULL);
/*  Note: dist returns distance in radians and azimuth in radians.*/
    dist(reflat/rad2dg,reflon/rad2dg,lat[j]/rad2dg,lon[j]/rad2dg,&distance,&azimuth);
    distance = distance*rad2dg*dg2km;
    rx=distance*cos(pi/2 - azimuth);
    ry=distance*sin(pi/2 - azimuth);
    shift = kx*rx+ky*ry;
    if (delaytype == 1) delay[j] = -shift + delay[j];
    if (delaytype == 0) delay[j] = -shift;
    printf("%3d %-5s %13.3f %7.2f %7.2f\n",j+1,sta[j],delay[j],rx,ry);
    fprintf(fp,"%-5s %-5s %13.3f\n",sta[j],chan[j],delay[j]);
  }

  dbclose(db);
  fclose(fp);

  return 0;
}
