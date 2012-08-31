/* 
This program will compute the relative distances and azimuths among array
elements.

Usage: distaz arrayname dbname

where

  arrayname = name of array
  dbname = database name

Note: ".arr" is appended in program to arrayname to make file name for file
      containing array parameters.

Two output files contain 2 sets of data, each having one row per station.
1 - distances among stations
2 - back azimuths from station to station

Each is a symmetric matrix.  To be precise, the azimuths should be computed
for forward and backward between stations; however, it is assumed that the 
stations occupy such a small area that the reverse azimuth between stations
is simply 180 degrees different from the forward azimuth.

*/

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "tttaup.h"

#define MXSTA 100
#define rad2dg 57.2958
#define dg2km 111.19
#define pi 3.14159

int main(int argc, char *argv[])
{
  int    nsta,i,j;
  double lat[MXSTA],lon[MXSTA],distance,azimuth,rx[MXSTA],ry[MXSTA];
  double reflat,reflon;
  char   sta[MXSTA][7];
  char   *arrayfile,*arrayname,*dbname,*filename;
  Dbptr  db,dbs;
  FILE   *fp;
 
  arrayname = malloc(9);
  arrayfile = malloc(13);
  dbname    = malloc(80);
  filename  = malloc(80);

  if (argc < 3) 
  {
    printf("Usage: %s arrayname dbname\n",argv[0]);
    return 1;
  }

  arrayname = argv[1]; 
  dbname = argv[2];

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
  while(fscanf(fp, "%s", sta[nsta]) != EOF)
  {
    nsta = nsta + 1;
  }
  printf("# stations = %d\n",nsta);
  fclose(fp);

/*Open the database and get required info on the array.*/

  if (dbopen(dbname,"r",&db) < 0)
  {
    complain(0,"Could not open database.\n");
    return 1;
  }
  dbs = dblookup(db,0,"site",0,0);

/*Get the latitudes and longitudes.*/
  reflat = 0.0;
  reflon = 0.0;
  for (j=0;j<nsta;j++)
  {
    dbs=dblookup(dbs,0,0,"sta",sta[j]);
    dbgetv(dbs,0,"lat",&lat[j],"lon",&lon[j],NULL);
    reflat = reflat + lat[j];
    reflon = reflon + lon[j];
  }
/*Reference lat and lon are simply the means.*/
  reflat = reflat/nsta;
  reflon = reflon/nsta; 
  printf("reflat,reflon = %f %f\n",reflat,reflon);

/*Compute station locations in a Cartesian system.*/
  for (j=0;j<nsta;j++)
  {
/*  Note: dist returns distance in radians and azimuth in radians.*/
    dist(reflat/rad2dg,reflon/rad2dg,lat[j]/rad2dg,lon[j]/rad2dg,&distance,&azimuth);
    distance = distance*rad2dg*dg2km;
    rx[j]=distance*cos(pi/2 - azimuth);
    ry[j]=distance*sin(pi/2 - azimuth);
  }

/*Compute interstation distances and azimuths and write to output file as
  matrices.*/

  memcpy(filename,"\0",1);
  strcat(filename,arrayname);
  strcat(filename,"_");
  strcat(filename,"distances.dat");
  printf("filename = %s\n",filename);
  fp = fopen(filename,"w");
  for (j=0;j<nsta;j++)
  {
    fprintf(fp,"%-4s",sta[j]);
    for (i=0;i<nsta;i++)
    {
      if (i == j)
      {
        distance = 0.0;
      }
      else
      {
        distance = sqrt((rx[j]-rx[i])*(rx[j]-rx[i])+(ry[j]-ry[i])*(ry[j]-ry[i]));
      }
      fprintf(fp," %5.2f",distance);
    }
    fprintf(fp,"\n");
  }
  fclose(fp);

  memcpy(filename,"\0",1);
  strcat(filename,arrayname);
  strcat(filename,"_");
  strcat(filename,"backazimuths.dat");
  printf("filename = %s\n",filename);
  fp = fopen(filename,"w");
  for (j=0;j<nsta;j++)
  {
    fprintf(fp,"%-4s",sta[j]);
    for (i=0;i<nsta;i++)
    {
/*    Compute the geographic azimuth -- degrees east of north.*/
      if (i == j)
      {
        azimuth = 0.0;
      }
      else
      {
        azimuth = 90 - rad2dg*atan2(ry[j]-ry[i],rx[j]-rx[i]);
        if (azimuth < 0.0) azimuth = azimuth + 360.0;
      }
      fprintf(fp," %5.0f",azimuth);
    }
    fprintf(fp,"\n");
  }    
  fclose(fp);

/*End main routine.*/
}
