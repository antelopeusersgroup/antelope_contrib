/*
  This program moves the hypocenter data from HYPODD reloc files to the 
  origin table of the CSS database.  The rms residual RCT is put in an
  origerr table if requested.

Usage:  hypodd2db filename database [origerr flag]

   where

        filename = HYPODD summary file (*.reloc) or file of identical format 
        database = database prefix 
        origerr flag = flag to create origerr table if > 0; default is 0

Note: orid and evid numbers are taken verbatim from the HYPODD output file.
Note: This version supports the 174 byte hypocenter record in the HYPODD
      update which extended the magnitude field to a width of 5 places.

David von Seggern 10/10/2005
modified for 64-bit Antelope 5.2 on Mac  08/31/2012
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "db.h"
#include "stock.h"
#include "coords.h"

int main(int argc, char *argv[])
{
  int		  iret,leap,yr,mon,day,ihr,imn,jday;
  int             ndays[12] = { 1,32,60,91,121,152,182,213,244,274,305,335};
  int             flag,orid,evid,nump,nums,ndef,nass;
  int             ne = 0;
  long int        no,ireturn,jdate;
  double          rlat,rlon,sec,depth,mag,time,sdobs;
  char            *database,*filename;
  char            astring[176];
  FILE            *fptr,*errfptr;
  Dbptr           db,dbo;
/*Following should probably be queried as input.*/
  char		  auth[16] = "unknown";
  char            algorithm[16] = "HYPODD";

  if (argc < 3)
  {
    printf("Usage: hypodd2db hypofile database [origerr flag]\n");
    return 1;
  }

/*printf("%d\n",argc);*/
  filename = argv[1];
  printf("%s\n",filename);
  fptr=fopen(filename,"r");
  database = argv[2];
  iret = dbopen(database,"r+",&db);
  if (iret != 0)
  {
    printf("Could not open database %s\n",database);
    return 1;
  }
  else
  {
    printf("Opened database = %s\n",database);
  }
  if (argc == 4)
    sscanf(argv[3],"%d",&flag);
  else
    flag = 0;

  dbo = dblookup(db,NULL,"origin",NULL,NULL);
  dbquery(dbo,dbRECORD_COUNT,&no);
  printf("# records in origin table = %ld\n",no);
  if (no > 0) 
  {
    printf("Database already has origin records -- stopping.\n");
    return 1;
  }
  fflush(stdout);

/*Open a file for errors.*/
  errfptr=fopen("bad_records","w");

/*HYPODD record is 174 bytes plus 1 for EOL, so ask for 176 to ensure that
  the whole line is read and entered into astring.  fgets adds the null
  character after the EOL character to make 176 bytes*/
  while (fgets(astring,176,fptr) != NULL)
  {
/*  Scan out the required variables.*/
    sscanf(astring+  0,"%9d",&orid);
    sscanf(astring+103,"%4d",&yr);
    sscanf(astring+108,"%4d",&mon);
    sscanf(astring+111,"%2d",&day);
    sscanf(astring+114,"%2d",&ihr);
    sscanf(astring+117,"%2d",&imn);
    sscanf(astring+120,"%6lf",&sec);
    sscanf(astring+ 10,"%10lf",&rlat);
    sscanf(astring+ 21,"%11lf",&rlon);
    sscanf(astring+ 35,"%7lf",&depth);
    sscanf(astring+127,"%5lf",&mag);
    sscanf(astring+138,"%6d",&nump);
    sscanf(astring+144,"%6d",&nums);
/*  Take the number of defining phases to be the sum of P and of S phases; ditto for nass.*/
    ndef = nump + nums;
    nass = ndef;
    if (flag > 0) sscanf(astring+163,"%5lf",&sdobs);

    jday = ndays[mon-1] - 1 + day;
    leap = yr % 4;
    if (leap == 0 && jday >= 61)
      jday = jday + 1;
/*  Special case: March 1 on leap year.*/
    if (leap == 0 && mon == 3 && day == 1)
      jday = 61;
    time = h2e(yr,jday,ihr,imn,sec);
    jdate = yearday(time);
    evid = orid;

    printf("%8d %8d %15.4lf %7ld %4d %03d %02d:%02d:%05.2f %7.4f %9.4f %5.2f %5.2f %4d %4d %s %s\n",
    orid,evid,time,jdate,yr,jday,ihr,imn,sec,rlat,rlon,depth,mag,ndef,nass,auth,algorithm);

/*  Now make the origin table entry.*/

    ireturn = dbaddv(dbo,NULL,"lat",rlat,"lon",rlon,"depth",depth,"time",time,"jdate",jdate,"ml",mag,"algorithm",algorithm,"auth",auth,"orid",orid,"evid",evid,"ndef",ndef,"nass",nass,NULL);
    if (ireturn < 0)
    {
      printf("could not add orid %d to origin table, ireturn = %ld\n",orid,ireturn);
/*    Break to next event if origin table write fails -- write record to
      an error file first. */
      fputs(astring,errfptr);
      continue;
    }

/*  Now make the origerr table entry, if requested.*/

    if (flag > 0)
    {
      if (dbaddv(db, "origerr", "orid", orid, "sdobs", sdobs, NULL) < 0)
      printf("couldn't add %d %d %d %f to origerr table\n", jday,ihr,imn,sec);
    }

    ne = ne + 1;
    if (ne % 100 == 0) printf("# events processed = %7d\n",ne);
  }
  fclose(fptr);
  fclose(errfptr);
  dbclose(db);
  printf("total # events processed = %d\n",ne);
  return 0;
}
