/*
Program to create the HYPODD ".cc" file from correl output.

Usage: 
  create_ccfile inputfile outputfile cccmin iflag dtchop spdifmin minobs

  inputfile -- cross-correlation (dbcorrelate) output file
  outputfile -- HYPODD cross-cor input file, usually "dt.cc"
  cccmin -- minimum cross-correlation coef. value to accept (absolute value)
            (~ 0.6 to 0.7 normally)
  iflag --  0 means use both neg. and pos. xcor's; 1 mean use only pos.
  dtchop -- do not write dt values whose absolute value exceeds dtchop.
            0.0 means ignore this criterion.  A chop value of around 0.5
            would be reasonable.
  spdifmin -- if both P and S xcor's present, they are both rejected if the
           difference is > spdifmin (~0.1 s would normally be good)
  minobs -- minimum # of xcor's for any event pair; otherwise all rejected
            (1 may be OK if xcor times are used in conjunction with normal
            travel time differences in HYPODD for instance)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#define maxrows 1000 /* maximum number of ccc's for an event pair*/

int main(int argc, char *argv[])
{

  char   filename[80],astring[200];
  char   asta[7],sta[maxrows][7];
  char   apha[2],pha[maxrows][2];
  double atdif,otdif,ttdif,tdif,dt;
  double tt[maxrows],to[maxrows];
  float  r[maxrows],cccmin,ccc,cccp,dtchop,spdifmin;
  int    irec,iret,np,i,k,na,iflag,ifirst,orid1,orid2,orid1last,orid2last,minobs;
  int    jdate1,jdate2;
  FILE   *ifileptr,*ofileptr;

  if (argc < 8)
  {
    printf("usage: create_ccfile inputfile outputfile cccmin iflag dtchop spdifmin minobs\n");
    return 0;
  }

  strcpy(filename,argv[1]);
  if ((ifileptr = fopen(filename,"r")) == NULL)
  {
    printf("Error: Cannot open input file\n");
    return 1;
  }
  printf("opened input file\n");

  strcpy(filename,argv[2]);
  if ((ofileptr = fopen(filename,"w")) == NULL)
  {
    printf("Error: Cannot open input file\n");
    return 1;
  }
  printf("opened output file\n");

  sscanf(argv[3],"%f",&cccmin);
  sscanf(argv[4],"%d",&iflag);
  sscanf(argv[5],"%f",&dtchop);
  sscanf(argv[6],"%f",&spdifmin);
  sscanf(argv[7],"%d",&minobs);

  np=0;	/*event pair counter*/
  k=-1;
  irec=0;
  ifirst=1;

/*Read through dbcorrelate output file, outputting cc file info.*/
  while (fgets(astring,200,ifileptr) != NULL)
  {
    irec++;
    iret = sscanf(astring,"%6s %1s %8d %7d %8d %7d %7f %10f %14lf %14lf %14lf",
    asta,apha,&orid1,&jdate1,&orid2,&jdate2,&ccc,&cccp,&ttdif,&otdif,&atdif);
/*  The dbcorrelate code may have generated some NaN values in ttdif.
    Here is a trick to catch NaN (and -NaN) values. */
    if (iret < 11) 
    {
      printf("bad scan on record # %d\n",irec);
      continue;
    }
    if ((orid1 == orid1last && orid2 == orid2last) || ifirst)
    {
check_ccc:
      if ((iflag == 0 && fabs(ccc) > cccmin) || (iflag == 1 && ccc > cccmin))
      {
/*      Continue to save info.*/
        k=k+1;
        strcpy(sta[k],asta);
        strcpy(pha[k],apha);
        tt[k]=ttdif;
        to[k]=otdif;
        r[k]=ccc;
        ifirst=0;
        orid1last=orid1;
        orid2last=orid2;
      }
      continue;
    }

/*  Read all stations for event pair, so dump info.*/
    np=np+1;
    na=k+1;
    if (na == 0) goto check_ccc;
    if (na < minobs) goto check_ccc;
    printf("event pair # %d ,orid1,orid2, # arrivals = %d %d %d\n",np,orid1last,orid2last,na);
/*  Write the event header record.*/
    fprintf(ofileptr,"#%8d%8d 0.0\n",orid1last,orid2last);
    for (i=0;i<na;i++)
    {
/*    Do a consistency check if there is a P-S pair. The computed event time 
      differences should be very close.*/
      if (i > 0 && (strcmp(sta[i],sta[i-1]) == 0) && (strcmp(pha[i],"S") == 0) && (strcmp(pha[i-1],"P") == 0))
      {
        tdif = tt[i]-tt[i-1];
        if (fabs(tdif) > spdifmin) continue;
      }
      dt = -tt[i] + to[i];
/*    If too large, skip writing.*/
      if (dtchop != 0.0)
      {
        if (dt < -dtchop || dt > dtchop) continue;
      }
/*    Rectify the ccc's so that all weights are positive.*/
      r[i] = fabs(r[i]);
      fprintf(ofileptr,"%-6s %7.3f%6.3f %1s\n",sta[i],dt,r[i],pha[i]);
    }
/*  Return to processing this event pair.*/
    orid1last=orid1;
    orid2last=orid2;
    k=-1;
    goto check_ccc;
  } /*end loop on input file*/

/*Write last event pair info (if any exists).*/
  np=np+1;
  if (k == 0) goto finish;
  if (k < minobs) goto finish;
  na=k+1;
  printf("event pair # %d ,orid1,orid2, # arrivals = %d %d %d\n",np,orid1last,orid2last,na);
/*Write the event header record.*/
  fprintf(ofileptr,"#%8d%8d 0.0\n",orid1last,orid2last);
  for (i=0;i<na;i++)
  {
/*  Do a consistency check if there is a P-S pair. The computed event time 
    differences should be very close.*/
    if (i > 0 && (strcmp(sta[i],sta[i-1]) == 0) && (strcmp(pha[i],"S") == 0) && (strcmp(pha[i-1],"P") == 0))
    {
      tdif = tt[i]-tt[i-1];
      if (fabs(tdif) > spdifmin) continue;
    }
/*  If dt too large, skip writing.*/
    dt = -tt[i] + to[i];
    if (dtchop != 0.0)
    {
      if (dt < -dtchop || dt > dtchop) continue;
    }
/*  Rectify the ccc's so that all weights are positive.*/
    r[i] = fabs(r[i]);
    fprintf(ofileptr,"%-6s %7.3f%6.3f %1s\n",sta[i],dt,r[i],pha[i]);
  }

finish:
  fclose(ifileptr);
  fclose(ofileptr);
}
