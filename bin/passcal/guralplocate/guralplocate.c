#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "elog.h"
#include "multiwavelet.h"


/* This program scans n input "LOG" files produced by guralp digitizers
an written in log files.  It hunts for lines tagged with "Lat" and "Lon"
and parses these to extract single lat lon estimates.
It then extracts these strings one by one and 
cracks them to produce a set of vectors of doubles.  These
are then passed through a robust statistics estimator to 
produce estimates of the three coordinates.
*/
void printstat(char *label, MW_scalar_statistics s)
{
	fprintf(stdout,"%s",label);
	fprintf(stdout,"%lf %lf (%lf - %lf)  (%lf - %lf)\n",
		s.mean,
		s.median,
		s.q1_4,s.q3_4,
		s.low,s.high);
}
void usage()
{
	elog_die(0,"Usage:  q330gpslocate log1 log2 ...\n");
}

int main(int argc, char **argv)
{
	Tbl *gpsfix_tbl;
	char line[2048];
	char *str;  /* holds dup of lines pushed to tbls */
	double *lat,*lon,*height;
	FILE *fp;
	int i,j,jj;
	char *s_to_crack;
	int is_negative;
	int degfield;
	double minfield;
	int nc;
	MW_scalar_statistics latstat,lonstat;
	int ngpsfix;

	elog_init(argc,argv);
	if(argc<2) usage();

/*
	cbanner("1.0","guralplocate log1 log2 ...",
		"Gary L. Pavlis",
		"Indiana University",
		"pavlis@indiana.edu");
*/
	gpsfix_tbl=newtbl(0);
	for(i=1;i<argc;++i)
	{
		fp = fopen(argv[i],"r");
		if(fp==NULL) elog_die(0,"Open failed on input log file %s\n",
				argv[i]);
		while(fgets(line,2048,fp) != NULL)
		{
			if(strstr(line,"Lat")!=NULL)
			{
				str = strdup(line);
				pushtbl(gpsfix_tbl,str);
				continue;
			}
		}
		fclose(fp);
	}
	
	ngpsfix = maxtbl(gpsfix_tbl);
	if(ngpsfix<=0)
	{
		fprintf(stderr,"Log files contain no gps fix data\nExiting:  no data to process\n");
		exit(-1);
	}
	allot(double *,lat,ngpsfix);
	allot(double *,lon,ngpsfix);
	for(i=0;i<ngpsfix;++i)
	{
		/* Guralp uses a fixed format string.  These hold components */
		char lats1[4],latmin[8];
		char lons1[4],lonmin[8];
		char sign_lat[2],sign_lon[2];
		str = gettbl(gpsfix_tbl,i);
		for(j=0,jj=3;jj<=5;++j,++jj)lats1[j]=str[jj];
		lats1[3]='\0';
		for(j=0,jj=7;jj<=13;++j,++jj)latmin[j]=str[jj];
		latmin[7]='\0';
		for(j=0,jj=22;jj<=24;++j,++jj)lons1[j]=str[jj];
		lons1[3]='\0';
		for(j=0,jj=26;jj<=32;++j,++jj)lonmin[j]=str[jj];
		lonmin[7]='\0';
		sign_lat[0]=str[14];
		sign_lon[0]=str[33];
		lat[i]= atof(lats1)+ atof(latmin)/60.0;
		if(sign_lat[0]=='S') lat[i]=-1.0*lat[i];
		lon[i]= atof(lons1)+ atof(lonmin)/60.0;
		if(sign_lon[0]=='W') lon[i]=-1.0*lon[i];
	}
	latstat = MW_calc_statistics_double(lat,ngpsfix);
	lonstat = MW_calc_statistics_double(lon,ngpsfix);
	fprintf(stdout,"Results from %d GPS fixes\n",ngpsfix);
	printstat("Latitude:  ",latstat);
	printstat("Longitude:  ",lonstat);
}
