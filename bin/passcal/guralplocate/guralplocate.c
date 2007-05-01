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

Added support for Guralp 6TD (Lee Powell, Univ of Wisc)
	lap070320 - changed usage message to guralplocate
	lap070320 - added 6TDdetect to line parsing
	lap070320 - added GPGGA nema LatLonElev extrat for 6TD
*/

/* fix and parse_nema adapted from
   Author: Bryce Nesbitt
   Website: http://www.obviously.com
*/
struct fix {
    float   ut;
    float   ilat;
    float   ilon;
    int     qual;
    int     nsat;
    float   hdop;
    float   elev;
    float   lat;
    float   lon;
};

int parse_nema( char * , struct fix * );

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
	elog_die(0,"Usage:  guralplocate log1 log2 ...\n");
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
	MW_scalar_statistics latstat,lonstat,heightstat;
	int ngpsfix;
	int gpgga; /* good NEMA GPGGA sentence count */
	struct fix gps;

	elog_init(argc,argv);
	if(argc<2) usage();

/*
	cbanner("1.0","guralplocate log1 log2 ...",
		"Gary L. Pavlis",
		"Indiana University",
		"pavlis@indiana.edu");
*/
	gpsfix_tbl=newtbl(0);
	ngpsfix=gpgga=0;
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
			if(strstr(line,"GPGGA")!=NULL)
			{
				str = strdup(line);
				pushtbl(gpsfix_tbl,str);
				gpgga++;
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
	allot(double *,height,ngpsfix);
	if( gpgga==0 ){
		for(i=0;i<ngpsfix;++i) {
			/* Guralp uses a fixed format string.
				These hold components */
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
	}
	if( gpgga != 0 ) {
		for(i=0,j=0;i<ngpsfix;++i) {
			str = gettbl(gpsfix_tbl,i);
			if(parse_nema(str,&gps)) {
				if( gps.qual != 0 &&
				    gps.nsat > 3 &&
				    gps.elev > 0. ) {
					lat[j]= gps.lat;
					lon[j]= gps.lon;
					height[j]= gps.elev;
					j++;
				}
			}
		}
		ngpsfix=j;
	}
	latstat = MW_calc_statistics_double(lat,ngpsfix);
	lonstat = MW_calc_statistics_double(lon,ngpsfix);
        heightstat = MW_calc_statistics_double(height,ngpsfix);
	fprintf(stdout,"Results from %d GPS fixes\n",ngpsfix);
	printstat("Latitude:  ",latstat);
	printstat("Longitude:  ",lonstat);
	if( gpgga != 0 ){
		printstat("Height(m):  ",heightstat);
	}
}

int parse_nema( char * nema_gps_string, struct fix * a  )
{
char	latdir,londir,eunit;
int     latDeg, lonDeg;
float   latMin, lonMin;

/* Guralp GPGGA 6TD data
$GPGGA,123858.00,1919.3570,N,15514.0853,W,1,08,1.09,00796,M,000,M,,*78
      ,hhmmss.ss,ddmm.mmmm,S,dddmm.mmmm,E,f,#s,hdop,elev,M,geoid,M,age,base
$GPGAA,hhmmss.ss,ddmm.mmmm,n,dddmm.mmmm,e,q,ss,y.y,a.a,z,g.g,z,t.t,iii*CC    
*/
    if( 0 == strncmp( "$GPGGA", nema_gps_string, 6 ) ) {

/*        printf("%s", nema_gps_string); */

        sscanf( nema_gps_string,
                "$GPGGA,%f,%f,%c,%f,%c,%d,%d,%f,%f,%c", 
                &a->ut,&a->ilat,&latdir,&a->ilon,&londir,
		&a->qual,&a->nsat,&a->hdop,&a->elev,&eunit);

        latDeg = (int)(a->ilat/100);
        latMin = (float)(a->ilat - latDeg*100);

        lonDeg = (int)(a->ilon/100);
        lonMin = (float)(a->ilon - lonDeg*100);

        a->lat  = latDeg + (latMin/60);	/* Concert to decimal degrees */

        a->lon = lonDeg + (lonMin/60);	/* Convert to decimal degrees */


        if(latdir == 'S')
            a->lat = -1.0*(a->lat);
        if(londir == 'W')
            a->lon = -1.0*(a->lon);
        if(eunit != 'M') {
            a->elev = 0.0;
	    fprintf(stderr,"parse_nema: elevation expected to be in meters\n");
	}
        return(1);
        }
    fprintf(stderr,"parse_nema: missing GPGGA header\n");
    return(0);
}

