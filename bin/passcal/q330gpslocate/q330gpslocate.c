#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "elog.h"
#include "multiwavelet.h"


/* This program scans n input "LOG" files produced by a Q330 
and stored in baler files.  It hunts for lines tagged with 
Latitude, Longitude, and Height and pushes these lines to three
output Tbls.  It then extracts these strings one by one and 
cracks them to produce a set of vectors of doubles.  These
are then passed through a robust statistics estimator to 
produce estimates of the three coordinates.
*/
char *extract_number_string(char *stri)
{
	char *s;
	/* skip the first tokens and use the seconD */
	s = strtok(stri,":");
	s = strtok(NULL,":");
	return(s);
}
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
	Tbl *lat_tbl,*lon_tbl,*height_tbl;
	char line[2048];
	char *str;  /* holds dup of lines pushed to tbls */
	int nlat,nlon,nheight;
	double *lat,*lon,*height;
	FILE *fp;
	int i;
	char *s_to_crack;
	int is_negative;
	int degfield;
	double minfield;
	int nc;
	MW_scalar_statistics latstat,lonstat,heightstat;

	elog_init(argc,argv);
	if(argc<2) usage();

/*
	cbanner("1.0","q300gpsfix log1 log2 ...",
		"Gary L. Pavlis",
		"Indiana University",
		"pavlis@indiana.edu");
*/
	lat_tbl=newtbl(0);  lon_tbl=newtbl(0);  height_tbl=newtbl(0);
	for(i=1;i<argc;++i)
	{
		fp = fopen(argv[i],"r");
		if(fp==NULL) elog_die(0,"Open failed on input log file %s\n",
				argv[i]);
		while(fgets(line,2048,fp) != NULL)
		{
			if(strstr(line,"Latitude")!=NULL)
			{
				str = strdup(line);
				pushtbl(lat_tbl,str);
				continue;
			}
			if(strstr(line,"Longitude")!=NULL)
			{
				str = strdup(line);
				pushtbl(lon_tbl,str);
				continue;
			}
			if(strstr(line,"Height")!=NULL)
			{
				str = strdup(line);
				pushtbl(height_tbl,str);
			}
		}
		fclose(fp);
	}
	
	nlat = maxtbl(lat_tbl);
	nlon = maxtbl(lon_tbl);
	nheight = maxtbl(height_tbl);
	allot(double *,lat,nlat);
	allot(double *,lon,nlon);
	allot(double *,height,nheight);
	/* Now pop off each element of each table, reformat, and 
	computer standard statistics of the results. 
	First Latitude */
	for(i=0;i<nlat;++i)
	{
		str = gettbl(lat_tbl,i);
		s_to_crack = extract_number_string(str);
		/* the string terminates with an S or N + cr and nl.  
		S is - and the magic -3 comes accounts for cr and nl*/
		nc = strlen(s_to_crack);
		if(s_to_crack[nc-3]=='S') 
			is_negative = 1;
		else
			is_negative = 0;
		s_to_crack[nc-3]='\0';
		sscanf(s_to_crack,"%2d%7lf",&degfield,&minfield);
		lat[i] = ((double)degfield) + (minfield/60.0);
		if(is_negative) lat[i] = -lat[i];
	}
	/* now longitude*/
	for(i=0;i<nlon;++i)
	{
		str = gettbl(lon_tbl,i);
		s_to_crack = extract_number_string(str);
		/* the string terminates with an W or E.  W is - */
		nc = strlen(s_to_crack);
		if(s_to_crack[nc-3]=='W') 
			is_negative = 1;
		else
			is_negative = 0;
		s_to_crack[nc-3]='\0';
		sscanf(s_to_crack,"%3d%7lf",&degfield,&minfield);
		lon[i] = ((double)degfield) + (minfield/60.0);
		if(is_negative) lon[i] = -lon[i];
	}	
	/* finally height*/
	for(i=0;i<nheight;++i)
	{
		str = gettbl(height_tbl,i);
		s_to_crack = extract_number_string(str);
		/* the string terminates with an M.  Discard it */
		nc = strlen(s_to_crack);
		s_to_crack[nc-1]='\0';
		sscanf(s_to_crack,"%lf",height+i);
		/* convert to km as that is what is stored in site */
		height[i]*=0.001;
	}
	latstat = MW_calc_statistics_double(lat,nlat);
	lonstat = MW_calc_statistics_double(lon,nlon);
	heightstat = MW_calc_statistics_double(height,nheight);
	printstat("Latitude:  ",latstat);
	printstat("Longitude:  ",lonstat);
	printstat("Height(m):  ",heightstat);
}
