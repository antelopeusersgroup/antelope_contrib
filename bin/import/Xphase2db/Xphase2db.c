/* This is a special purpose program to reformat the output of Xphase 
written by Steve Roecker as a css3.0 arrival table.  Cracking the time
line and deltim is simple, but mapping the station and channel codes from
a file name is problematic.  I use a parameter file assume the station 
and channel codes are fields in a string of characters separated by 
a unique character that can be parsed with strtok.  

Written:  April 2000
Author:  G Pavlis

Usage:  Xphase2db dbout

Reads input from stdin and stored to dbout.arrival table using
datascope dbaddv.  Pretty much css3.0 specific.  

Parameter file Xphase2db.pf is required, and is frozen.
*/
#include <stdio.h>
#include <strings.h>
#include "coords.h"
#include "elog.h"
#include "db.h"
#include "pf.h"
int main(int argc, char **argv)
{
	Dbptr db;
	char *dbname;
	Pf *pf;
	char *progname="Xphase2db";
	char line[1024];
	int year,jday,hour,min;
	double sec;
	double twin,deltim;
	int other1,other2,pol;
	char fname[512];
	char tstring[100];
	double time;
	char *seperators;  /* holds seperators passed to strtok*/
	int ista,ichan;  /* token positions of station and channel 
			strings */
	char *phase;
	char *fm,*up="uc",*down="dr";
	char *token;
	char *sta,*chan;
	int i,j;


	if(argc < 2) elog_die(0,"usage:  %s dbout [-phase x]\n",progname);
	if(argc == 4)
	{
		if(!strcmp(argv[2],"-phase"))
			elog_die(0,"usage:  %s dbout [-phase x]\n",progname);
		phase = argv[3];
	}
	else
		phase = strdup("P");
	dbname = argv[1];
	if(pfread(progname,&pf))
		elog_die(0,"Failure reading parameter file %s.pf\n",
				progname);
	if(dbopen(dbname,"r+",&db) == dbINVALID)
		elog_die(0,"dbopen failure for database %s\n",dbname);
	db = dblookup(db,0,"arrival",0,0);
	seperators = pfget_string(pf,"seperators");
	if(seperators == NULL) elog_die(0,"required parameter separators no in parameter file\n");
	ista = pfget_int(pf,"station_token_number");
	ichan = pfget_int(pf,"chan_token_number");

	while( fgets(line,1024,stdin) != NULL)
	{
		sscanf(line,"%d%d%d%d%lf%d%lf%lf%d%d%s",
			&year,&jday,&hour,&min,
			&sec,&other1,&twin,&deltim,&other2,&pol,fname);

		if(pol<0)
			fm = down;
		else
			fm = up;
		sprintf(tstring,"%4d%3d:%2d:%2d:%7.4lf",year,jday,hour,min,sec);
		/* There is a way to avoid this in formats for sprintf, but
		I'll be damned if I can find it.  Easier to do by brute force*/
		for(j=0;j<strlen(tstring);++j) 
			if(tstring[j]==' ')tstring[j]='0';
		time = str2epoch(tstring);
		token = strtok(fname,seperators);
		i=1;
		while(token != NULL)
		{
			if(i == ista)  sta = token;
			if(i == ichan) chan = token;
			++i;
			token = strtok(NULL,seperators);
		}
		dbaddv(db,0,
			"sta",sta,
			"chan",chan,
			"time",time,
			"deltim",deltim,
			"iphase",phase,
			"fm",fm,
			"auth","Xphase",
			0);
	}
}
