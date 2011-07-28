#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "stock.h"
#include "coords.h"
#include "db.h"
#include "elog.h"
#include "arrays.h"
#include "tr.h"
#include "pf.h"
#include "dbmwspec.h"
#define MINSAMP 16   /* traces shorter than this will be discarded */


/*  dbmwspec

Usage: dbmwspec dbin [-V -pf pffile -s spec_dir  -jack -correct]

This routine uses the multiwindow spectral estimation method (also 
called multitaper method) using a fortran subroutine obtained from
Frank Vernon. This is a second generation program that is a descendent
of an earlier one called "dbspectra".  It differs from the former
largely in the fact that the algorithm used to handle the data is 
far simpler than the earlier program.  That is, here all we do is
join the wfdisc and arrival tables  (Note these are css3.0 schema
specific so this program depends upon css3.0) and process this 
join table.  The program process keys on phase picks so the basic
algorithm is for each line in the recipe file we subset the 
joined wfdisc-arrival table for the key phase and then just loop 
through the subsetted table.  

written:  July 1995

Modified June 2001

Replaced recipe file by a normal parameter file.  Now use a pf tbl
with the same items noted above, but keyed by the parameter name
spectra_windows

Argument list changed also to reflect this.  

Modified Feb 2003
Fairly major changes to change database output form.  Original code
used a "psdisc" table that was special to this program.  That table
had some problems.  BRTT produced a "specdisc" table for another 
project.  To reduce confusion I adapted this program to that structure.
They are extremely similar, but some fairly major modification followed.
The biggest problem is that spectral ratio programs that used to feed
off this table are completely broken because I removed the "sname" 
parameter that was an important key in that processing scheme.  
I did that on purpose because it was a bad scheme.  
This form is better to release to the world.
Modified April 2004
1. Replaced scv2 trace manipulation by tr routine.  This allows reading
miniseed.
2. Implemented -correct option.  Was only stubbed before.
*/


/* functions intimately connected to main */

void usage()
{
  	cbanner("2.0","dbin [-V -pf pffile -s spec_dir -jack -correct]\n",
                    "Gary L. Pavlis",
                    "Indiana University",
                    "pavlis@indiana.edu");
	exit(-1);
}

/* This function is here to reduce repetitious error message fprintf
statements */

void 
dbserr(error, s, c, t, flag)
	char           *error, *s, *c;
	double          t;
	int             flag;	/* if 1 calls clear_register function */
{
	int             year, day, hour, minute;
	double          sec;
	e2h(t, &year, &day, &hour, &minute, &sec);
	elog_notify(0,"WARNING (dbmwspec): %s:%s %d:%d:%d:%d:%lf\n",
		s, c, year, day, hour, minute, sec);
	elog_notify(0, "%s\n", error);
	if (flag)
		elog_clear_register(1);
}
/* This small function returns a tbl of Spectra_phase_specification
structures that define the window and multitaper spectra properties
for each phase.  It functionally replaces and older function
called "read_recipe".  

Written:  June 2001
*/

void parse_spectra_windows(Pf *pf,Tbl **phases)
{
	Spectra_phase_specification *sps;
	int i;
	Tbl *t;
	char *line;
	char phase[20];

	t = pfget_tbl(pf,"spectra_windows");
	*phases = newtbl(0);
	if((*phases)==NULL) elog_die(0,"newtbl for spectra list failed\n");
	for(i=0;i<maxtbl(t);++i)
	{
		double start,end,tbwp;
		line = gettbl(t,i);
		sscanf(line, "%s %s %lg %lg %lf",
		     phase, &start, &end, &tbwp);
		sps = (Spectra_phase_specification *)
			 malloc(sizeof(Spectra_phase_specification));
		if (sps == NULL) 
			elog_die(0,"malloc failed for Spectra_phase_specification structure\n");
		sps->phase_reference = strdup(phase);
		sps->start = start;
		sps->end = end;
		sps->tbwp = tbwp;
		pushtbl((*phases),sps);
	}
}


/* Special function to fix problem with overlapping segments in spectral 
calculations using triggered data.  Sometimes retriggering led to overlapping
wfdisc segments.  We handle this here in a way that is not as robust as
it probably should be, but should work over 99% of the time.  The only way
it would fail is if the time window desired was fairly long and extended
outside the boundaries of the overlap.   In that situation the traces 
could, in principle, be glued back together, but in fact that kind of problem
should be fixed at demux time anyway.  

Arguments:  db - db view that must have been formed as a join of the 
		wfdisc and arrival tables and sorted by sta,time,arrival.time, 
		and wfdisc start time.  Results are unpredictable if this is
		pointed at any other arbitrary view.
	    sps - This stucture specifies the time window around the 
		arrival.time field.  It is used to be sure we keep the 
		most appropriate row of the table 

Function is void because the use of dbmark dbcrunch doesn't
create a new view so the db is altered in place.

Author:  Gary pavlis
Written: July 1995
*/

void fix_overlapping_segments(Dbptr db,Spectra_phase_specification *sps)
{
	int crunch_table=0;  /* Set to 1 if any rows get marked */
	int dups=0;  /* set to 1 whenever duplicate records are found */
	int ilow,ihigh;  /* range or matching records to scan */
	double a_time,time,endtime;
	double alast;
	char sta[8],chan[10],sta_last[8],chan_last[10];
	int nrows;

	dbquery(db,dbRECORD_COUNT,&nrows);
	if(nrows <= 2) return;
	db.record = 0;
	dbgetv(db,0,"sta",sta_last,"chan",chan_last,"arrival.time",&alast,0);

	/* we intentionally start with the second record here for reasons
	that should be obvious */

	for(db.record=1;db.record<nrows;++db.record)
	{
		ilow = (db.record) - 1;
		ihigh = ilow;
		dbgetv(db,0,"sta",sta,"chan",chan,"arrival.time",&a_time,0);
		while( (!strcmp(sta,sta_last)) && (!strcmp(chan,chan_last))
			&& (alast == a_time) && (db.record<nrows) )
		{
			dups = 1;
			crunch_table = 1;
			ihigh = db.record;
			++db.record;
			dbgetv(db,0,"sta",sta,"chan",chan,
					"arrival.time",&a_time,0);
		}
		if(dups)
		{
		/* We hunt for the first complete data segment, and 
		mark the rest to be deleted at the end.  this is
		signaled here by setting dups back to zero once a 
		valid record is found. */
			for(db.record=ilow;db.record <= ihigh;++db.record)
			{
				if(dups)
				{
					dbgetv(db,0,"sta",sta,
						"chan",chan,
						"time",&time,
						"endtime",&endtime,
						"arrival.time",&a_time,0);
					elog_log(0,"Deleting redundant entry for sta:chan %s:%s in dbsubset view\nRows deleted = ",
						sta,chan);
					if( ((a_time+sps->start) >= time)
					   && ((a_time+sps->end) <= endtime) )
					{
						dups = 0;
					}
					else
					{
						dbmark(db);
						fprintf(stdout,"%d,",db.record);
					}

						
				}
				else
				{
					dbmark(db);
					fprintf(stdout," %d",db.record);
				}
			}
			fprintf(stdout,"\n");
			dups = 0;
			db.record = ihigh + 1;
			if(db.record >= nrows) break;
			dbgetv(db,0,"sta",sta_last,"chan",chan_last,
					"arrival.time",&alast,0);
		}
		else
		{
			alast = a_time;
			strcpy(sta_last,sta);
			strcpy(chan_last,chan);
		}
	}
	if(crunch_table) dbcrunch(db);
}
/* small function returns true of the data in the array s 
have a gap defined by Wftype in w.
*/
int trace_has_gap(int n, float *s,Wftype *w)
{
	int i;

	for(i=0;i<n;++i)
		if((s[i]>=(w->upper)) || (s[i]<=(w->lower))) return(1);
	return(0);
}

/* START OF MAIN */

int             jack;		/* jackknife error switch */
int             correct_response;  /* -correct switch */


int main(int argc, char **argv)
{
	char           *dbin, *pffile=strdup("dbmwspec");
	Tbl            *phases;	/* table of pointers to list of windows to
				 * estimate spectra for */
	int             i, j, k;
	int             narr, nsite;

	Dbptr           dbi;	/* db being worked on */
	Dbptr	dball;  /* join of wfdisc and arrival table */
	Dbptr 	dbs;  /* dball subset */

	char           *spec_dir = NULL;
	int             ierr;	/* int return from assorted functions */
	Tbl *sortkeys;
	double pspec_conversion_factor;
	Pf *pf;
	float *seis;
	Wftype *wtype;

	elog_init(argc,argv);
	elog_log(0,"%s\n",argv[0]);


	/* set defaults explicitly here */
	jack = 0;
	correct_response = 0;
	seis=NULL;

	if (argc < 2) {
		usage();
		exit(1);
	}
	dbin = argv[1];
	for (i = 2; i < argc; ++i) {
		if (!strcmp(argv[i], "-s")) {
			++i;
			spec_dir = argv[i];
		} 
		else if (!strcmp(argv[i], "-jack"))
			jack = 1;
		else if (!strcmp(argv[i], "-correct"))
		{
			correct_response = 1;	
		}
		else if(!strcmp(argv[i],"-pf"))
		{
			free(pffile);
			++i;
			pffile=argv[i];
		}
		else if(!strcmp(argv[i],"-V"))
		{
        		cbanner("2.0","dbin [-V -pf pffile -s spec_dir -jack -correct]\n",
                        "Gary L. Pavlis",
                        "Indiana University",
                        "pavlis@indiana.edu");
		}
		else {
			usage();
			exit(1);
		}
	}
	/* initialized at top to detect gaps */
	wtype = trwftype("t4");
	if (spec_dir == NULL)
		spec_dir = strdup("./spectra");
	/* Create spec_dir and die if this can't be done */
	if (makedir(spec_dir) == -1) {
		elog_die(0, "Cannot open spectral output directory %s\n", spec_dir);
	}

	if(pfread(pffile,&pf)) elog_die(0,"pfread error\n");
	parse_spectra_windows(pf,&phases);

	/* Open input database */
	if (dbopen(dbin, "r+", &dbi) == dbINVALID) {
		elog_clear_register(1);
		elog_die(0, "dbmwspec: Unable to open database '%s'.\n", dbin);
	}
	/* join the full wfdisc and arrival tables.  We keep the pointer
	because we reuse it repeatedly in the loop below */
	dbi = dbjoin( dblookup(dbi,0,"wfdisc",0,0),
			dblookup(dbi,0,"arrival",0,0), 0, 0, 0, 0, 0);
	dbi = dbjoin( dbi,dblookup(dbi,0,"sensor",0,0), 0, 0, 0, 0, 0);
	dbi = dbjoin( dbi,dblookup(dbi,0,"instrument",0,0),  0, 0, 0, 0, 0);
	dbi = dbjoin( dbi,dblookup(dbi,0,"affiliation",0,0),  0, 0, 0, 0, 0);

	/* Now we have to sort the db.  This is necessary to fix the 
	case of overlapping waveform segments (see below).  I'm sorting
	here under an assumption that normally this code woudl process 
	the following loop several times.  Resorting repeatedly in that
	case would be slower. */
	sortkeys = newtbl(4);
        pushtbl(sortkeys,"sta");
        pushtbl(sortkeys,"chan");
        pushtbl(sortkeys,"arrival.time");
        pushtbl(sortkeys,"time");
        dball = dbsort(dbi,sortkeys,0,0);
	/* the previous view was pretty large, so we should free it up */
/*
	dbfree (dbi);
	freetbl(sortkeys,free);
*/
	
	dbquery(dball,dbRECORD_COUNT, &narr);
	if(narr<=0) elog_die(0,"Working view has no data after dbjoins\n");
	fprintf(stdout,"dbmwpec:  wfdisc->arrival->sensor->instrument->affiliation join of %s has %d rows\n",
				dbin,narr);

	/* Now we loop through the recipe file specification and 
	subset the db for each entry in the parameter file phase specification */
	for (i = 0; i < maxtbl(phases); ++i) {
		Dbptr           db_this_phase;	/* db subset of picks of
						 * current phase */
		Spectra_phase_specification *this_phase;
		int nrecords;
		char string[32];

		/* Extract entries in arrival table that match current phase */
		this_phase = (Spectra_phase_specification *) gettbl(phases, i);
		sprintf(string, "iphase =~ /%s/", this_phase->phase_reference);
		db_this_phase = dbsubset(dball, string, 0);
		if (dbi.record == dbINVALID) {
			elog_notify(0, "dbmwspec:  db error parsing phase &s\n",
				this_phase->phase_reference);
			continue;
		}
		fprintf(stdout,"Processing phase %s for interval %lf to %lf\n",
			this_phase->phase_reference,this_phase->start,this_phase->end);

		/* This routine takes the sorted, subsetted db and 
		deletes rows where waveform segments overlap.  It does
		this with some wisdom to make assure the segment retained
		has will work with the given time window.  This is why
		we pass the this_phase structure*/

		fix_overlapping_segments(db_this_phase,this_phase);

		dbquery(db_this_phase, dbRECORD_COUNT, &nrecords);
		fprintf(stdout,"This should yield %d spectral estimates\n",nrecords);

		for (db_this_phase.record = 0; db_this_phase.record < nrecords;
			++db_this_phase.record) 
		{
			/* These are variables extracted from db */

			double calib,samprate,time,endtime;
			int foff;
			char sta[7],chan[9];
			double pick,azimuth;
			int arid;
			/* Other necessary variables */
			double tstart,tend;  /* time window for spectra*/
			float xi,seavg,dt;
			int nfft,ntest,ierr; /* variables used by powspc*/
			int div; /* used in calculating nfft*/
			float *freq,*spec,*errspc;  /* arrays used by powspc*/
			double tsread,teread;
			int npts;

			/* other variables */
			int nsamples;

			dbgetv(db_this_phase, 0,
				"sta",sta,
				"chan",chan,
				"time",&time,
				"endtime",&endtime,
				"samprate",&samprate,
				"calib",&calib,
				"arrival.time",&pick,
				"arid",&arid,
				"azimuth",&azimuth,
							0);
			/* calculate the start and end time of the window
			to derive the spectra from, and then read in this
			time window */
			tstart = pick + this_phase->start;
			tend = pick + this_phase->end;
			nsamples = rint ( (tend - tstart)*samprate);
			if(seis==NULL) 
				allot(float *,seis,nsamples);
			if( (tstart < time) || (tend > endtime) )
			{
				dbserr("Spectra window does all contained in waveform segment",
					sta,chan,pick, 0);
				elog_notify(0,"Data skipped\n");
				continue;
			}
			ierr=trgetwf(db_this_phase,0,&seis,&nsamples,tstart,tend,
				&tsread,&teread,&npts,0,0);
			if(ierr)
			{
				trgetwf_error(db_this_phase,ierr);
				elog_clear_register(1);
				continue;
			}
			/* delete traces with gaps */
			if(trace_has_gap(npts,seis,wtype))
			{
				elog_log(0,"%s %s %lf has data gap. Data skipped\n",
					sta,chan,pick);
				continue;
			}
			if(calib>0.0)
				for(i=0;i<npts;++i) seis[i]*=calib;
			else
				elog_complain(0,"Null or negative calib (%lf) for %s %s %lf\nCalib not applied\n",
					calib,sta,chan,pick);
			/* Drop absurdly short traces */
			if(npts<MINSAMP)
			{
				dbserr("Absurdly short trace", sta,chan,pick,0);
				elog_notify(0,"Length of in window = %d\nSkipped\n",
					npts);
				continue;
			}
				 

			/* set up variables required by powspc*/
			dt = (float) (1.0 / samprate);
			/*
			 * Establish fft size.  Here I always scale
			 * up to approximately 2* nearest power of 2.
			 * This assures reasonably fast fft
			 * calculations and removes burden of worry
			 * about efficiency of number of samples from
			 * the user
			 */

			nfft = 1;
			div = npts;
			do {
				div /= 2;
				nfft *= 2;
			} while (div > 0);
			/* powspc request min 2*npts */
			nfft *= 2;
			freq = (float *) calloc(nfft / 2 + 1, sizeof(float));
			spec = (float *) calloc(nfft / 2 + 1, sizeof(float));
			errspc = (float *) calloc(nfft / 2 + 1, sizeof(float));
			if (npts > MAXSAMPLES) {
				dbserr("Window has too many samples:  trimmed to MAXSAMPLES",
				       sta, chan, pick,  0);
			}
			powspc_(&npts, seis, &nfft, &dt,
				&(this_phase->tbwp), freq,
			   spec, errspc, &xi, &seavg, &jack, &ierr);


			switch (ierr) {
			case (0):
				break;
			case (1):
				dbserr("powspc array size too small",
				       sta, chan, pick,
				     0);
				break;
			case (-1):
				dbserr("powspc fft size increased automatically",
				       sta, chan, pick,  0);
				break;

			case (-2):
				dbserr("powspc number of spectra reduced",
				       sta, chan, pick,  0);
				break;
			case (2):
				dbserr("powspc error while computing dpps",
				       sta, chan, pick,  0);
				break;
			}

			if (ierr > 0) {
				free(freq);
				free(spec);
				free(errspc);
				continue;
			}


			/* We need to rescale spectra to be in
			seismological conventions used for determination
			of Mw.  This magic formula is from Frank Vernon's
			original code after it called powspc_ */

			pspec_conversion_factor = (double)npts*dt*dt/2.0;
			for(j=0;j<(nfft/2 +1);++j) 
			{
				spec[j] *= pspec_conversion_factor;
				errspc[j] *= pspec_conversion_factor;
			}
			
			/*
			 * Here I need to add a routine to correct
			 * for system response function
			 */

			if (correct_response) {
				if (ierr = correct_for_response(freq, spec,
					  (nfft / 2 + 1), db_this_phase)) {
					if(ierr<0)
					{
						elog_complain(0,"correct_response failed completely for %s %s %s\n",
							sta,chan,pick);

					}
					else if(ierr> 0)
					{
						elog_complain(0,"%d eval_response errors for %s %s %s\n",
							sta,chan,pick);
					}
				}
			}
 

			/*
			 * This routine writes spectral files and
			 * updates the db
			 */
			switch (save_spectrum(db_this_phase, this_phase, spec, errspc, 
			    nfft,npts, dt, pick, spec_dir, correct_response, jack)) {
			case (0):
				break;
			case (-1):
				dbserr("FATAL ERROR writing spectral output file",
				       sta, chan, pick,
				     0);
				exit(1);

			case (-2):
				dbserr("FATAL ERROR with dbput on specdisc table for spectral file",
				       sta, chan, pick,
				     1);
				exit(1);

			case (-3):
				dbserr("FATAL ERROR writing jackknife error file",
				       sta, chan, pick,
				     0);
				exit(1);

			case (-4):
				dbserr("FATAL ERROR with dbput on specdisc table for error file",
				       sta, chan, pick,
				     1);
				exit(1);
			case (-5):
				dbserr("Error in save_spectrum dbgetv failure in parameter lookup\n",
				       sta, chan, pick,
				     1);
				break;

			default:
				dbserr("Unknown error code from save_spectrum",
				       sta, chan, pick,
				     0);
			}
			fprintf(stdout,"%10.10s %10.10s %10.10s %s\n",
				sta,chan,this_phase->phase_reference,
				epoch2str(pick,"%y%j%H%M"));
			free(freq);
			free(spec);
			free(errspc);
		}
		db_this_phase.field = dbALL;
		db_this_phase.record = dbALL;
		dbfree(db_this_phase);

	}
	return(0);
}
