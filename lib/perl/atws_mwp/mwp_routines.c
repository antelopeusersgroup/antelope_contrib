#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "db.h"
#include "tr.h"
#include "pf.h"
#include "tttaup.h"
#include "response.h"
#include "coords.h"
#include "stock.h"

extern int filter3c (), num_traces ();
extern int rot (), rfcn_calc (), trsubset (), trdecimate ();
extern int fill (Dbptr *tr, Trsample *data, int *i0p, int *i1p, int *imaxp);
int makeMwp (float *x,		/* The "raw" data: a pointer to an arry of integers.             */
	     int nsamp,		/* Number of samples in the data array                           */
	     double sample_rate,	/* Number of samples per second.                                 */
	     double du_per_meter_per_sec,	/* GAIN FACTOR: Digital Units ==> Velocity (meters/second).      */
	     double pre_event_offset,	/* Mean of 300 secs of noise ending 60 secs before p-wave arrives */
	     double epic_dist,	/* Epicentral distance of this site from the earthquake.         */
	     double *moment,	/* The moment array.                                             */
	     double *Mw);	/* The Magnitude array.                                          */

int num_traces (Dbptr tr);
int taper (int nSamples, double *DataArray);

static int trace = 0;

int mwp_prepare (Dbptr db, int orid, char* sta, double stime, double etime, float **rdata, double **rmag, double **rmom)
	{
	Pf *pf;
	char pfile[132];
	void *pfstr;
	char *oridstr, *aridstr, *evidstr, yesno;
	char /**dbname,*/ *output_database;
	char line[200];
	Dbptr dbo, dbs, dbwf, dbsc, tr, dbout, dbarr, dbar2, dbar3, dbar4, dbar5, dbar6, db_calib, bundle;
	int bs, be, rs, re, nsamp, bundletype, nwf;
	double delta, dt, calib = 0., calibdb = 0.;
	double samprate;
	float offset;
	float *data;
	double *moment, *magnitude;
	Tbl *stachan;
	int size2alloc;
	double dist;
	int ok;
	char time_str[100], endtime_str[100];
	unsigned char i;
	char sta_name[10];
	char chn_name[10];

	printf("In mwp_prepare for %s\n", sta);

	db_calib = dblookup(db, 0, "calibration", 0, 0);
	dbarr = dblookup(db, 0, "assoc", 0, 0);
	dbar2 = dblookup(db, 0, "arrival", 0, 0);
	sprintf(line, "(sta == \"%s\" && orid == \"%i\")", sta, orid);
	dbar3 = dbsubset(dbarr, line, 0);
	dbar4 = dbjoin(dbar3, dbar2, 0, 0, 0, 0, 0);
	dbquery (dbar4, dbRECORD_COUNT, &nwf);
	dbar4.record = 0;
	dbgetv(dbar4,0, "sta",  sta_name, "chan", chn_name, "delta", &dist, 0);

//	printf("%s:%i\n", __FILE__, __LINE__);

	sprintf(line, "(sta == \"%s\" && chan == \"%s\" && time < %.2f && endtime > %.2f)", sta_name, chn_name, stime, stime);
	printf("Doing Calib_Db Subset: %s\n", line);
	dbar6 = dbsubset(db_calib, line, 0);

	dbquery (dbar6, dbRECORD_COUNT, &nwf);
	calibdb = 0.;
	if(nwf)
		{
		dbar6.record = 0;
        	dbgetv(dbar6,0, "calib",  &calibdb, 0);
		printf("Got DB Calib Row: %.5f\n", calibdb);
		}
	else
		{
		printf("No Calibration record found for %s:%s\n", sta_name, chn_name);
		}
	
	/* subset the time window */
	sprintf (line, "(time < %f && endtime > %f)", stime, etime);

	dbwf = dbsubset (db, line, 0);
	dbquery (dbwf, dbRECORD_COUNT, &nwf);
//	printf ("%d records after time subset...\n ", nwf);
	if (nwf < 1)
		{
		printf (" No unique data\n");
		return 0;
		}


//	printf("%s:%i\n", __FILE__, __LINE__);

	/* Make trace database */
	tr = dbinvalid ();
	sprintf (time_str, "(%.5f)", stime);
	sprintf (endtime_str, "(%.5f)", etime);

	if (trload_css (db, time_str, endtime_str, &tr, 0, 0) < 0)
		elog_die(0, "Problems loading traces\n");

//	printf("%s:%i\n", __FILE__, __LINE__);

	trsplit (tr, 0, 0);
	trsplice (tr, 1.0, fill, 0);
	stachan = strtbl ("sta", "chan", 0);
	tr = dbgroup (tr, stachan, 0, 1);

	//printf ("after trgroup %d traces\n", num_traces (tr));

	dbquery (tr, dbRECORD_COUNT, &nwf);

	//printf ("Getting data into array...\n");
	trdemean (tr);

	dbget_range (tr, &rs, &re);
	for (tr.record = rs; tr.record < re; tr.record++)
		{
		//printf("Doing TR: %i\n", tr.record);
		dbgetv (tr, 0, "bundle", &bundle, 0);
		dbget_range (bundle, &bs, &be);
		for (bundle.record = bs; bundle.record < be; bundle.record++)
			{
			//printf("Doing Bundle: %i\n", bundle.record);
			dbgetv (bundle, 0, "bundletype", &bundletype, 0);
			if (bundletype != 0)
				elog_die(0, "bundletype != 0");

			if (dbgetv (bundle, 0, "data", &data, "nsamp", &nsamp, "samprate", &samprate, "calib", &calib, 0) != 0)
				elog_die(0, "dbgetv data problem\n");

			delta = 1. / samprate;
			}
		}

	//for(i = 0; i < nsamp; i++)
	//{
		//data[i]*=calib;
		//if(i % 5 == 0)
		//	printf ("\n");
	//}

	moment = (double *) malloc (nsamp * sizeof (double));
	magnitude = (double *) malloc (nsamp * sizeof (double));

	printf("Calib: %f DBCalib: %f\n", calib, calibdb);
	//calib *= 1.e-7;
	//printf("Calib: %f\n", calib);

	if(calibdb < 0.0000000001)
		calibdb = calib;
//	printf("%s:%i\n", __FILE__, __LINE__);

	ok = makeMwp (data, nsamp, samprate, calibdb, offset, dist, moment, magnitude);
//	printf("%s:%i\n", __FILE__, __LINE__);


	*rmag = magnitude;
	*rmom = moment;
	*rdata = data;

	//printf ("Trace %d\n", trace++);
	return (nsamp);
	}


/***************************************************************************************************************/
/*								       					       */
/*  	        CALCULATE MWP FOR EACH SAMPLE OF AN ARRAY OF "BHZ" DATA.				       */
/*		BASED ON PAPER OF TSUBOI ET AL. BSSA 1995	       					       */
/***************************************************************************************************************/

int
	makeMwp (float *x,	/* The "raw" data: a pointer to an arry of integers.             */
	int nsamp,			/* Number of samples in the data array                           */
	double sample_rate,	/* Number of samples per second.                                 */
	double du_per_nm_per_sec,	/* GAIN FACTOR: Digital Units ==> Velocity (meters/second).      */
	double pre_event_offset,	/* Mean of 300 secs of noise ending 60 secs before p-wave arrives */
	double epic_dist,	/* Epicentral distance of this site from the earthquake.         */
	double *moment,		/* The moment array.                                             */
	double *Mw)			/* The Magnitude array.                                          */
	{
	int j;				/* Loop index.                                                   */
	double dt;			/* Interval (seconds) between each sample point.                 */
	double MomentFactor;
	double *veloc;		/* An array of velocities.                                    */
	double *displ;		/* An array of displacements.                                 */
	double *intdispl;	/* An array of integrated displacements.                              */
	double PI = 3.1415;
	double DENSITY = 3400.0;
	double Vp = 7.9;
	unsigned char i;

	/* PROTOTYPES */
	int taper (int, double *);

	veloc = (double *) malloc (nsamp * sizeof (double));
	displ = (double *) malloc (nsamp * sizeof (double));
	intdispl = (double *) malloc (nsamp * sizeof (double));


	//du_per_cm_per_sec = du_per_meter_per_sec; // / ((double) 100.0);



	for (j = 0; j < nsamp; j++)
		{
		if(isnan(x[j]))
			x[j] = 0.0e10;

		x[j] *= du_per_nm_per_sec;


		veloc[j] = (x[j] * 1.e-7) * 300.0; // FUDGE FACTOR
		}


	/* TAPER THE VELOCITY DATA. */
	if (!taper (nsamp, veloc))
		{
		printf ("\n[makeMwp]: UNABLE TO TAPER THE VELOCITY DATA ARRAY\n\n");
		return (0);
		}

	dt = 1.000 / sample_rate;
	/* INTEGRATE VELOCITY TO DISPLACEMENT: */
	displ[0] = (dt / 2.0000) * (veloc[0] + veloc[1]);
	for (j = 1; j < nsamp - 1; j++)
		displ[j] = displ[j - 1] + ((dt / 2.0000) * (veloc[j] + veloc[j + 1]));
	displ[nsamp - 1] = displ[nsamp - 2] + ((dt / 2.0000) * (veloc[j] + veloc[j + 1]));

	/* TAPER THE DISPLACEMENT DATA: */
	if (!taper (nsamp, displ))
		{
		printf ("\n[makeMwp]: UNABLE TO TAPER THE DISPLACEMENT DATA ARRAY\n\n");
		return (0);
		}

	/* INTEGRATE TO INTEGRATED DISPLACEMENT: */
	intdispl[0] = (dt / 2.0000) * (displ[0] + displ[1]);
	for (j = 1; j < nsamp - 1; j++)
		intdispl[j] = intdispl[j - 1] + ((dt / 2.0000) * (displ[j] + displ[j + 1]));
	intdispl[nsamp - 1] = intdispl[nsamp - 2] + ((dt / 2.0000) * (displ[j] + displ[j + 1]));

	/* OUTPUT THE MOMENT RATE, Mo(t) */
	MomentFactor = 4.0 * PI * DENSITY * (Vp * Vp * Vp * 1000000000.);

	for (j = 0; j < nsamp; j++)
		{
		moment[j] = MomentFactor * epic_dist * 1000.0 * fabs (intdispl[j] / 100.0);
		Mw[j] = (log10 (moment[j]) - 9.1) / 1.5;	/* Mw(t) FROM Mo(t) aka. KANAMORI, 77: */
		Mw[j] = (Mw[j] - 1.03) / 0.843;	/* 4/29/02: bfh. APPLY WC/ATWC's CORRECTION */
		}

	free (veloc);
	free (displ);
	free (intdispl);
	return (1);
	}


/***************************************************************************************************************/
/* 													       */
/*    APPLY A COSINE TAPER TO A DATA ARRAY.       							       */
/*     													       */
/***************************************************************************************************************/

int taper (int nSamples, double *DataArray)
	{
	double angle = 0.0;		/* Dummy argument for the taper function: cs.       */
	double cs = 0.0;		/* The taper function.        */
	double start = 0.05;		/* Fractional part of the data to apply the STARTING taper to. */
	double end = 0.05;		/* Fractional part of the data to apply the ENDING taper to.  */
	long n1 = 0, n2 = 0, n3 = 0, n4 = 0, n5 = 0;	/* Pre-defined "jumps" in the taper (cosine) function's domain */
	long m = 0;			/* LOOP index.       */
	long xi = 0;			/* Dummy argument for the taper function: cs.        */
	double PI = 3.14;
	n1 = start * nSamples;
	n2 = n1 + 1;

	angle = PI / ((double) n1);
	for (m = 1; m <= n1; m++)
		{
		xi = m;
		cs = ((1 - cos (xi * angle)) / 2);
		DataArray[m] = cs * DataArray[m];
		}

	n3 = end * nSamples;
	n5 = nSamples - n3;
	n4 = n5 + 1;
	angle = PI / ((double) n3);
	for (m = n4; m < nSamples; m++)
		{
		xi = m - nSamples - 1;
		cs = ((1 - cos (xi * angle)) / 2);
		DataArray[m] = cs * DataArray[m];
		}

	return (1);
	}

int num_traces (Dbptr tr)
	{
	int ntr, rs, re, bs, be;
	Dbptr bundle;

	dbget_range (tr, &rs, &re);
	ntr = 0;
	printf (" num_tr outer range %d - %d \n", rs, re);
	for (tr.record = rs; tr.record < re; tr.record++)
		{
		dbgetv (tr, 0, "bundle", &bundle, 0);
		dbget_range (bundle, &bs, &be);
		ntr += be - bs;
		}
	return (ntr);
	}
