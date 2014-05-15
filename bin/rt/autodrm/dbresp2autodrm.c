#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#include "db.h"
#include "response.h"
#include "stock.h"
#include "paz.h"

static Arr     *Segtype;

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s database\n", Program_Name);
    banner (Program_Name, 0);
    exit (1);
}

static void
adwrite_paz (long stageid, int decimation, double gnom, char segtype,
	     char *ounits, Paz * paz)
{
    char           *desc = "";
    int             i;
    int             nzeros;

    if (paz->frequency == 0.0) {
	double          amplitude,
	                phase;
	paz->frequency = 1.0;
	paz->normalization = 1.0;
	eval_paz (paz->frequency, paz, &amplitude, &phase);
	paz->normalization = 1.0 / amplitude;
    }
    switch (segtype) {
      case 'A':
	nzeros = paz->nzeros + 2;
	gnom /= SQR (2 * M_PI * paz->frequency);
	break;

      case 'V':
	nzeros = paz->nzeros + 1;
	gnom /= 2 * M_PI * paz->frequency;
	break;

      default:
	nzeros = paz->nzeros;
	break;
    }

    if (decimation >= 1) {
	fprintf (stdout, "PAZ2 %2ld %c %15.8e %4d         %3d %3d %-25.25s\n",
	     stageid, *ounits, gnom, decimation, paz->npoles, nzeros, desc);
    } else {
	fprintf (stdout, "PAZ2 %2ld %c %15.8e              %3d %3d %-25.25s\n",
		 stageid, *ounits, gnom, paz->npoles, nzeros, desc);
    }
    for (i = 0; i < paz->npoles; i++) {
	fprintf (stdout, "%15.8e %15.8e\n",
		 paz->poles[i].real, paz->poles[i].imag);
    }
    for (i = 0; i < paz->nzeros; i++) {
	fprintf (stdout, "%15.8e %15.8e\n",
		 paz->zeros[i].real, paz->zeros[i].imag);
    }
    for (; i < nzeros; i++) {
	fprintf (stdout, "%15.8e %15.8e\n", 0.0, 0.0);
    }
}

static void
get_symmetry (Fir * fir, int *n2print, char *symmetry)
{
    int             i,
                    n;
    int             symmetric = 1;

    n = fir->nnum / 2;
    for (i = 0; i < n; i++) {
	if (fir->num_coefs[i] != fir->num_coefs[fir->nnum - i - 1]) {
	    symmetric = 0;
	    break;
	}
    }

    if (symmetric) {
	if (2 * n == fir->nnum) {
	    *symmetry = 'C';
	    *n2print = n;
	} else {
	    *symmetry = 'B';
	    *n2print = n + 1;
	}
    } else {
	*symmetry = 'A';
	*n2print = fir->nnum;
    }
}

static int
adwrite_fir (long stageid, double gnom, Fir * fir)
{
    int             i;
    char           *desc = "",
                    symmetry;
    double          group_correction = 0.0;
    int             n2print;
    int             errors = 0;

    get_symmetry (fir, &n2print, &symmetry);
    fprintf (stdout, "FIR2 %2ld %10.2e %4d %8.3f %c %4d %-25.25s\n",
	     stageid, gnom, fir->dec_factor,
	     group_correction, symmetry, n2print, desc);

    if (fir->nden > 0) {
	complain (0, "fir filter has denominator coefficients, autoDRM doesn't allow");
	errors++;
    }
    for (i = 0; i < n2print; i++) {
	if (i > 0 && (i % 5) == 0)
	    fprintf (stdout, "\n");
	fprintf (stdout, " %15.8e",
		 fir->num_coefs[i]);
    }
    fprintf (stdout, "\n");

    return errors;
}

static int
adwrite_fap (long stageid, char *ounits, Fap * fap)
{
    int             i;
    char           *desc = "";
    double          group_correction = 0.0;
    int             errors = 0;

    fprintf (stdout, "FAP2 %2ld %c      %8.3f %3d %-25.25s\n",
	     stageid, *ounits, group_correction, fap->ntriplets, desc);

    for (i = 0; i < fap->ntriplets; i++) {
	fprintf (stdout, " %10.5f %15.8e %4.0f\n",
		 fap->freqs[i],
		 fap->amps[i],
		 fap->phases[i]);
    }

    return errors;
}

static int
adwrite_fap2 (long stageid, char *ounits, Fap2 * fap2)
{
    int             i;
    char           *desc = "";
    double          group_correction = 0.0;
    int             errors = 0;

    fprintf (stdout, "FAP2 %2ld %c      %8.3f %3d %-25.25s\n",
	     stageid, *ounits, group_correction, fap2->ntriplets, desc);

    for (i = 0; i < fap2->ntriplets; i++) {
	fprintf (stdout, " %10.5f %15.8e %4.0f\n",
		 fap2->freqs[i],
		 fap2->amps[i],
		 fap2->phases[i]);
    }

    return errors;
}


static int
get_instype (Dbptr db, char *instype, double *samprate)
{
    static Dbptr    dbsi = INVALID_DBPTR;
    static Hook    *hook = 0;
    long            nmatches;
    Tbl            *tbl;
    char            sta[MAX_STA_SIZE],
                    chan[MAX_CHAN_SIZE];
    double          time;
    char           *s;
    int             errors = 0;

    if (dbsi.database < 0) {
	Dbptr           dbsensor,
	                dbinstrument;
	dbsensor = dblookup (db, 0, "sensor", 0, 0);
	dbinstrument = dblookup (db, 0, "instrument", 0, 0);
	dbsi = dbjoin (dbsensor, dbinstrument, 0, 0, 0, 0, 0);
    }
    nmatches = dbmatches (db, dbsi, 0, 0, &hook, &tbl);
    switch (nmatches) {
      case dbINVALID:
      case 0:
	dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	complain (0, "no matches in sensor/instrument join for %s:%s @ %s",
		  sta, chan, s = strydtime (time));
	free (s);
	errors++;
	break;

      default:
	dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	complain (0, "too many matches (%ld) in sensor/instrument join for %s:%s @ %s",
		  nmatches, sta, chan, s = strydtime (time));
	free (s);
	errors++;
	/* FALLTHRU */

      case 1:
	dbsi.record = (long) gettbl (tbl, 0);
	dbgetv (dbsi, 0, "instype", instype, "samprate", samprate, NULL);
	break;
    }

    freetbl (tbl, 0);
    return errors;
}

static int
write_cal2 (Dbptr db, double time, double endtime, double *calperp)
{
    static Dbptr    dbx = INVALID_DBPTR;
    static double   null_endtime = 0.0;
    Dbptr           dbcalibration;
    long            nmatches;
    char            sta[MAX_STA_SIZE],
                    chan[MAX_CHAN_SIZE],
                    units[16],
                    instype[16];
    double          calib,
                    calper=0;
    long            j;
    int             errors = 0;
    Tbl            *tbl;
    static Hook    *hook = 0;
    char           *ontime,
                   *offtime;
    char           *aux = "";
    char           *s,
                   *t;
    char            segtype;
    double          samprate;

    if (dbx.database == dbINVALID) {
	dbx = dblookup (db, 0, "wfdisc", 0, 0);
	dbx.record = dbNULL;
	dbgetv (dbx, 0, "endtime", &null_endtime, NULL);
	dbx.record = dbSCRATCH;
    }
    dbgetv (db, 0, "sta", sta, "chan", chan, NULL);
    dbputv (dbx, 0, "sta", sta, "chan", chan, "time", time, "endtime", endtime, NULL);

    errors += get_instype (dbx, instype, &samprate);

    dbcalibration = dblookup (db, 0, "calibration", 0, 0);
    nmatches = dbmatches (dbx, dbcalibration, 0, 0, &hook, &tbl);
    switch (nmatches) {
      case dbINVALID:
      case 0:
	dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	complain (0, "no matches in calibration table for %s:%s @ %s",
		  sta, chan, s = strydtime (time));
	free (s);
	break;

      default:
	dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	complain (0, "too many matches (%ld) in calibration table for %s:%s @ %s - %s",
	nmatches, sta, chan, s = strydtime (time), t = strydtime (endtime));
	free (s);
	free (t);
	errors++;
	/* FALLTHRU */

      case 1:
	dbcalibration.record = (long) gettbl (tbl, 0);
	dbgetv (dbcalibration, 0,
		"sta", sta,
		"chan", chan,
		"calib", &calib,
		"calper", &calper,
		"units", units,
		NULL);
	*calperp = calper;

	for (j = 0; j < strlen (units); j++) {
	    units[j] = tolower (units[j]);
	}
	s = getarr (Segtype, units);
	if (s == 0) {
	    segtype = 'D';
	} else {
	    segtype = *s;
	}
	switch (segtype) {
	  case 'A':
	    calib *= SQR (calper / (2 * M_PI));
	    break;

	  case 'V':
	    calib *= calper / (2 * M_PI);
	    break;

	  default:
	    break;
	}

	ontime = epoch2str (time, "%Y/%m/%d %H:%M");
	if (endtime == null_endtime) {
	    offtime = strdup ("");
	} else {
	    offtime = epoch2str (endtime, "%Y/%m/%d %H:%M");
	}
	fprintf (stdout, "\nCAL2 %-5.5s %-3.3s %4.4s %-6.6s %15.8e %7.3f %11.5f %s %s\n",
	 sta, chan, aux, instype, calib, calper, samprate, ontime, offtime);
	free (ontime);
	free (offtime);
	break;
    }

    freetbl (tbl, 0);
    return errors;
}


static int
autodrm_response (Dbptr db)
{

    Dbptr           dbstage;
    static Hook    *hook = 0;
    char            sta[MAX_STA_SIZE],
                    chan[MAX_CHAN_SIZE];
    double          time,
                    endtime;
    long            stageid;
    char           *s;
    Response       *response;
    Response_group *group;
    char            iunits[96],
                    ounits[96];
    char            gtype[100];
    long            i,
                    j;
    Paz            *paz;
    Fir            *fir;
    Fap            *fap;
    Fap2           *fap2;
    FILE           *file;
    double          samprate,
                    gnom,
                    gcalib;
    double          calper=0.0;
    long            decifac;
    char            dir[100],
                    dfile[100];
    char            filename[STRSZ];
    long            nmatches;
    int             errors = 0;
    Tbl            *tbl,
                   *stbl;
    double          mintime=0,
                    maxtime=0;
    char            segtype;
    static Tbl     *keys1 = 0,
                   *keys2 = 0;

    if (keys1 == 0) {
	keys1 = strtbl ("sta", "chan", "time", NULL);
	keys2 = strtbl ("sta", "chan", "time::endtime", NULL);
    }
    dbstage = dblookup (db, 0, "stage", 0, 0);
    nmatches = dbmatches (db, dbstage, &keys1, &keys2, &hook, &tbl);

    switch (nmatches) {
      case dbINVALID:
      case 0:
	dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	complain (0, "Can't match record for %s:%s @ %s in stage table",
		  sta, chan, s = strydtime (time));
	free (s);
	errors++;
	break;

      default:
	stbl = newtbl (maxtbl (tbl));
	for (i = 0; i < nmatches; i++) {
	    dbstage.record = (long) gettbl (tbl, i);
	    dbgetv (dbstage, 0,
		    "stageid", &stageid,
		    "time", &time,
		    "endtime", &endtime,
		    NULL);
	    if (i == 0) {
		mintime = time;
		maxtime = endtime;
	    } else {
		mintime = MAX (time, mintime);
		maxtime = MIN (endtime, maxtime);
	    }
	    settbl (stbl, stageid - 1, (char *) i);
	}
	if (maxtbl (tbl) != maxtbl (stbl)) {
	    dbgetv (db, 0, "sta", sta, "chan", chan, "time", &time, NULL);
	    complain (0, "stageid numbers for %s:%s @ %s don't add up.",
		      sta, chan, s = strydtime (time));
	    free (s);
	    errors++;
	} else {
	    errors += write_cal2 (db, mintime, maxtime, &calper);

	    for (i = 0; i < nmatches; i++) {
		j = (long) gettbl (stbl, i);
		dbstage.record = (long) gettbl (tbl, j);
		dbgetv (dbstage, 0,
			"sta", sta,
			"chan", chan,
			"time", &time,
			"endtime", &endtime,
			"stageid", &stageid,
			"decifac", &decifac,
			"samprate", &samprate,
			"gnom", &gnom,
			"gcalib", &gcalib,
			"dir", dir,
			"dfile", dfile,
			"gtype", gtype,
			"iunits", iunits,
			"ounits", ounits,
			NULL);

		if (gcalib > 0.0) {
		    gnom *= gcalib;
		} else if (gcalib < 0.0) {
		    complain (0, "gcalib = %10.3f < 0. is invalid for %s:%s @ %s.\n",
			      gcalib, sta, chan, s = strydtime (time));
		    free (s);
		    errors++;
		}
		if (*dir != '-' || *dfile != '-') {
		    long            mark;
		    dbextfile (dbstage, "stage", filename);
		    mark = elog_mark ();
		    elog_log (0, "response file is '%s'", filename);
		    if ((file = fopen (filename, "r")) == 0
			    || read_response (file, &response) < 0) {
			register_error (0,
			      "Can't read response file %s  for %s_%s @ %s",
				 filename, sta, chan, s = strydtime (time));
			free (s);
			fclose (file);
			errors++;
		    } else {
			fclose (file);
			if (response->ngroups > 1) {
			    register_error (0,
			      "stage response file %s has %d stages, not 1",
					    filename, response->ngroups);
			    errors++;
			} else {

			    group = response->groups;

			    switch (group->id) {

			      case PAZ:
				/* The normalization frequency chosen in the
				 * response file may not necessarily be the
				 * same as the one chosen by calibration
				 * table for insertion into the seed volumes.
				 * Consequently, we have to adjust the
				 * specified gnom to be correct for the seed
				 * normalization frequency.  Since the gain
				 * is
				 * 
				 * G(f) = gnom_db * A_response_file * P(f) =
				 * gnom_seed * A_seed * P(f)
				 * 
				 * We have
				 * 
				 * gnom_seed = gnom_db * A_response_file /
				 * A_seed
				 * 
				 * gnom_db is just the gnom from the stage
				 * table. A_response_file is the
				 * normalization from the response file, left
				 * in the stage structure. Below, we
				 * calculate A_seed by setting
				 * A_response_file to 1.0.
				 * 
				 */

				paz = (Paz *) group->pvt;
				for (j = 0; j < strlen (iunits); j++) {
				    iunits[j] = tolower (iunits[j]);
				}
				s = getarr (Segtype, iunits);
				if (s == 0) {
				    segtype = 'D';
				} else {
				    segtype = *s;
				}
				adwrite_paz (stageid, 0, gnom, segtype, ounits, paz);
				break;

			      case IIR:
				paz = (Paz *) group->pvt;
				for (j = 0; j < strlen (iunits); j++) {
				    iunits[j] = tolower (iunits[j]);
				}
				s = getarr (Segtype, iunits);
				if (s == 0) {
				    segtype = 'D';
				} else {
				    segtype = *s;
				}
				adwrite_paz (stageid, 1, gnom, segtype, ounits, paz);
				break;

			      case FIR:
				fir = (Fir *) group->pvt;
				errors += adwrite_fir (stageid, gnom, fir);
				break;

			      case FAP:
				fap = (Fap *) group->pvt;
				errors += adwrite_fap (stageid, ounits, fap);
				break;

			      case FAP2:
				fap2 = (Fap2 *) group->pvt;
				errors += adwrite_fap2 (stageid, ounits, fap2);
				break;


			      default:
				complain (0, "Unknown filter type %d in response file %s\n",
					  group->id, filename);
				errors++;
				break;

			    }
			}
		    }
		    elog_flush (mark, 0);
		} else {
		    char           *desc = "";

		    if (gcalib > 0.0) {
			gnom *= gcalib;
		    } else if (gcalib < 0.0) {
			complain (0, "gcalib = %10.3f < 0. is an invalid value.\n", gcalib);
			errors++;
		    }
		    if (strcmp (gtype, "digitizer") == 0
		    /* following hack for psd2db */
			    || strcmp (gtype, "sensor") == 0) {
			fprintf (stdout, "DIG2 %2ld %15.8e %11.5f %s\n",
				 stageid, gnom, samprate, desc);
		    } else if (strcmp (gtype, "amplifier") == 0) {
			/* no corners */
			fprintf (stdout, "GEN2 %2ld %c %15.8e %7.3f                 0 %s\n",
				 stageid, *ounits, gnom, calper, desc);
		    } else {
			complain (0, "Unrecognized gtype='%s' for %s:%s @ %s",
				  gtype, sta, chan, s = strydtime (time));
			free (s);
			errors++;
		    }
		}
	    }
	}
	freetbl (stbl, 0);
	break;

    }
    freetbl (tbl, 0);
    return errors;
}

int
main (int argc, char **argv)
{
    int             c,
                    errflg = 0;
    Dbptr           db;
    char           *table;
    int             verbose = 0;
    int             errors = 0;
    long            n;
    Pf             *pf;

    elog_init (argc, argv);

    if (pfread (Program_Name, &pf) != 0)
	die (0, "Can't read parameter file\n");

    Segtype = pfget_arr (pf, "segtype");

    while ((c = getopt (argc, argv, "vV")) != -1) {
	switch (c) {

	  case 'v':
	    verbose++;
	    break;

	  case 'V':
	    banner (Program_Name, 0) ;
	    exit (0);

	  default:
	    errflg++;
	    break;
	}
    }

    if (errflg || argc - optind != 1)
	usage ();

    table = argv[optind++];
    n = dbopen_table (table, "r", &db);
    switch (n) {

      case dbINVALID:
	die (0, "Can't open input table '%s'", table);
	break;

      case 0:
	die (0, "No records in input table '%s'", table);
	break;
    }

    for (db.record = 0; db.record < n; db.record++) {
	errors += autodrm_response (db);
    }

    return (errors == 0 ? 0 : 1);
}
