/*
 *    I offer this software with no promise that it
 *    works properly and certainly no promise of support.
 *    It is a very old program originating from my
 *    days at the University. I have only put this into
 *    contrib at the request of many folks who wanted
 *    to see it resurrected. This source code is my ONLY
 *    contribution. Please modify it as much as you wish,
 *    but please don't send me e-mails asking questions -
 *    I have long since recycled any memory associated with
 *    this ancient program.
 *
 *    Danny Harvey - BRTT - March, 2007
 */

#include <stdio.h>

#include "scv2.h"
#include "dbl2.h"
#include "csstime.h"
#include "math.h"

void
usage()

{
	fprintf (stderr, 
		"usage: dbarrparams dbname [-pol tlead_pol tlag_pol] [-ap tmin_ap tmax_ap]\n");
	fprintf (stderr, 
		"                          [-fm tmin_fm tmax_fm] [-snr tlead_snr tlag_snr]\n");
	fprintf (stderr, 
		"                          [-filter type lco hco lord hord tpad]\n");
}


int
main (int argc, char **argv)
{
	char *prog, *dbname, *stachan=NULL, *tstart=NULL, *tend=NULL;
	int pol=0,ap=0,fm=0,snr=0,fil=0;
	double tmin_ap, tmax_ap;
	double tmin_fm, tmax_fm;
	double tlead_pol,tlag_pol;
	double tlead_snr,tlag_snr;
	int type=0, lord, hord;
	double lco=0.0, hco=0.0, tpad;
	SCV **scvs;
	int nscvs;
	int i, j, k;
	DBLink *dbl;
	int narrs, ituple, jtuple;
	double time, tmin, tmax;
	char *phase;
	struct date_time dt;
	Trace *trace;
	double amp, per, mean, noise, signal;
	char fm_str[32];
	double az, az2, azres, inc, rect;
	int arid, orid;
	char *sta, *chan;
	char vmodel[32];

	double atof();

	if (argc < 2) {
		usage();
		exit (1);
	}
	Program_Name = argv[0] ; 
	if (strcmp(argv[1], "-V" ) == 0 ) {
	    banner ( Program_Name, 0) ;
	    exit(0); 
	}
	prog = *argv;
	argc--;
	argv++;
	dbname = *argv;
	argc--;
	argv++;
	while (argc > 0) {
		if (!strcmp(*argv, "-pol")) {
			pol = 1;
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tlead_pol = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tlag_pol = atof(*argv);
		} else if (!strcmp(*argv, "-ap")) {
			ap = 1;
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tmin_ap = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tmax_ap = atof(*argv);
		} else if (!strcmp(*argv, "-fm")) {
			fm = 1;
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tmin_fm = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tmax_fm = atof(*argv);
		} else if (!strcmp(*argv, "-snr")) {
			snr = 1;
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tlead_snr = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tlag_snr = atof(*argv);
		} else if (!strcmp(*argv, "-filter")) {
			fil = 1;
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			type = atoi(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			lco = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			hco = atof(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			lord = atoi(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			hord = atoi(*argv);
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "%s: Missing Argument.\n",
						prog);
				usage();
				exit (1);
			}
			if ((*argv)[0] == '-') {
				fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
				usage();
				exit (1);
			}
			tpad = atof(*argv);
		} else {
			fprintf (stderr, "%s: Illegal Argument %s.\n",
						prog, *argv);
			usage();
			exit (1);
		}
		argc--;
		argv++;
	}
	if (!pol && !ap && !fm && !snr) {
		fprintf (stderr, "%s: Nothing to do!\n", prog);
		usage();
		exit (1);
	}
	if (lco == 0.0 && hco == 0.0) type = 0;
	scvs = (SCV **) SCV_create("css3.0", 1, &dbname,
			stachan, tstart, tend, 1, &nscvs);
	if (scvs == (SCV **) NULL) {
		if (nscvs == 0) {
			fprintf(stderr, "%s: No sta-chans found.\n",
							prog);
			exit (1);
		} else {
			fprintf(stderr, "%s: Error return from SCV_create.\n",
							prog);
			exit (1);
		}
	}
	dbl = scvs[0]->dbl;
	for (i=0; i<nscvs; i++) {
		SCV_get (scvs[i], 
				SCV_STA, &sta,
				SCV_CHAN, &chan,
				SCV_NARRS, &narrs, NULL);
		for (j=0; j<narrs; j++) {
			SCV_get_arrival (scvs[i], j, SCV_ARR_TIME, &time,
					SCV_ARR_PHASE, &phase, NULL);
			ituple = scvs[i]->arrivals[j]->ituple;
			dt.epoch = time;
			etoh (&dt);
			DBL_get_attrs (dbl, RELID_ARRIVAL, ituple,
							ATTRID_ARID, &arid,
							NULL);
			printf ("%5.5d %-6.6s %-6.6s on %d",
					arid, sta, chan, dt.date,
					dt.second);
			if (pol) {
				if (get_pol (scvs, i, nscvs, time-tlead_pol, time+tlag_pol,
						type, lco, hco, lord, hord, tpad,
						&az, &inc, &rect)) {
					jtuple = find_assoc (dbl, ituple, &az2);
					printf ("%8.3f %8.3f %8.3f", az, inc, rect);
					DBL_put_attrs (dbl, RELID_ARRIVAL, ituple,
							ATTRID_AZIMUTH, az,
							ATTRID_EMA, inc,
							ATTRID_RECT, rect,
							NULL);
					if (jtuple > -1) {
						azres = az - az2;
						while (azres > 180.0) azres -= 360.0;
						while (azres < -180.0) azres += 360.0;
						DBL_get_attrs (dbl, RELID_ASSOC, jtuple,
							ATTRID_VMODEL, vmodel,
							ATTRID_ORID, &orid,
							NULL);
						printf (" %5.5d %8.3f %8.3f %s", orid, az2, azres,
								vmodel);
						DBL_put_attrs (dbl, RELID_ASSOC, jtuple,
							ATTRID_AZRES, azres, NULL);
					}
				}
			}
			if (ap || fm) {
				printf (" ");
				if (ap) {
					tmin = tmin_ap;
					tmax = tmax_ap;
				} else {
					tmin = tmin_fm;
					tmax = tmax_fm;
				}
				if (get_apfm (scvs[i], time, tmin, tmax, fm_str, &amp, &per)) {
					printf ("%s %8.3f %8.3f", fm_str, amp, per);
					DBL_put_attrs (dbl, RELID_ARRIVAL, ituple,
							ATTRID_FM, fm_str,
							ATTRID_AMP, amp,
							ATTRID_PER, per,
							NULL);
				}
			}
			if (snr) {
				mean = 0.0;
				printf (" ");
				if (get_rms (scvs[i], time-tlead_snr, time, &mean, &noise))
					if (get_rms (scvs[i], time, time+tlag_snr,&mean,&signal)){
					printf ("%8.3f", signal/noise);
					DBL_put_attrs (dbl, RELID_ARRIVAL, ituple,
							ATTRID_SNR, signal/noise,
							NULL);
				}
			}
			printf ("\n");
		}
	}
	exit (0);
}

int find_assoc (dbl, ituple, az)

DBLink *dbl;
int ituple;
double *az;

{
	int arid, assoc_arid;
	int i, n;
	float assoc_az;

	arid = -1;
	DBL_get_attrs (dbl, RELID_ARRIVAL, ituple, ATTRID_ARID, &arid, NULL);
	if (arid < 0) return (-1);
	n = DBL_get_ntuples (dbl, RELID_ASSOC);
	if (n < 1) return (-1);
	for (i=0; i<n; i++) {
		DBL_get_attrs (dbl, RELID_ASSOC, i,
				ATTRID_ARID, &assoc_arid, 
				ATTRID_SEAZ, &assoc_az, NULL);
		if (arid == assoc_arid) {
			*az = assoc_az;
			return (i);
		}
	}
	return (-1);
}

int get_pol (scvs, iscv, nscv, tstart, tend, 
		type, lco, hco, lord, hord, tpad, az, inc, rect)

SCV **scvs;
int iscv;
int nscv;
double tstart;
double tend;
int type;
double lco, hco, tpad;
int lord, hord;
double *az;
double *inc;
double *rect;

{
	char *sta, *chan;
	char *sta2, *chan2;
	Trace *tracee=NULL, *tracen=NULL, *tracez=NULL;
	int i, j, k;
	float *s;
	float az4, inc4, rect4, srate, xlpass, xhpass;
	int iwndocntr, ns, itype;
	int is, ie;
	float t0, dt, tsout, twout, lco4, lord4, hco4, hord4;
	int ifilt, n;

	SCV_get (scvs[iscv], SCV_STA, &sta,
				SCV_CHAN, &chan, NULL);
	switch (chan[strlen(chan)-1]) {
	case 'N':
	case 'n':
		tracen = (Trace *) SCV_get_trace (scvs[iscv], tstart-tpad, tend+tpad);
		if (tracen != NULL) {
			if (tracen->next) {
				printf ("ms");
				SCV_free_trace(tracen);
				return (0);
			} 
		} else {
			printf ("nd");
			return (0);
		}
		break;
	case 'E':
	case 'e':
		tracee = (Trace *) SCV_get_trace (scvs[iscv], tstart-tpad, tend+tpad);
		if (tracee != NULL) {
			if (tracee->next) {
				printf ("ms");
				SCV_free_trace(tracee);
				return (0);
			} 
		} else {
			printf ("nd");
			return (0);
		}
		break;
	case 'Z':
	case 'z':
		tracez = (Trace *) SCV_get_trace (scvs[iscv], tstart-tpad, tend+tpad);
		if (tracez != NULL) {
			if (tracez->next) {
				printf ("ms");
				SCV_free_trace(tracez);
				return (0);
			} 
		} else {
			printf ("nd");
			return (0);
		}
		break;
		break;
	default:
		printf ("ch");
		return (0);
	}
	for (i=0; i<nscv; i++) {
		if (i == iscv) continue;
		SCV_get (scvs[i], SCV_STA, &sta2,
				SCV_CHAN, &chan2, NULL);
		if (!strcmp(sta, sta2) && 
				!strncmp(chan, chan2, strlen(chan)-1)) {
			switch (chan2[strlen(chan2)-1]) {
			case 'N':
			case 'n':
				if (tracen) {
					printf ("mt");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				tracen = (Trace *) SCV_get_trace (scvs[i], tstart-tpad, tend+tpad);
				if (tracen != NULL) {
					if (tracen->next) {
						printf ("ms");
						SCV_free_trace(tracen);
						SCV_free_trace(tracee);
						SCV_free_trace(tracez);
						return (0);
					} 
				} else {
					printf ("nd");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				break;
			case 'E':
			case 'e':
				if (tracee) {
					printf ("mt");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				tracee = (Trace *) SCV_get_trace (scvs[i], tstart-tpad, tend+tpad);
				if (tracee != NULL) {
					if (tracee->next) {
						printf ("ms");
						SCV_free_trace(tracen);
						SCV_free_trace(tracee);
						SCV_free_trace(tracez);
						return (0);
					} 
				} else {
					printf ("nd");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				break;
			case 'Z':
			case 'z':
				if (tracez) {
					printf ("mt");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				tracez = (Trace *) SCV_get_trace (scvs[i], tstart-tpad, tend+tpad);
				if (tracez != NULL) {
					if (tracez->next) {
						printf ("ms");
						SCV_free_trace(tracen);
						SCV_free_trace(tracee);
						SCV_free_trace(tracez);
						return (0);
					} 
				} else {
					printf ("nd");
					SCV_free_trace(tracen);
					SCV_free_trace(tracee);
					SCV_free_trace(tracez);
					return (0);
				}
				break;
			default:
				break;
			}
		}
	}
	if (tracen == NULL) {
		printf ("mn");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracee == NULL) {
		printf ("me");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracez == NULL) {
		printf ("mz");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracen->nsamps == 0) {
		printf ("mn");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracee->nsamps == 0) {
		printf ("me");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracez->nsamps == 0) {
		printf ("mz");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracen->tstart != tracee->tstart
			|| tracen->tstart != tracez->tstart) {
		printf ("ts");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracen->dt != tracee->dt
			|| tracen->dt != tracez->dt) {
		printf ("dt");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	if (tracen->nsamps != tracee->nsamps
			|| tracen->nsamps != tracez->nsamps) {
		printf ("ns");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	is = (tstart - tracee->tstart) / tracee->dt + 0.5;
	ie = (tend - tracee->tstart) / tracee->dt + 0.5;
	if (ie >= tracee->nsamps || ie >= tracen->nsamps
				|| ie >= tracez->nsamps) {
		printf ("ns");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	s = (float *) malloc (4*tracee->nsamps*sizeof(float));
	if (s == NULL) {
		printf ("me");
		SCV_free_trace(tracen);
		SCV_free_trace(tracee);
		SCV_free_trace(tracez);
		return (0);
	}
	ns = ie - is + 1;
	iwndocntr = (is + ie) / 2;
	xlpass = 0.0;
	xhpass = 0.0;
	itype = 1;
	srate = 1.0 / tracee->dt;
	for (i=0,j=0; i<tracee->nsamps; i++,j++) {
		s[j] = tracez->data[i];
	}
	for (i=0; i<tracee->nsamps; i++,j++) {
		s[j] = tracen->data[i];
	}
	for (i=0; i<tracee->nsamps; i++,j++) {
		s[j] = tracee->data[i];
	}
	if (type) {
		t0 = 0.0;
		dt = tracee->dt;
		tsout = 0.0;
		twout = (tracee->nsamps-1)*dt;
		lco4 = lco;
		hco4 = hco;
		lord4 = lord;
		hord4 = hord;
		ifilt = type;
		filter_ (&tracee->nsamps, &t0, &dt, s,
			&tsout, &twout, &ifilt,
			&lco4, &lord4, &hco4, &hord4, &hord4, &tracee->nsamps,
			&n, &t0, s);
		filter_ (&tracee->nsamps, &t0, &dt, &s[tracee->nsamps],
			&tsout, &twout, &ifilt,
			&lco4, &lord4, &hco4, &hord4, &hord4, &tracee->nsamps,
			&n, &t0, &s[tracee->nsamps]);
		filter_ (&tracee->nsamps, &t0, &dt, &s[2*tracee->nsamps],
			&tsout, &twout, &ifilt,
			&lco4, &lord4, &hco4, &hord4, &hord4, &tracee->nsamps,
			&n, &t0, &s[2*tracee->nsamps]);
	}
	polarization_ (s, &s[3*tracee->nsamps], &tracee->nsamps, &tracee->nsamps,
		&iwndocntr, &ns, &srate, &xlpass, &xhpass, &itype,
		&az4, &inc4, &rect4);
	*az = az4;
	*inc = inc4;
	*rect = rect4;
	SCV_free_trace(tracen);
	SCV_free_trace(tracee);
	SCV_free_trace(tracez);
	free (s);
	return (1);
}

int get_rms (scv, tstart, tend, mean, rms)

SCV *scv;
double tstart;
double tend;
double *mean;
double *rms;

{
	Trace *trace;
	int j, k;

	double sqrt();

	trace = (Trace *) SCV_get_trace (scv, tstart, tend);
	if (trace != NULL) {
		if (trace->next) {
			printf ("ms");
			SCV_free_trace(trace);
			return (0);
		} else {
			if (trace->nsamps == 0) {
				printf ("0s");
				SCV_free_trace(trace);
				return (0);
			} else {
				*rms = 0.0;
				if (*mean == 0.0) {
					for (j=0; j<trace->nsamps; j++) 
						*mean += trace->data[j];
					*mean = *mean / trace->nsamps ;
				}
				for (j=0; j<trace->nsamps; j++)
					*rms += (trace->data[j]-(*mean))
						*(trace->data[j]-(*mean));
				*rms = *rms / trace->nsamps;
				*rms = sqrt ( *rms );
			}
		}
		SCV_free_trace(trace);
	} else {
		printf ("nd");
		return (0);
	}
	return (1);
}

int get_apfm (scv, tstart, tmin, tmax, fm, amp, per)

SCV *scv;
double tstart;
double tmin, tmax;
char *fm;
double *amp;
double *per;

{
	Trace *trace;
	int i, j;
	float *extrema;
	int *iextrema;
	int nextrema;
	float exmax;
	int jmin;

	trace = (Trace *) SCV_get_trace (scv, tstart, tstart+tmax);
	if (trace != NULL) {
		if (trace->nsamps == 0) {
			printf ("0s");
			SCV_free_trace(trace);
			return (0);
		} else {
			for (i = 0; i < trace->nsamps; i++)
				if ((trace->tstart + i * trace->dt) > tstart) break;
			if (trace->nsamps - i < 3) {
				printf ("0s");
				SCV_free_trace(trace);
				return (0);
			}
			if (!find_extrema (&trace->data[i], trace->nsamps-i+1,
							&nextrema, &extrema, &iextrema)) {
				printf ("er");
				SCV_free_trace(trace);
				return (0);
			}
			if (nextrema < 1) {
				printf ("0s");
				SCV_free_trace(trace);
				return (0);
			}
			jmin = tmin / trace->dt + 0.5;
			for (j=0; j<nextrema; j++) if (iextrema[j] >= jmin) break;
			if (j == nextrema) {
				printf ("nm");
				SCV_free_trace(trace);
				return (0);
			}
			if (extrema[j] < 0.0) {
				exmax = 0.8*extrema[j];
				for (j++; j<nextrema; j++) {
					if (extrema[j] > exmax) break;
					exmax = 0.8*extrema[j];
				}
				strcpy (fm, "d");
				*amp = extrema[j-1];
				*per = trace->dt*iextrema[j-1];
			} else {
				exmax = 0.8*extrema[j];
				for (j++; j<nextrema; j++) {
					if (extrema[j] < exmax) break;
					exmax = 0.8*extrema[j];
				}
				strcpy (fm, "c");
				*amp = extrema[j-1];
				*per = trace->dt*iextrema[j-1];
			}
		}
		SCV_free_trace(trace);
	} else {
		printf ("nd");
		return (0);
	}
	return (1);
}

int find_extrema (data, ns, nextrema, extrema, iextrema)

float *data;
int ns;
int *nextrema;
float **extrema;
int **iextrema;

{
	int i, sense, old_sense;
	static float *extr = NULL;
	static int *iextr = NULL;
	static int exsize = 100;

	*nextrema = 0;
	if (data[1] >= data[0]) old_sense = 1; else old_sense = -1;
	for (i=2; i<ns; i++) {
		if (data[i] == data[i-1]) sense = old_sense;
		else if (data[i] > data[i-1]) sense = 1; else sense = -1;
		if (sense != old_sense) {
			if (extr == NULL) {
				extr = (float *) malloc (exsize*sizeof(float));
				if (extr == NULL) return (0);
				iextr = (int *) malloc (exsize*sizeof(int));
				if (iextr == NULL) {
					free (extr);
					extr = NULL;
					return (0);
				}
			} else {
				if (*nextrema >= exsize) {
					exsize *= 2;
					extr = (float *) realloc (extr, exsize*sizeof(float));
					if (extr == NULL) {
						free (iextr);
						iextr = NULL;
						return (0);
					}
					iextr = (int *) realloc (iextr, exsize*sizeof(int));
					if (iextr == NULL) {
						free (extr);
						extr = NULL;
						return (0);
					}
				}
			}
			iextr[*nextrema] = i-1;
			extr[*nextrema] = data[i-1] - data[0];
			(*nextrema)++;
		}
		old_sense = sense;
	}
	*extrema = extr;
	*iextrema = iextr;
	return (1);
}
