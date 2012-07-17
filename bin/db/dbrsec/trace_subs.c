
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "tr.h"
#include "db.h"
#include "arrays.h"
#include "scv2.h"

#define ST_POWER        1
#define ST_VELOCITY     2
#define ST_BOTH     	3

Trace *read_trace();
Trace *read_stgrid_trace();
Trace *convert_trace();
Trace *newtrace();
int zaccess (char *path, int mode);

int
write_trace (Dbptr db, char *sta, char *chan, char *dir, char *dfile, Trace *trace, double tstart, double tend, int overwrite)

{
	int fd;
	char outdir[1024];
	char outbase[1024];
	char fname[1024];
	long sz, size, ret;
	long is, ie, ns;
	Dbvalue dbv;

	is = (tstart-trace->tstart)/trace->dt + 0.0001;
	ie = (tend-trace->tstart)/trace->dt - 0.0001;
	if (is > trace->nsamps-1) return (1);
	if (ie < 0) return (1);
	if (is < 0) is = 0;
	if (ie > trace->nsamps-1) ie = trace->nsamps-1;
	ns = ie - is + 1;
	if (ns < 1) return (1);
	db = dblookup (db, 0, "wfdisc", 0, 0);
	if (dir[0] == '/') {
		sprintf (fname, "%s/%s", dir, dfile);
	} else {
		dbquery (db, dbTABLE_DIRNAME, &dbv);
		strcpy (outbase, dbv.t);
		sprintf (fname, "%s/%s/%s", outbase, dir, dfile);
	}
	if (!overwrite) {
		if (zaccess(fname, F_OK) != -1) {
			fprintf (stderr, "write_trace: wf file conflict for '%s'.\n",
								fname);
			return (0);
		}
	}
	dirbase (fname, outdir, outbase);
	if (makedir(outdir) == -1) {
                fprintf (stderr, "write_trace: Unable to create %s\n", outdir);
                return (0);
        }
	fd = open (fname, (O_RDWR|O_CREAT|O_TRUNC), 0666);
	if (fd < 0) {
		fprintf (stderr, "write_trace: Open error on '%s'.\n", fname);
		return (0);
	}
	sz = atoi(&trace->rawdata_format[strlen(trace->rawdata_format)-1]);
	size = sz*ns;
	ret = write (fd, &(((char *)trace->raw_data)[is*sz]), size);
	close (fd);
	if (ret < size) {
		fprintf (stderr, "write_trace: Write error on '%s'.\n", fname);
		return (0);
	}
        db.record = dbNULL;
        db.field = dbALL;
        dbget(db, 0);
        db.record = dbSCRATCH;
	dbputv (db, 0,	"sta", sta,
			"chan", chan,
			"time", trace->tstart+is*trace->dt,
			"wfid", dbnextid(db, "wfid"),
/*			"chanid", chanid,*/
			"jdate", yearday(trace->tstart),
			"endtime", trace->tstart+trace->dt*(trace->nsamps-1),
			"nsamp", ns,
			"samprate", 1.0/trace->dt,
			"calib", trace->calib,
			"calper", trace->calper,
			"instype", "BEAM",
			"segtype", trace->rawdata_type,
			"datatype", trace->rawdata_format,
/*			"clip", clip,*/
			"dir", dir,
			"dfile", dfile,
			"foff", (long)0,
			NULL);
	dbadd (db, 0);
	return (1);
}

Trace *
convert_trace (Trace *trace, char *format)

{
	Trace *tr;

	if (format) {
		for (tr=trace; tr!=NULL; tr=tr->next) {
			strcpy (tr->rawdata_format, format);
		}
	}
	trace = (Trace *) SCV_trace_toraw (trace, 0);
	trace = (Trace *) SCV_trace_fillgaps (trace);
	return (trace);
}

Trace *
copy_trace (Trace *trace, int copydata)

{
	Trace *trhead, *tr;

	trhead = NULL;
	for (tr=trace; tr!=NULL; tr=tr->next) {
		trhead = (Trace *) copytr (trhead, tr, copydata);
		if (trhead == NULL) {
			fprintf (stderr, "copy_trace: copytr() error.\n");
			return (NULL);
		}
	}
	for (;trhead->prev!=NULL;trhead=trhead->prev);
	return (trhead);
}

void
convsym (float *datain, int nsin, float *coefs, int ncoefs, int decfac, int ioff, int *nsout, float *dataout)

{
	int i, j;
	float hold;
	int i1, i2, n1, n2;

	n1 = ncoefs-1;
	n2 = nsin-ncoefs;
	for (i=ioff,(*nsout)=0; i<nsin; i+=decfac,(*nsout)++) {
		if (i >= n1 && i <= n2) {
			hold = datain[i]*coefs[0];
			for (j=1; j<ncoefs; j++) hold += coefs[j]*(datain[i+j]+datain[i-j]);
		} else {
			i1 = i - n1;
			i2 = i + n1;
			hold = 0.0;
			for (j=i1; j<0; j++) hold += datain[0]*coefs[i-j];
			for (j=nsin; j<=i2; j++) hold += datain[nsin-1]*coefs[j-i];
			if (i1 < 0) i1 = 0;
			if (i2 > nsin-1) i2 = nsin-1;
			for (j=i1; j<i; j++) hold += datain[j]*coefs[i-j];
			for (j=i; j<=i2; j++) hold += datain[j]*coefs[j-i];
		}
		dataout[(*nsout)] = hold;
	}
}

int
decimate_trace (Trace *trace, Tbl *ncoefs, Tbl *coefs, Tbl *dec_fac, double tref)

{
	Trace *tr;
	int i, j, nstages;
	float *cfs;
	int ncfs, decfac;
	int ns, nsout, ioff;
	float *buf=NULL;

	nstages = maxtbl(ncoefs);
	for (tr=trace; tr!=NULL; tr=tr->next) {
		ns = tr->nsamps;
		buf = (float *) malloc (ns*sizeof(float));
		if (buf == NULL) {
			fprintf (stderr, "decimate_trace: Malloc error.\n");
			return (0);
		}
		for (i=0; i<nstages; i++) {
			cfs = (float *) gettbl (coefs, i);
			ncfs = *((int *) gettbl (ncoefs, i));
			decfac = *((int *) gettbl (dec_fac, i));
			ns = tr->nsamps;
			ioff = (tr->tstart-tref)/tr->dt + 0.5;
			ioff = decfac - (ioff%decfac);
			if (ioff == decfac) ioff = 0;
			convsym (tr->data, ns, cfs, ncfs, decfac, ioff, &nsout, buf);
			tr->tstart += ioff*tr->dt;
			tr->dt *= decfac;
			tr->nsamps = nsout;
			for (j=0; j<nsout; j++) tr->data[j] = buf[j];
		}
		free (buf);
	}
	return (1);
}

int
add_trace (Dbptr db, int stgrid, double tstart, double tend, Trace *trace, Trace **traceo, Trace *trace2, Trace **trace2o)

{
	Trace *tr, *trn;

	switch (stgrid) {
	default:
		trn = read_trace (db, tstart, tend);
		if (trn == NULL) {
			fprintf (stderr, "add_trace: read_trace() error.\n");
			return (0);
		}
		if (trn->raw_data == NULL) {
			SCV_free_trace (trn);
			*traceo = trace;
			return (1);
		}
		if (trace == NULL) {
			*traceo = trn;
			return (1);
		}
		for (tr=trace; tr->next!=NULL; tr=tr->next);
		tr->next = trn;
		trn->prev = tr;
		*traceo = trace;
		return (1);
	case ST_POWER:
	case ST_VELOCITY:
		trn = read_stgrid_trace (db, stgrid, tstart, tend);
		if (trn == NULL) {
			fprintf (stderr, "add_trace: read_trace() error.\n");
			return (0);
		}
		if (trn->raw_data == NULL) {
			SCV_free_trace (trn);
			*traceo = trace;
			return (1);
		}
		if (trace == NULL) {
			*traceo = trn;
			return (1);
		}
		for (tr=trace; tr->next!=NULL; tr=tr->next);
		tr->next = trn;
		trn->prev = tr;
		*traceo = trace;
		return (1);
	case ST_BOTH:
		trn = read_stgrid_trace (db, ST_POWER, tstart, tend);
		if (trn == NULL) {
			fprintf (stderr, "add_trace: read_trace() error.\n");
			return (0);
		}
		if (trn->raw_data == NULL) {
			SCV_free_trace (trn);
			*traceo = trace;
		} else if (trace == NULL) {
			*traceo = trn;
		} else {
			for (tr=trace; tr->next!=NULL; tr=tr->next);
			tr->next = trn;
			trn->prev = tr;
			*traceo = trace;
		}
		trn = read_stgrid_trace (db, ST_VELOCITY, tstart, tend);
		if (trn == NULL) {
			fprintf (stderr, "add_trace: read_trace() error.\n");
			return (0);
		}
		if (trn->raw_data == NULL) {
			SCV_free_trace (trn);
			*trace2o = trace2;
		} else if (trace == NULL) {
			*trace2o = trn;
		} else {
			for (tr=trace2; tr->next!=NULL; tr=tr->next);
			tr->next = trn;
			trn->prev = tr;
			*trace2o = trace2;
		}
		return (1);
	}
}

int
read_file (char *fname, long foff, char *datatype, long *nsamps, void **buf)

{
	FILE *file;
	int n, size;

	size = atoi(&datatype[strlen(datatype)-1]);
	file = zopen (fname);
	if (file == NULL) {
		fprintf (stderr, "read_file: Unable to open '%s'\n",
							fname);
		return (0);
	}
	if (fseek(file, foff, 0) < 0) {
		fprintf (stderr, "read_file: fseek() error on '%s'\n",
							fname);
		fclose (file);
		return (0);
	}
	*buf = (void *) malloc (size*(*nsamps));
	if (*buf == NULL) {
		fprintf (stderr, "read_file: Malloc error.\n");
		fclose (file);
		return (0);
	}
	if ((n=fread (*buf, size, *nsamps, file)) < *nsamps) {
		if (n < 1) {
			fprintf (stderr, "read_file: fread() error on '%s'\n",
							fname);
			fclose (file);
			free (*buf);
			return (0);
		} else {
			fprintf (stderr, "read_file: Read %d samples but expected %ld on '%s'\n",
							n, *nsamps, fname);
			*nsamps = n;
		}
	}
	fclose (file);
	return (1);
}

Trace *
read_stgrid_trace (Dbptr db, int stgrid, double tstart, double tend)

{
	char fname[1024];
	char dtype[8];
	long foff, nsamp, ns;
	float *data, *data2;
	Trace *trace;
	double time, samprate;
	double smin, smax, ds, maxpow, slow;
	int isamp, jsamp, size;
	int i, j, k;

        if (dbextfile (db, "stgrid", fname) < 1) {
                fprintf (stderr, "read_stgrid_trace: Unable to find input file '%s'\n",
                                                fname);
                return (NULL);
        }
	dbgetv (db, 0, "time", &time, "dtime", &samprate,
				"nt", &nsamp, "datatype", dtype, 
				"ns", &ns, "foff", &foff, 
				"smin", &smin, "smax", &smax, NULL);
	samprate = 1.0/samprate;
	isamp = (tstart - time)*samprate - 1.5;
	jsamp = (tend - time)*samprate + 1.5;
	if (isamp < 0) isamp = 0;
	if (jsamp > nsamp-1) jsamp = nsamp-1;
	nsamp = jsamp+1;
	size = atoi(&dtype[strlen(dtype)-1]);
	foff += isamp*size*ns;
	nsamp -= isamp;
	time += isamp/samprate;
	if (nsamp < 0) nsamp = 0;
	data = NULL;
	data2 = NULL;
	nsamp *= ns;
	if (nsamp > 0) {
		if (!read_file (fname, foff, dtype, &nsamp, (void **) &data)) {
			fprintf (stderr, "read_stgrid_trace: Unable to read input file '%s'\n",
							fname);
			return (NULL);
		}
		nsamp /= ns;
		data2 = (float *) malloc (sizeof(float)*nsamp);
		if (data2 == NULL) {
			fprintf (stderr, "read_stgrid_trace: Malloc error on data2.\n");
			free (data);
			return (NULL);
		}
		switch (stgrid) {
		default:
		case ST_POWER:
			for (i=0,k=0; i<nsamp; i++) {
				data2[i] = 0.0;
				for (j=0; j<ns; j++,k++) {
					if (data[k] > data2[i]) data2[i] = data[k];
				}
			}
			break;
		case ST_VELOCITY:
			ds = (smax-smin)/(ns-1);
			for (i=0,k=0; i<nsamp; i++) {
				maxpow = 0.0;
				for (j=0; j<ns; j++,k++) {
					if (data[k] > maxpow) {
						maxpow = data[k];
						slow = smin + j*ds;
						if (slow == 0.0) {
							data2[i] = 10.0;
						} else {
							data2[i] = 1.0/slow;
						}
					}
				}
			}
			break;
		}
		free (data);
	}
	trace = (Trace *) malloc (sizeof(Trace));
	if (trace == NULL) {
		fprintf (stderr, "read_stgrid_trace: Malloc error on Trace structure.\n");
		free (data2);
		return (NULL);
	}
	trace->tstart = time;
	trace->dt = 1.0/samprate;
	trace->nsamps = nsamp;
	trace->calib = 1.0;
	trace->calper = 0.0;
	strcpy (trace->rawdata_format, dtype);
	strcpy (trace->rawdata_type, "V");
	trace->data = NULL;
	trace->data_free = NULL;
	trace->data_malloc = 0;
	trace->raw_data = (void *) data2;
	trace->rawdata_free = (void *) data2;
	if (data) trace->rawdata_malloc = nsamp*size; else trace->rawdata_malloc = 0;
	trace->prev = NULL;
	trace->next = NULL;
	if (data) trace = (Trace *) SCV_trace_fixgaps (trace, "segment");
	return (trace);
}

int
zaccess (char *path, int mode)

{
	char fname[1024];

	if (access(path, mode) == 0) return (0);
	sprintf (fname, "%s.Z", path);
	if (access(fname, mode) == 0) return (1);
	sprintf (fname, "%s.gz", path);
	if (access(fname, mode) == 0) return (2);
	return (-1);
}

Trace *
read_trace (Dbptr db, double tstart, double tend)

{
	char fname[1024];
	char dtype[8];
	char segtype[16];
	long foff, nsamp;
	float *data;
	Trace *trace;
	double time, samprate;
	double calib, calper;
	double ts, te;
	int size;
	int ret;

	if (dbextfile (db, "wfdisc", fname) < 1) {
		fprintf (stderr, "read_trace: Unable to find input file '%s'\n",
						fname);
		return (NULL);
	}
	dbgetv (db, 0, "time", &time, "samprate", &samprate,
				"nsamp", &nsamp, "datatype", dtype, 
				"segtype", segtype, "foff", &foff, 
				"calib", &calib, "calper", &calper, NULL);

	data = NULL;
	ret = trgetwf (db, NULL, &data, 0, tstart, tend,
				&ts, &te, &nsamp, 0, 0);
	size = 4;

	switch (ret) {
	default:
	case 0:
		if (ret < 0) data = NULL;
		if (ts == 0.0 && te == 0.0) data = NULL;
		break;

	case -9: /* no data */
	    clear_register (0); 
	    data = NULL;
	    break; 

	case -1:
	case -2:
	case -3:
	case -5:
	case -6:
	case -7:
	case -8:
		complain (0, 
		"read_trace: trgetwf() error.\n");
		return (NULL);
	}

	if (data == (void *) -1) {
		data = NULL;
	}

	if (data == (void *) -2) {
		clear_register (0);
		data = NULL;
	}
	trace = newtrace();
	if (trace == NULL) {
		fprintf (stderr, "read_trace: newtrace() error.\n");
		my_free (data);
		return (NULL);
	}
	trace->scv = NULL;
	trace->tstart = ts;
	trace->dt = 1.0/samprate;
	trace->nsamps = nsamp;
	trace->calib = calib;
	trace->calper = calper;
	strcpy (trace->rawdata_format, "t4");
	strcpy (trace->rawdata_type, segtype);
	strcpy (trace->input_units, "");
	strcpy (trace->output_units, "");
	trace->data = NULL;
	trace->data_free = NULL;
	trace->data_malloc = 0;
	trace->raw_data = data;
	trace->rawdata_free = (short int *)data;
	if (data) trace->rawdata_malloc = nsamp*size; else trace->rawdata_malloc = 0;
	trace->prev = NULL;
	trace->next = NULL;
	if (data) {
		trace = (Trace *) SCV_trace_fixgaps (trace, "segment");
	}
	return (trace);
}
