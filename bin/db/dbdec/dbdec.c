
/*
 *
 *  NAME
 *	dbdec - Decimate waveform data
 *
 *  SYNOPSIS
 *	dbdec [-sift sift_expr] [-c calper] [-wfdir wfdir] [-f  for-
 *	mat] dbin dbout chan_maps dec_stage1 [dec_stage2 ...]
 *
 *  DESCRIPTION
 *	dbdec will decimate waveform data.  This program  will  only
 *	work with v. 3.0 of the CSS schema.
 *
 *  COMMAND LINE ARGUMENTS
 *	-sift sift_expr
 *	               A sifting expression  for  determining  which
 *	               waveform segments to process.  This should be
 *	               formed  as  a  normal  expression,  such   as
 *	               'sta   ==   "AAK"'  to  get  just  the
 *	               waveform segments for station AAK. Note  that
 *	               the  channel  mappings  also effectively sift
 *	               the data.  This argument is optional  and  if
 *	               it  is  ommited,  then all waveforms are pro-
 *	               cessed subject to the channel mappings.
 *	
 *	-c calper      If this argument is specified, then the calib
 *	               and  calper values in the output wfdisc table
 *	               will be recomputed according to the specified
 *	               calper  value.  In  addition  the  instrument
 *	               response files will be  modified  to  reflect
 *	               the  new  normalization value.  This argument
 *	               is optional and if it is  ommited,  then  the
 *	               calib, calper and instrument response normal-
 *	               izations  remain  unchanged.  Note  that  the
 *	               decimation  FIR stages are always included to
 *	               produce new instrument response files for the
 *	               decimated waveforms.
 *	
 *	-wfdir wfdir   The wfdir argument specifies where the output
 *	               waveform  files  are  to  be  stored. The dir
 *	               entry in the output wfdisc table is generated
 *	               by  concatenating the wfdir argument with the
 *	               dir entry from the input wfdisc table. If the
 *	               wfdir    specification   is   of   the   form
 *	               :stringi:stringo:, then a string substitution
 *	               is  attempted on the dir entry from the input
 *	               wfdisc table to form the  dir  entry  in  the
 *	               output   wfdisc   table.   This  argument  is
 *	               optional and if it is ommited, then the  out-
 *	               put  decimated  waveform  file directory tree
 *	               will be the same as the input tree. Note that
 *	               in  this  case, in order to avoid file colli-
 *	               sions, the  dir  path  in  the  input  wfdisc
 *	               tables  must  be  relative (not absolute) and
 *	               the output database wfdisc file must be in  a
 *	               different  directory  than  the  input wfdisc
 *	               file.
 *	
 *	-f format      The format for the decimated  waveform  data.
 *	               This  should be a normal datatype value, such
 *	               as  s2,  s4,  t4,  etc.   This  argument   is
 *	               optional  and if it is ommited, then the out-
 *	               put format is the same as the input format.
 *	
 *	dbin           The name of the input database. This argument
 *	               is required.
 *	
 *	dbout          The name of the output database.  This  argu-
 *	               ment is required.
 *	
 *	chan_maps      A list of channel code mappings.  This  is  a
 *	               comma  separated  list  of  the form 
 *                     outchan1=inchan1,outchan2=inchan2,...  with no
 *	               embedded white space that describes how input
 *	               channel codes are mapped  to  output  channel
 *	               codes.  Only  the input channels in this list
 *	               will be processed.   If  the  output  channel
 *	               code  is  the same as the input channel code,
 *	               then the output database, dbout, must be dif-
 *	               ferent  from  the  input  database,  dbin. An
 *	               example of this argument is
 *	               SHN=EHN,SHE=EHE,SHZ=EHZ
 *	               This argument is required.
 *	
 *	dec_stage [dec_stage2 ...]
 *	               A set of  decimation  stage  response  files.
 *	               This list of file names refers to one or more
 *	               decimation stages that are to be  applied  to
 *	               the   data.   Each  response  stage  file  is
 *	               assumed to contain  the  coefficients  for  a
 *	               normalized  FIR  filter  in CSS response file
 *	               format.  At least one stage  must  be  speci-
 *	               fied.
 *	
 *  SEE ALSO
 *	dbex_eval(3)
 *
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "db.h"
#include "arrays.h"
#include "scv2.h"
#include "response.h"
#include "stock.h"
#include "tr.h"

Trace *read_trace();
Trace *convert_trace();

int main(int argc, char **argv)
{
	char *sift_expr, *wfdir, *format, *dbin, *dbout, *chan_maps;
	double calper;
	int overwrite=0;
	int ndec_stages;
	char **dec_stages;
	Tbl *chan_in_tbl, *chan_out_tbl;
	Dbptr dbi, dbo;
	Dbvalue dbv;
	Response *resp;
	Tbl *ncoefs, *coefs, *dec_fac;
	int dec_factor;
	int nwi, n, i, j, nchans;
	char string[512];
	char expr[1024];
	Dbptr dbwfi;
	char dbbase[1024];
	char dir[128];
	char dfile[128];
	char fnamei[1024];
	char fnameo[1024];
	char wfdir1[512];
	char wfdir2[512];
	double time, tref;

	/* Get command line args */

	if (!getargs(argc, argv, &sift_expr, &calper, &wfdir, &format, &dbin,
				&dbout, &chan_maps, &ndec_stages, &dec_stages)) {
		usage();
		exit (1);
	}
	if (calper >= 0.0) {
		fprintf (stderr, "dbdec: -c option not operational.\n");
		usage();
		exit (1);
	}

	/* Parse channel maps and wfdir */

	if (!parse_chan_maps(chan_maps, &chan_in_tbl, &chan_out_tbl)) {
		fprintf (stderr, "dbdec: Unable to parse channel maps, '%s'\n",
						chan_maps);
		usage();
		exit (1);
	}
	if (!parse_wfdir(wfdir, wfdir1, wfdir2)) {
		fprintf (stderr, "dbdec: Unable to parse wfdir, '%s'\n",
						wfdir);
		usage();
		exit (1);
	}

	/* Open and read decimation stage files */

	if (!read_dec_files(ndec_stages, dec_stages, &resp, &dec_factor, &ncoefs, &coefs, &dec_fac)) {
		fprintf (stderr, "dbdec: Unable to read decimation stage(s).\n");
		usage();
		exit (1);
	}
	printf ("Total decimation factor = %d\n", dec_factor);

	/* Open the input database */

        if (dbopen (dbin, "r+", &dbi) == dbINVALID) {
                elog_clear_register(1);
                fprintf (stderr, "dbdec: Unable to open input database '%s'.\n",
									dbin);
                exit (1);
        }
	dbi = dblookup (dbi, 0, "wfdisc", 0, 0);
        dbquery (dbi, dbRECORD_COUNT, &nwi);
        if (nwi < 1) {
        	fprintf (stderr, "dbdec: No wfdisc rows for input database '%s'.\n",
									dbin);
                exit (1);
        }

	/* Compose a subset expression for the input channel mappings */

	nchans = maxtbl (chan_in_tbl);
	for (i=0; i<nchans; i++) {
		sprintf (string, "chan == \"%s\"", gettbl(chan_in_tbl, i));
		if (i == 0) {
			strcpy (expr, "(");
			strcat (expr, string);
		} else {
			strcat (expr, " || ");
			strcat (expr, string);
		}
	}
	strcat (expr, ")");
	if (sift_expr) {
		strcat (expr, " && (");
		strcat (expr, sift_expr);
		strcat (expr, ")");
	}

	/* Subset the input wfdisc table */

	dbwfi = dblookup (dbi, 0, "wfdisc", 0, 0);
	dbwfi = dbsubset (dbwfi, expr, 0);
        dbquery (dbwfi, dbRECORD_COUNT, &n);
        if (n < 1) {
        	fprintf (stderr, "dbdec: No input channels for database '%s'.\n",
									dbin);
                exit (1);
        }
	printf ("Processing %d out of %d waveform segments.\n", n, nwi);

	/* Open the output database */

        if (dbopen (dbout, "r+", &dbo) == dbINVALID) {
                elog_clear_register(1);
                fprintf (stderr, "dbdec: Unable to open output database '%s'.\n",
									dbout);
                exit (1);
        }
	dbo = dblookup (dbo, 0, "wfdisc", 0, 0);
        dbquery (dbo, dbTABLE_DIRNAME, &dbv);
        strcpy (dbbase, dbv.t);

	/* Make a pass through the input wfdiscs to look for wf file conflicts */

	if (!overwrite) {
		printf ("Looking for waveform file conflicts...");
		fflush (stdout);
	}
	tref = 1.e30;
	for (dbwfi.record=0; dbwfi.record<n; dbwfi.record++) {
		if (!overwrite && (!strcmp(wfdir1,wfdir2)) ){
			if (!makeoutfname (dbwfi, wfdir1, wfdir2, dbbase, dir, dfile, fnameo)) {
				fprintf (stderr, "dbdec: makeoutfname() error.\n");
				exit (1);
			}
			if (zaccess(fnameo, F_OK) != -1) {
				fprintf (stderr, "\ndbdec(Warning): wf file conflict for '%s'.\n",
									fnameo);
				fprintf(stderr,"Will append but may waste space\n");
			}
		}
		dbgetv (dbwfi, 0, "time", &time, 0);
		if (time < tref) tref = time;
	}
	if (!overwrite) {
		printf ("OK\n");
	}
	tref = (double)((int)tref);

	/* Loop through and do the decimation */

	for (dbwfi.record=0; dbwfi.record<n; dbwfi.record++) {
		Trace *trace=NULL;

		/* Read in trace and convert to float */

		SCV_free_trace (trace);
		trace = NULL;
		trace = read_trace (dbwfi);
		if (trace == NULL) {
			fprintf (stderr, "dbdec: read_trace() error.\n");
			continue;
		}

		/* Decimate float trace */

		if (!decimate_trace (trace, ncoefs, coefs, dec_fac, tref)) {
			fprintf (stderr, "dbdec: decimate_trace() error.\n");
			exit (1);
		}

		/* Convert trace to output units and put back in data gaps */

		trace = convert_trace (trace, format);
		if (trace == NULL) {
			fprintf (stderr, "dbdec: convert_trace() error.\n");
			exit (1);
		}

		/* Write out decimated trace and wfdisc */

		makeoutfname (dbwfi, wfdir1, wfdir2, dbbase, dir, dfile, fnameo);
		if (!write_trace (dbwfi, dbo, dir, dfile, fnameo, 
					chan_in_tbl, chan_out_tbl, trace, overwrite)) {
			fprintf (stderr, "dbdec: write_trace() error.\n");
			exit (1);
		}
	}

	/* Fix up the output sensor, sitechan, etc. tables */

	if (!fixup_tables (dbi, dbo, chan_in_tbl, chan_out_tbl, calper, resp, dec_factor)) {
		fprintf (stderr, "dbdec: fixup_tables() error.\n");
		exit (1);
	}

	/* Normal exit */

	exit (0);
}

int
fixup_tables (dbi, dbo, chan_in, chan_out, calper, resp, dec_factor)

Dbptr         dbi;
Dbptr              dbo;
Tbl *                   chan_in;
Tbl *                            chan_out;
double                                     calper;
Response *                                         resp;
int                                                      dec_factor;

{
	int i, nchans, n;
	char string[128], expr[128];
	Dbptr dbsi, dbji;
	Stbl *sta_stbl=NULL;

	/* Compose a subset expression for the input channel mappings */

	nchans = maxtbl (chan_in);
	for (i=0; i<nchans; i++) {
		sprintf (string, "chan == \"%s\"", gettbl(chan_in, i));
		if (i == 0) {
			strcpy (expr, "(");
			strcat (expr, string);
		} else {
			strcat (expr, " || ");
			strcat (expr, string);
		}
	}
	strcat (expr, ")");

	/* Subset the input sensor table */

	dbsi = dblookup (dbi, 0, "sensor", 0, 0);
	dbsi = dbsubset (dbsi, expr, 0);
        dbquery (dbsi, dbRECORD_COUNT, &n);
        if (n < 1) {
        	fprintf (stderr, "fixup_tables: No input sensor rows.\n");
        	return (0);
        }

        /* Join with sitechan and instrument tables */

	dbi = dblookup (dbi, 0, "sitechan", 0, 0);
	dbji = dbjoin (dbsi, dbi, 0, 0, 0, 0, 0);
	dbi = dblookup (dbi, 0, "instrument", 0, 0);
	dbji = dbjoin (dbji, dbi, 0, 0, 0, 0, 0);
        dbquery (dbji, dbRECORD_COUNT, &n);
        if (n < 1) {
        	fprintf (stderr, "fixup_tables: No input join rows.\n");
        	return (0);
        }
        sta_stbl = newstbl (NULL);
        if (sta_stbl == NULL) {
        	fprintf (stderr, "fixup_tables: newstbl() error.\n");
        	return (0);
        }

	/* Loop through the joined input view */

	for (dbji.record=0; dbji.record<n; dbji.record++) {
		char sta[32], chani[32], chano[32], instant[8], ctype[8];
		char descrip[64], insname[64], instype[32];
		char band[8], digital[8], dir[128], dfile[64], rsptype[8];
		double time, endtime, calratio, scalper, tshift;
		double edepth, hang, vang, samprate, ncalib, ncalper;
		int jdate, ondate, offdate;
		int chanid, inid;
		char dfileo[64];
		char *ptr;

		/* Read in everything */

		dbgetv (dbji, 0,	"sensor.sta", sta,
					"sensor.chan", chani,
					"sensor.time", &time,
					"sensor.endtime", &endtime,
					"sensor.jdate", &jdate,
					"sensor.calratio", &calratio,
					"sensor.calper", &scalper,
					"sensor.tshift", &tshift,
					"sensor.instant", instant,
					"sitechan.ondate", &ondate,
					"sitechan.offdate", &offdate,
					"sitechan.ctype", ctype,
					"sitechan.edepth", &edepth,
					"sitechan.hang", &hang,
					"sitechan.vang", &vang,
					"sitechan.descrip", descrip,
					"instrument.insname", insname,
					"instrument.instype", instype,
					"instrument.band", band,
					"instrument.digital", digital,
					"instrument.samprate", &samprate,
					"instrument.ncalib", &ncalib,
					"instrument.ncalper", &ncalper,
					"instrument.dir", dir,
					"instrument.dfile", dfile,
					"instrument.rsptype", rsptype,
					0);

		/* Translate channel code */

		for (i=0; i<nchans; i++) {
			strcpy (chano, gettbl(chan_out, i));
			if (!strcmp(chani, gettbl(chan_in, i))) break;
		}
		if (i == nchans) {
			fprintf (stderr, "fixup_tables: Unable to map input channel '%s'.\n", chani);
			return (0);
		}

		/* Make a new response file */

		samprate /= dec_factor;
		if (!make_response_file (dbi, dbo, dir, dfile, dfileo, resp, dec_factor, samprate)) {
			fprintf (stderr, "fixup_tables: make_response_file() error.\n");
			return (0);
		}

		/* Append instrument stuff */

		sprintf (string, "%s:d%d", insname, dec_factor);
		inid = merge_instrument (dbo, string, instype, band, digital, samprate, ncalib, ncalper,
		                dir, dfileo, rsptype );
		if (inid < 1) {
			fprintf (stderr, "fixup_tables: merge_instrument() error.\n");
			return (0);
		}

		/* Append sitechan stuff */

		chanid = merge_sitechan (dbo, sta, chano, ondate, offdate, ctype, edepth, hang, vang,
		                descrip);
		if (chanid < 1) {
			fprintf (stderr, "fixup_tables: merge_sitechan() error.\n");
			return (0);
		}

		/* Append sensor stuff */

		if (!merge_sensor (dbo, sta, chano, time, endtime, inid, chanid, jdate,
		                calratio, calper, tshift, instant)) {
			fprintf (stderr, "fixup_tables: merge_sensor() error.\n");
			return (0);
		}

		/* Add station name to list */

		ptr = strdup(sta);
		if (tststbl (sta_stbl, ptr)) {
			free (ptr);
		} else {
			addstbl (sta_stbl, ptr);
		}
	}

	/* Copy the site table */

	dbi = dblookup (dbi, 0, "site", 0, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        for (dbi.record=0; dbi.record<n; dbi.record++) {
        	char sta[32], staname[64], statype[8], refsta[32];
        	double lat, lon, elev, dnorth, deast;
        	int ondate, offdate;

        	dbgetv (dbi, 0,	"sta", sta,
        			"ondate", &ondate,
        			"offdate", &offdate,
        			"lat", &lat,
        			"lon", &lon,
        			"elev", &elev,
        			"staname", staname,
        			"statype", statype,
        			"refsta", refsta,
        			"dnorth", &dnorth,
        			"deast", &deast,
        			0);
		if (!tststbl (sta_stbl, sta)) continue;
		if (!merge_site (dbo, sta, ondate, offdate, lat, lon, elev, 1.0, staname, statype,
		                refsta, dnorth, deast)) {
			fprintf (stderr, "fixup_tables: merge_site() error.\n");
			return (0);
		}
        }

	return (1);
}

int
make_response_file (dbi, dbo, diri, dfilei, dfileo, resp, dec_factor, samprate)

Dbptr               dbi;
Dbptr                    dbo;
char *                        diri;
char *                              dfilei;
char *                                      dfileo;
Response *                                          resp;
int                                                       dec_factor;
double                                                                samprate;

{
	Dbvalue dbv;
	char dbbase[1024];
	char fname[1024];
	char string[1024];
	char dfile[1024];
	FILE *file;
	Response *rspi, *rsp;
	int i, n, ret;

	/* Read in input response file */

	dbi = dblookup (dbi, 0, "instrument", 0, 0);
        dbquery (dbi, dbTABLE_DIRNAME, &dbv);
        strcpy (dbbase, dbv.t);
	if (diri[0] == '/') {
		sprintf (fname, "%s/%s", diri, dfilei);
	} else {
		sprintf (fname, "%s/%s/%s", dbbase, diri, dfilei);
	}
	file = fopen(fname, "r");
	if (file == NULL) {
		fprintf (stderr, "make_response_file: Unable to open input response file '%s'.\n",
						fname);
		return (0);
	}
	if (read_response (file, &rspi)) {
		elog_clear_register(1);
		fprintf (stderr, "make_response_file: read_response() error on response file '%s'.\n",
						fname);
		return (0);
	}
	fclose (file);

	/* Make output response */

	get_response_nstages (resp, &n);
	for (i=0; i<n; i++) {
		Response_group *gpi;

		gpi = resp->groups + i;
		if (copy_response_group (gpi, rspi, -1) < 0) {
			fprintf (stderr, "make_response_file: copy_response_group() error.\n");
			return (0);
		}
	}
	fixsrate (rspi, samprate);

	/* Compare output response with existing output responses */

	ret = comp_responses (dbo, rspi, &rsp);
	if (ret < 0) {
		elog_clear_register(1);
		fprintf (stderr, "make_response_file: comp_responses() error.\n");
		return (0);
	}
	if (!ret) {
		strcpy (dfileo, rsp->dfile);
		free_response (rspi);
		return (1);
	}

	/* Write out response file */

	strcpy (string, dfilei);
	for (i=strlen(string)-1; i>=0; i--) if (string[i] == '.') break;
	if (i > 0) string[i] = '\0';
	sprintf (fname, "%s_d%d", string, dec_factor);
	strcpy (string, fname);
	if (diri[0] == '/') {
		sprintf (dbbase, "%s", diri);
	} else {
		dbo = dblookup (dbo, 0, "instrument", 0, 0);
        	dbquery (dbo, dbTABLE_DIRNAME, &dbv);
        	strcpy (dbbase, dbv.t);
		sprintf (dbbase, "%s/%s", dbv.t, diri);
	}
	i = 1;
	while (1) {
		sprintf (dfile, "%s.%d", string, i++);
		if (strlen(dfile) > 32) {
			strcpy (fname, &dfile[strlen(dfile)-32]);
			strcpy (dfile, fname);
		}
		sprintf (fname, "%s/%s", dbbase, dfile);
		if (access(fname, F_OK) < 0) break;
	}
	dirbase (fname, string, dbbase);
	if (makedir(string) == -1) {
                fprintf (stderr, "make_response_file: Unable to create %s\n", string);
                return (0);
        }
	file = fopen(fname, "w");
	if (file == NULL) {
		fprintf (stderr, "make_response_file: Unable to open output response file '%s'.\n",
						fname);
		return (0);
	}
	if (write_response (file, rsp)) {
		elog_clear_register(1);
		fprintf (stderr, "make_response_file: write_response() error on response file '%s'.\n",
						fname);
		return (0);
	}
	fclose (file);
	strcpy (dfileo, dfile);
	rsp->dfile = strdup (dfileo);

        /* Normal exit */

	return (1);
}

int comp_responses (Dbptr db, Response *resp, Response **respo)
{
	static Stbl *resp_stbl=NULL;
	int n;
	char fname[1024];
	Response *rsp;
	FILE *file;

	int compare_response();

	if (!resp_stbl) {
		resp_stbl = newstbl(compare_response);
		if (resp_stbl == NULL) {
			fprintf (stderr, "comp_responses: newstbl() error.\n");
			return (-1);
		}
		db = dblookup (db, 0, "instrument", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		for (db.record=0; db.record<n; db.record++) {
			if (dbfilename (db, fname) < 1) {
				fprintf (stderr, "comp_responses: Unable to find input file '%s'\n",
								fname);
				return (-1);
			}
			file = fopen(fname, "r");
			if (file == NULL) {
				fprintf (stderr, "comp_responses: Unable to open input response file '%s'.\n",
								fname);
				return (-1);
			}
			if (read_response (file, &rsp)) {
				elog_clear_register(1);
				fprintf (stderr, "comp_responses: read_response() error on response file '%s'.\n",
								fname);
				return (-1);
			}
			fclose (file);
			dbgetv (db, 0, "dfile", fname, 0);
			rsp->dfile = strdup(fname);
			if (tststbl(resp_stbl, rsp)) {
				free_response(rsp);
			} else {
				addstbl (resp_stbl, rsp);
			}
		}
	}
	if (((*respo)=(Response *)tststbl(resp_stbl, resp))) {
		return (0);
	} else {
		(*respo) = (Response *)addstbl(resp_stbl, resp);
		return (1);
	}
}

int
write_trace (dbi, dbo, dir, dfile, fname, chan_in, chan_out, trace, overwrite)

Dbptr        dbi;
Dbptr             dbo;
char *                 dir;
char *                      dfile;
char *                             fname;
Tbl *                                     chan_in;
Tbl *                                              chan_out;
Trace *                                                      trace;
int                                                                 overwrite;

{
	FILE *fp;
	char outdir[1024];
	char outbase[1024];
	char sta[32], chani[32], chano[32], instype[32], segtype[8];
	char clip[8];
	int chanid;
	double calib, calper;
	int i, n, size, ret;
	long int foff;

        dbgetv (dbi, 0, "sta", sta, "chan", chani, "chanid", &chanid,
        		"calib", &calib, "calper", &calper,
        		"instype", instype, "segtype", segtype,
        		"clip", clip, 0);
	n = maxtbl(chan_in);
	for (i=0; i<n; i++) {
		strcpy (chano, gettbl(chan_out, i));
		if (!strcmp(chani, gettbl(chan_in, i))) break;
	}
	if (i == n) {
		fprintf (stderr, "write_trace: Unable to map input channel '%s'.\n", chani);
		return (0);
	}
	dirbase (fname, outdir, outbase);
	if (makedir(outdir) == -1) {
                fprintf (stderr, "write_trace: Unable to create %s\n", outdir);
                return (0);
        }
	/* Shortcoming here.  Converted from older code here to allow the 
	program to append to files.  Have not bothered to allow the program
	to support writing output in miniseed (sd format) */
	fp = fopen(fname, "a");
	if(fp==NULL)
	{
		fprintf (stderr, "write_trace: Open error on '%s'.\n", fname);
		return (0);
	}
	fseek(fp,0L,SEEK_END);
	foff = ftell(fp);
	size = atoi(&trace->rawdata_format[strlen(trace->rawdata_format)-1]);
	ret = fwrite(trace->raw_data,size,trace->nsamps,fp);
	fclose (fp);
	if(ret != (trace->nsamps)) {
		fprintf (stderr, "write_trace: Write error on '%s'.\n", fname);
		return (0);
	}
        dbo.record = dbNULL;
        dbo.field = dbALL;
        dbget(dbo, 0);
        dbo.record = dbSCRATCH;
	dbputv (dbo, 0,	"sta", sta,
			"chan", chano,
			"time", trace->tstart,
			"wfid", dbnextid(dbo, "wfid"),
			"chanid", chanid,
			"jdate", yearday(trace->tstart),
			"endtime", trace->tstart+trace->dt*(trace->nsamps-1),
			"nsamp", trace->nsamps,
			"samprate", 1.0/trace->dt,
			"calib", calib,
			"calper", calper,
			"instype", instype,
			"segtype", segtype,
			"datatype", trace->rawdata_format,
			"clip", clip,
			"dir", dir,
			"dfile", dfile,
			"foff",foff,
			0);
	dbadd (dbo, 0);
	return (1);
}

Trace *convert_trace(Trace *trace, char *format)
{
	Trace *tr;

	if (format) {
		for (tr=trace; tr!=NULL; tr=tr->next) {
			strcpy (tr->rawdata_format, format);
		}
	}
	trace = (Trace *) SCV_trace_toraw (trace, 1);
	trace = (Trace *) SCV_trace_fillgaps (trace);
	return (trace);
}

int
decimate_trace (trace, ncoefs, coefs, dec_fac, tref)

Trace *trace;
Tbl *ncoefs;
Tbl *coefs;
Tbl *dec_fac;
double tref;

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
convsym (datain, nsin, coefs, ncoefs, decfac, ioff, nsout, dataout)

float *datain;
int nsin;
float *coefs;
int ncoefs;
int decfac;
int ioff;
int *nsout;
float *dataout;

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

Trace *
read_trace (db)

Dbptr db;

{
	char dtype[8];
	int foff, nsamp,npts;
	float *data;
	Trace *trace;
	double time, endtime, dt, samprate;
	double t0,t1;

	if( dbgetv (db, 0, "time", &time, "endtime", &endtime, "samprate", &samprate,
		"nsamp", &nsamp, "datatype", dtype, "foff", &foff, 0) == dbINVALID)
	{
		fprintf(stderr,"read_trace:  dbgetv error reading row %d\n",db.record);
		return(NULL);
	}
	allot(float *,data,nsamp);
	/* The +dt assurs that the data read are always long enough*/
        dt=1.0/samprate;
	if(trgetwf(db,0,&data,&nsamp,time,endtime+dt,&t0,&t1,&npts,0,0))
	{
		fprintf(stderr,"trgetwf error for row %d of input db\n",db.record);
		return(NULL);
	}
	if(nsamp>npts)
        {
		char sta[10],chan[10];
                dbgetv(db,0,"sta",sta,"chan",chan,0);
		fprintf(stderr,"Warning:  %s:%s at start time %s\nExpected to read %d samples, got only %d\n",
			sta,chan,strtime(time),nsamp,npts);
	}
	trace = (Trace *) malloc (sizeof(Trace));
	if (trace == NULL) {
		fprintf (stderr, "read_trace: Malloc error on Trace structure.\n");
		free (data);
		return (NULL);
	}
	trace->tstart = time;
	trace->dt = 1.0/samprate;
	trace->nsamps = nsamp;
	/* Added to support miniseed format.  This may not work around gaps
	correctly depending on how the trgetwf routine handles this. */
	if(!strcmp(dtype,"sd"))
		strcpy (trace->rawdata_format,"t4");
	else
		strcpy (trace->rawdata_format, dtype);
		
	trace->data = NULL;
	trace->data_free = NULL;
	trace->data_malloc = 0;
	trace->raw_data = data;
	trace->rawdata_free = data;
	trace->rawdata_malloc = 1;
	trace->prev = NULL;
	trace->next = NULL;
	trace = (Trace *) SCV_trace_fixgaps(trace, "segment");
        trace = (Trace *) SCV_trace_tofloat(trace, 1);
	return (trace);
}

int zaccess(char *path, int mode)
{
        char fname[1024];

        if (access(path, mode) == 0) return (0);
        sprintf (fname, "%s.Z", path);
        if (access(fname, mode) == 0) return (1);
        sprintf (fname, "%s.gz", path);
        if (access(fname, mode) == 0) return (2);
        return (-1);
}


int
makeoutfname (dbwfi, wfdir1, wfdir2, dbbase, dir, dfile, fname)

Dbptr dbwfi;
char *wfdir1;
char *wfdir2;
char *dbbase;
char *dir;
char *dfile;
char *fname;

{
	char *ptr;
	int i, j;

	dbgetv (dbwfi, 0, "dir", dir, "dfile", dfile, 0);
	if (wfdir1[0] == '\0') {
		if (dir[0] == '/') {
			sprintf (fname, "%s/%s", dir, dfile);
		} else {
			sprintf (fname, "%s/%s/%s", dbbase, dir, dfile);
		}
		return (1);
	}
	if (wfdir2[0] == '\0') {
		sprintf (fname, "%s%s", wfdir1, dir);
		strcpy (dir, fname);
		if (dir[0] == '/') {
			sprintf (fname, "%s/%s", dir, dfile);
		} else {
			sprintf (fname, "%s/%s/%s", dbbase, dir, dfile);
		}
		return (1);
    	}
    	ptr = strstr (dir, wfdir1);
    	if (ptr == NULL) {
    		fprintf (stderr, "\nmakeoutfname: Unable to match %s.\n",
    						wfdir1);
		return (0);
    	}
    	for (i=0; i<strlen(dir); i++) 
    			if (&dir[i] == ptr) break; else fname[i] = dir[i];
	for (j=0; j<strlen(wfdir2); j++) fname[i+j] = wfdir2[j];
	for (j+=i,i+=strlen(wfdir1); i<strlen(dir); i++) fname[j++] = dir[i];
	fname[j] = '\0';
	strcpy (dir, fname);
	if (dir[0] == '/') {
		sprintf (fname, "%s/%s", dir, dfile);
	} else {
		sprintf (fname, "%s/%s/%s", dbbase, dir, dfile);
	}

    	return (1);
}

int
read_dec_files (ndec_stages, dec_stages, resp, dec_fac, ncoefs, coefs, decfac)

int ndec_stages;
char **dec_stages;
Response **resp;
int *dec_fac;
Tbl **ncoefs;
Tbl **coefs;
Tbl **decfac;

{
	int i, j, n;
	Response *rsp;
	char string[512];
	FILE *file;

	*resp = (Response *) new_response ();
	if (*resp == NULL) {
		fprintf (stderr, "read_dec_files: Malloc error on decimation response structure.\n");
		return (0);
	}
	for (i=0,(*dec_fac)=1; i<ndec_stages; i++) {
		int ok;

		file = fopen(dec_stages[i], "r");
		if (file == NULL) {
			fprintf (stderr, "read_dec_files: Unable to open response stage file '%s'.\n",
							dec_stages[i]);
			return (0);
		}
		if (read_response (file, &rsp)) {
			elog_clear_register(1);
			fprintf (stderr, "read_dec_files: read_response() error on stage file '%s'.\n",
							dec_stages[i]);
			return (0);
		}
		fclose (file);
		get_response_nstages (rsp, &n);
		for (j=0,ok=0; j<n; j++) {
			int dec_factor, nnum, nden;
			double srate;

			get_response_stage_type (rsp, j, string);
			if (strcmp(string, "fir")) continue;
			get_response_stage_fir_ncoefs (rsp, j, &srate, &dec_factor, &nnum, &nden);
			if (nden > 1) {
				fprintf (stderr, "read_dec_files: Dont know how to do IIR filters (%s).\n",
										dec_stages[i]);
				return (0);
			}
			if (nnum < 1) {
				fprintf (stderr, "read_dec_files: No numerator terms (%s).\n",
										dec_stages[i]);
				return (0);
			}
			ok=1;
			(*dec_fac) *= dec_factor;
		}
		if (!ok) {
			fprintf (stderr, "read_dec_files: no fir stage on file '%s'.\n",
							dec_stages[i]);
			return (0);
		}
		for (j=0; j<n; j++) {
			Response_group *gpi;

			get_response_stage_type (rsp, j, string);
			if (strcmp(string, "fir")) continue;
			gpi = rsp->groups + j;
			if (copy_response_group (gpi, (*resp), -1) < 0) {
				fprintf (stderr, "read_dec_files: copy_response_group() error.\n");
				return (0);
			}
		}
		free_response (rsp);
	}
	get_response_nstages ((*resp), &n);
	*ncoefs = newtbl (n);
	*coefs = newtbl (n);
	*decfac = newtbl (n);
	for (i=0; i<n; i++) {
		Response_group *gpi;
		int dec_factor, nnum, nden, n2;
		double srate;
		double *coefsi, *coefs_err;
		double *coefdi, *coefd_err;
		float *cfs;
		int *numb, *decf;

		gpi = (*resp)->groups + i;
		strcpy (gpi->author, "dbdec");
		get_response_stage_fir_ncoefs ((*resp), i, &srate, &dec_factor, &nnum, &nden);
		get_response_stage_fir_coefs ((*resp), i, &nnum, &coefsi, &coefs_err,
						&nden, &coefdi, &coefd_err);
		for (j=0; j<nnum/2; j++) if (coefsi[j] != coefsi[nnum-j-1]) break;
		if (j < nnum/2) {
			fprintf (stderr, "read_dec_files: Can only do symetrical FIR filters.\n");
			return (0);
		}
		n2 = nnum/2+1;
		cfs = (float *) malloc (n2*sizeof(float));
		if (cfs == NULL) {
			fprintf (stderr, "read_dec_files: Malloc error.\n");
			return (0);
		}
		for (j=0; j<n2; j++) cfs[j] = coefsi[j+n2-1];
		numb = (int *) malloc (sizeof(int));
		if (numb == NULL) {
			fprintf (stderr, "read_dec_files: Malloc error.\n");
			return (0);
		}
		*numb = n2;
		decf = (int *) malloc (sizeof(int));
		if (decf == NULL) {
			fprintf (stderr, "read_dec_files: Malloc error.\n");
			return (0);
		}
		*decf = dec_factor;
		settbl (*coefs, -1, cfs);
		settbl (*ncoefs, -1, numb);
		settbl (*decfac, -1, decf);
	}
	return (1);
}

int
parse_wfdir (wfdir, wfdir1, wfdir2)

char *wfdir;
char *wfdir1;
char *wfdir2;

{
	int i, j;

	strcpy (wfdir1, "");
	strcpy (wfdir2, "");
	if (wfdir) {
		if (wfdir[0] == ':') {
			for (j=0,i=1; i<strlen(wfdir); i++) if (wfdir[i] == ':') break; else wfdir1[j++]=wfdir[i];
			if (i == strlen(wfdir)) {
				return (0);
			}
			wfdir1[j] = '\0';
			for (j=0,i++; i<strlen(wfdir); i++) if (wfdir[i] == ':') break; else wfdir2[j++]=wfdir[i];
			if (i != strlen(wfdir)-1) {
				return (0);
			}
			wfdir2[j] = '\0';
		} else {
			strcpy (wfdir1, wfdir);
		}
	}
	return (1);
}

int
parse_chan_maps (chan_maps, chan_in_tbl, chan_out_tbl)

char *chan_maps;
Tbl **chan_in_tbl;
Tbl **chan_out_tbl;

{
	char *ptr;
	int i, j;

	*chan_in_tbl = newtbl (10);
	if (*chan_in_tbl == NULL) {
		elog_clear_register(1);
		fprintf (stderr, "parse_chan_maps: newtbl() error.\n");
		return (0);
	}
	*chan_out_tbl = newtbl (10);
	if (*chan_out_tbl == NULL) {
		elog_clear_register(1);
		fprintf (stderr, "parse_chan_maps: newtbl() error.\n");
		return (0);
	}
	j = 0;
	for (ptr=strtok(chan_maps, ","); ptr!=NULL; ptr=strtok(NULL, ",")) {
		for (i=0; i<strlen(ptr)-1; i++) if (ptr[i] == '=') break;
		if (i == 0 || i >= strlen(ptr)-1) {
			fprintf (stderr, "parse_chan_maps: Unable to parse '%s'\n",
								ptr);
			return (1);
		}
		ptr[i] = '\0';
		i += 1;
		if (settbl (*chan_out_tbl, j, ptr) != j) {
			elog_clear_register(1);
			fprintf (stderr, "parse_chan_maps: settbl() error.\n");
			return (0);
		}
		if (settbl (*chan_in_tbl, j, &ptr[i]) != j) {
			elog_clear_register(1);
			fprintf (stderr, "parse_chan_maps: settbl() error.\n");
			return (0);
		}
		j++;
	}
	return (1);
}

int
getargs (argc, argv, sift_expr, calper, wfdir, format, dbin,
	 dbout, chan_maps, ndec_stages, dec_stages)

int argc;
char **argv;
char **sift_expr;
double *calper;
char **wfdir;
char **format;
char **dbin;
char **dbout;
char **chan_maps;
int *ndec_stages;
char ***dec_stages;

{
	double atof();

	*sift_expr = NULL;
	*calper = -1.0;
	*wfdir = NULL;
	*format = NULL;
	for (argc--,argv++; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-sift")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "dbdec: No -sift argument.\n");
				return (0);
			}
			*sift_expr = *argv;
		} else if (!strcmp(*argv, "-c")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "dbdec: No -c argument.\n");
				return (0);
			}
			*calper = atof(*argv);
		} else if (!strcmp(*argv, "-wfdir")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "dbdec: No -wfdir argument.\n");
				return (0);
			}
			*wfdir = *argv;
		} else if (!strcmp(*argv, "-f")) {
			argc--; argv++;
			if (argc < 1) {
				fprintf (stderr, "dbdec: No -f argument.\n");
				return (0);
			}
			*format = *argv;
		} else {
			break;
		}
	}
	if (argc < 1) {
		fprintf (stderr, "dbdec: No dbin argument.\n");
		return (0);
	}
	*dbin = *argv;
	argc--; argv++;
	if (argc < 1) {
		fprintf (stderr, "dbdec: No dbout argument.\n");
		return (0);
	}
	*dbout = *argv;
	argc--; argv++;
	if (argc < 1) {
		fprintf (stderr, "dbdec: No chan_maps argument.\n");
		return (0);
	}
	*chan_maps = *argv;
	argc--; argv++;
	if (argc < 1) {
		fprintf (stderr, "dbdec: No dec_stages arguments.\n");
		return (0);
	}
	*ndec_stages = argc;
	*dec_stages = argv;
	return (1);
}

usage()
{
        cbanner("$Revision$", 
		"dbdec [-sift sift_expr] [-c calper] [-wfdir wfdir]\n"
		"             [-f format] dbin dbout chan_maps dec_stage1\n"
		"             [dec_stage2 ...]\n",
		"Gary Pavlis", 
		"Indiana University", 
		"pavlis@geology.indiana.edu" ) ; 
}

/* $Id$ */
