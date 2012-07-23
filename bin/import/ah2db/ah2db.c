/*  convert ah-xdr format files to css3.0 w/Antelope schema current as of 11/94
  Modified from sac2db, 11/21/94 G Abers
 */
#include <stdio.h>
#include <sys/param.h>
#include <rpc/rpc.h>
#include <math.h>
#include <ctype.h>

#include "ahhead.h"
#include "stock.h"
#include "db.h"
#include "response.h"

double          ah_2_epoch (struct ah_time * time);
char           *progname;

Response_group *
                new_response_group (void);

/* size of header in ah- xdr */
#define AH_XDR_HEAD_SIZE 1080L

void
usage (void)
{
    fprintf (stderr, "Usage: %s ah_file [ ah_file .. ] database_name\n    Only uses first ah per file \n",
	     Program_Name);
    exit (1);
}

void
cp2upper (char *out, char *in)
{
    while ((*out++ = toupper (*in++)) != 0);
}

double
ah_2_epoch (struct ah_time * time)
{
    return h2e ((int) time->yr,
		mday2doy ((int) time->yr,
			  (int) time->mo,
			  (int) time->day),
		(int) time->hr,
		(int) time->mn,
		(double) time->sec);
}

Response_group *
paz_group_fill (ahhed * ah)
{
    /* This allocates the memory and fills in the poles and zero (paz)
     * structure. From DanQ 1/9/95 */

    Response_group *group;
    Paz            *paz;
    long             i;

    group = new_response_group ();
    allot (Paz *, paz, 1);
    group->id = PAZ;
    group->pvt = (void *) paz;
    group->srcid = THEORETICAL;
    strcpy (group->description, "ahPoleZero");	/* maximum 12 characters */
    strcpy (group->author, "From ah2db");	/* maximum 44 characters */

    paz->normalization = ah->station.A0;	/* normalization factor */
    paz->frequency = 0.;	       /* frequency (hz) at which stage is
				        * normalized; UNKNOWN */

    paz->npoles = (int) (ah->station.cal[0].pole.r);
    paz->nzeros = (int) (ah->station.cal[0].zero.r);
    if ((paz->npoles == 0) && (paz->nzeros == 0)) {
	complain (0, "No calibration info present");
	return group;
    }
    allot (Complex_t *, paz->poles, paz->npoles);
    allot (Complex_t *, paz->zeros, paz->nzeros);
    allot (Complex_t *, paz->zero_errors, paz->nzeros);
    allot (Complex_t *, paz->pole_errors, paz->npoles);

    for (i = 0; i < paz->npoles; i++) {
	paz->poles[i].real = (double) ah->station.cal[i + 1].pole.r;
	paz->poles[i].imag = (double) ah->station.cal[i + 1].pole.i;
	paz->pole_errors[i].real = 0.0;
	paz->pole_errors[i].imag = 0.0;
    }

    for (i = 0; i < paz->nzeros; i++) {
	paz->zeros[i].real = (double) ah->station.cal[i + 1].zero.r;
	paz->zeros[i].imag = (double) ah->station.cal[i + 1].zero.i;
	paz->zero_errors[i].real = 0.0;
	paz->zero_errors[i].imag = 0.0;
    }

    return group;
}

int
putinst (ahhed * ah, double timemin, Dbptr db)
{
    Dbptr           dbi;
    double          samprate;
    char            insname[STRSZ];
    long             stypesz;
    double          calib;
    long             inid;
    char            dir[STRSZ],
                    base[STRSZ],
                    filename[STRSZ],
                    response_fbase[STRSZ];
    char           *response_dir = "response";
    FILE           *file;
    Response       *response;
    Response_group *group;

    stypesz = (long) STYPESIZE - 1;
    copystrip (insname, ah->station.stype, stypesz);
    samprate = rint (100. / ah->record.delta) / 100.;	/* ROUNDED off */
    /* calib is supposed to be nm/count; just for fun assume DS is count/m */
    calib = 0.0;
    if (ah->station.DS != 0.0) {
	calib = 1e+9 / ah->station.DS;
    }

    dbi = dblookup (db, 0, "instrument", 0, 0);
    dbi.record = dbaddv (dbi, 0,
			 "insname", insname,
			 "samprate", samprate,
			 "ncalib", calib,
			 NULL );
    if (dbi.record < 0) {
	complain (0, "Could not make instrument entry\n");
	return (-1);
    } else {
	clear_register (0);
	dbgetv (dbi, 0, "inid", &inid, NULL );
    }

    sprintf (response_fbase, "resp_ah_%ld\0", inid);
    dbputv (dbi, 0,
	    "dir", response_dir,
	    "dfile", response_fbase,
	    NULL );

    /* get new response */
    response = new_response ();
    group = paz_group_fill (ah);
    add_response_group (response, group);

    /* Set up/get output file */
    dbfilename (dbi, filename);
    dirbase (filename, dir, base);
    if (makedir (dir) != 0) {
	complain (1, "Unable to open/create response directory %s\n", dir);
	return (-1);
    }
    if ((file = fopen (filename, "w+")) == 0) {
	complain (1, "Could not open response file %s\n", filename);
	return (-2);
    }
    if (write_response (file, response) < 0) {
	complain (1, "Can't write response file '%s'\n", filename);
	return (-3);
    }
    if ( fclose (file) ) { 
	complain (1, "failed to close response file %s", filename ) ; 
    }

    return (inid);
}

int
getchan (Dbptr db, char *chan, char *sta)
/* returns record # for first sta/chan combo.  dba must already point to appropriate
	table.  e.g., run
	db = dblookup(db,0, "sitechan", 0, 0);
	irec = getchan(db, chan, sta);
*/
{
    Dbptr           dba;
    long             i,
                    n,
                    iret;
    Dbvalue         dval;
    char            sta1[16],
                    chan1[32];

    iret = -1;
    dba = db;
    dbquery (dba, dbRECORD_COUNT, &dval);
    n = dval.i;
    if (n == 0) {
	return (-1);
    }
    for (i = 0; i < n; i++) {
	dba.record = i;
	dbgetv (dba, 0,
		"sta", sta1,
		"chan", chan1,
		NULL );
	if (!(strcmp (sta1, sta) || strcmp (chan1, chan))) {
	    return (i);
	}
    }
    return (iret);
}

int
getorig (Dbptr db, double lat, double lon, double dep, double ot)
/* returns record # for first event match.
*/
{
    Dbptr           dba;
    long             i,
                    n,
                    iret;
    Dbvalue         dval;
    double          alat,
                    alon,
                    adep,
                    aot;

    iret = -1;
    dba = dblookup (db, 0, "origin", 0, 0);
    dbquery (dba, dbRECORD_COUNT, &dval);
    n = dval.i;
    if (n == 0) {
	return (-1);
    }
    for (i = 0; i < n; i++) {
	dba.record = i;
	dbgetv (dba, 0,
		"lat", &alat,
		"lon", &alon,
		"depth", &adep,
		"time", &aot,
		NULL );
	if ((alat == lat) && (alon == lon) && (adep == dep) && (aot = ot)) {
	    return (i);
	}
    }
    return (iret);
}


int
ah2db (XDR * xdr_name, ahhed * ah, char *ah_name, double timemin, Dbptr db)
{
    double          samprate,
                    time,
                    evtime,
                    endtime,
                    calib;
    long             nsamp;
    char            sta[15],
                    chan[15];
    double          hang,
                    vang;
    char            dir[STRSZ],
                    base[STRSZ];
    Dbptr           dbe,
                    dbo,
                    dbw,
                    dbsen,
                    dba;
    long             comsz,
                    jday1,
                    irec;
    double          lat,
                    lon,
                    depth;
    long             evid,
                    chanid,
                    inid,
                    orid,
                    nev1;
    char            instype[20],
                    evname[20];
    char           *segtype;
    char            ahcode[CODESIZE];
    char            ahchan[CHANSIZE];
    Dbvalue         dval;
    long             size;


    time = ah_2_epoch (&ah->record.abstime);
    jday1 = yearday (timemin);

    nsamp = ah->record.ndata;


    if (ah->record.delta == 0.0) {
	complain (0, "Sample rate (1/delta) undefined \n");
	return -1;
    }
    samprate = rint (10000. / ah->record.delta) / 10000.;	/* some rounding off */

    endtime = time + (nsamp - 1) / samprate;

    calib = 0.0;		       /* calib is supposed to be nm/count;
				        * just for fun assume DS is count/m */
    if (ah->station.DS != 0.0) {
	calib = 1e+9 / ah->station.DS;
    }

    dirbase (ah_name, dir, base);

    cp2upper (ahcode, ah->station.code);
    cp2upper (ahchan, ah->station.chan);
    copystrip (sta, ahcode, CODESIZE - 1);
    copystrip (chan, ahchan, CHANSIZE - 1);
    copystrip (instype, ah->station.stype, STYPESIZE - 1);
    instype[6] = 0;

    dbw = db = dblookup (db, 0, "wfdisc", 0, 0);
    db = dblookup (db, 0, "wfdisc", "dir", 0);
    dbquery (db, dbFIELD_SIZE, &size);
    if (strlen (dir) > size) {
	complain (0, "'%s' is too large for %ld chars in dir field in wfdisc: please shorten it", dir, size);
	return -1;
    }
    db = dblookup (db, 0, 0, "dfile", 0);
    dbquery (db, dbFIELD_SIZE, &size);
    if (strlen (base) > size) {
	complain (0, "'%s' is too large for %ld chars in dfile field in wfdisc: please shorten it", base, size);
	return -1;
    }
    segtype = "-";		       /* don't usually know units of AH data */

    dbw.record = dbaddv (db, "wfdisc",
			 "sta", sta,
			 "chan", chan,
			 "time", time,
			 "jdate", yearday (time),
			 "endtime", endtime,
			 "nsamp", nsamp,
			 "instype", instype,
			 "segtype", segtype,
			 "samprate", samprate,
			 "calib", calib,
			 "datatype", "t4",
			 "dir", dir,
			 "dfile", base,
			 "foff", AH_XDR_HEAD_SIZE,
			 NULL );
    if (dbw.record < 0) {
	complain (0, "couldn't add %s to wfdisc\n", ah_name);
	return (-1);
    }
    dba = dblookup (db, 0, "site", "sta", sta);
    if (dba.record == dbINVALID) {
	if (dbaddv (db, "site",
		    "sta", sta,
		    "ondate", jday1,
		    "lat", (double) ah->station.slat,
		    "lon", (double) ah->station.slon,
		    "elev", (double) ah->station.elev / 1000.,
		    NULL ) < 0) {
	    complain (0, "couldn't add %s to site\n", ah_name);
	} else {
	    clear_register (0);
	}
    }
    /* try to figure out channel */
    dba = dblookup (db, 0, "sitechan", 0, 0);
    irec = getchan (dba, chan, sta);
    if (irec < 0) {
	switch (chan[strlen (chan) - 1]) {
	  case 'Z':
	    hang = 0.0;
	    vang = 0.0;
	    break;

	  case 'N':
	    hang = 0.0;
	    vang = 90.0;
	    break;

	  case 'E':
	    hang = 90.0;
	    vang = 90.0;
	    break;

	  default:
	    hang = -1.0;
	    vang = -1.0;
	    break;
	}

	dba = dblookup (db, 0, "sitechan", 0, 0);
	dba.record = dbaddv (db, "sitechan",
			     "sta", sta,
			     "chan", chan,
			     "hang", hang,
			     "vang", vang,
			     "ondate", jday1,
			     NULL );
	if (dba.record < 0) {
	    complain (0, "couldn't add %s to sitechan\n", ah_name);
	} else {
	    clear_register (0);
	    dbgetv (dba, 0, "chanid", &chanid, NULL );
	    dbputv (dbw, "wfdisc", "chanid", chanid, NULL );
	}
    } else {
	dba.record = irec;
	dbgetv (dba, 0, "chanid", &chanid, NULL );
	dbputv (dbw, "wfdisc", "chanid", chanid, NULL );
    }

    /* RESPONSE Information */
    if (((long) (ah->station.cal[0].pole.r) > 0) || ((long) (ah->station.cal[0].zero.r) > 0)) {
	dbsen = dblookup (db, 0, "sensor", 0, 0);
	irec = getchan (dbsen, chan, sta);
	if (irec < 0) {
	    dbsen.record = dbaddv (db, "sensor",
				   "sta", sta,
				   "chan", chan,
				   "time", epoch (jday1),
				   "jdate", jday1,
				   "instant", "y",
				   NULL );
	    if (dbsen.record < 0) {
		complain (0, "couldn't add %s to sensor\n", ah_name);
	    } else {
		clear_register (0);
		if (dba.record >= 0) {
		    dbputv (dbsen, "sensor", "chanid", chanid, NULL );
		}
	    }

	    /* Put In Instrument response:  Only if this is a new STA/CHAN in
	     * sensor */
	    inid = putinst (ah, timemin, db);
	    if ((dbsen.record >= 0) && (inid > 0)) {
		dbputv (dbsen, "sensor", "inid", inid, NULL );
	    }
	}
    }
    /* EVENT Information */
    if (ah->event.ot.yr > 0) {
	lat = ah->event.lat;
	lon = ah->event.lon;
	depth = ah->event.dep;
	evtime = ah_2_epoch (&ah->event.ot);
	irec = getorig (db, lat, lon, depth, evtime);
	if (irec < 0) {
	    dbo = dblookup (db, 0, "origin", 0, 0);
	    dbquery (dbo, dbRECORD_COUNT, &dval);
	    nev1 = dval.i;
	    dbo.record = dbaddv (dbo, 0,
				 "time", evtime,
				 "lat", lat,
				 "lon", lon,
				 "depth", depth,
				 "jdate", yearday (evtime),
				 "ndef", 0L,
				 "nass", 0L,
				 NULL );
	    if (dbo.record < 0) {
		complain (0, "couldn't add %s to origin\n", ah_name);
	    } else {
		clear_register (0);
		dbquery (dbo, dbRECORD_COUNT, &dval);
		if (nev1 != dval.i) {
		    dbgetv (dbo, 0, "orid", &orid, NULL );
		    comsz = (long) COMSIZE - 1;
		    comsz = 19;	       /* hack to try to avoid memory
				        * trashing */
		    copystrip (evname, ah->event.ecomment, comsz);
		    dbe = dblookup (db, 0, "event", 0, 0);
		    dbe.record = dbaddv (dbe, 0,
					 "evname", evname,
					 "prefor", orid,
					 NULL );
		    if (dbe.record < 0) {
			complain (0, "couldn't add %s to event\n", ah_name);
		    } else {
			clear_register (0);
			dbgetv (dbe, 0, "evid", &evid, NULL );
			dbputv (dbo, "origin", "evid", evid, NULL );
		    }
		}
	    }
	}
    } else
	orid = -1;


    return 0;
}

int
main (int argc, char **argv)
{

    long             i;
    char           *database_name,
                   *ah_name;
    char            absp[MAXPATHLEN];
    ahhed           ah_header;
    XDR             xdr_in;
    FILE           *file;
    Dbptr           db;
    int             absflag,
                    ii;
    double          time,
                    timemin;

    Program_Name = argv[0];

    if (argc < 3) {
	usage ();
    }

    database_name = argv[argc - 1];

    /* flag for absolute path's in wfdisc */
    absflag = strchr (database_name, '/');

    /* find earliest ondate -- use to start all ondates  */
    for (i = 1; i < argc - 1; i++) {
	if (absflag) {
	    abspath (argv[i], absp);
	    ah_name = absp;
	} else
	    ah_name = argv[i];
	if ((file = fopen (ah_name, "r")) == 0) {
	    complain (1, "Can't open ah file %s\n", ah_name);
	} else {
	    xdrstdio_create (&xdr_in, file, XDR_DECODE);
	    ii = 1;
	    if (xdr_gogethead (ii, &ah_header, &xdr_in) != 0) {
		complain (1, "Can't read ah header %s\n", ah_name);
	    }
	    time = ah_2_epoch (&ah_header.record.abstime);
	    if (i == 1) {
		timemin = time;
	    }
	    if (time < timemin) {
		timemin = time;
	    }
	    xdr_destroy (&xdr_in);
	    if ( fclose (file)) { 
		complain (1, "Failed to close file %s", ah_name ) ; 
	    }
	}
    }

    /* Load database now */
    dbopen (database_name, "r+", &db);

    for (i = 1; i < argc - 1; i++) {
	if (absflag) {
	    abspath (argv[i], absp);
	    ah_name = absp;
	} else {
	    ah_name = argv[i];
	}
	if ((file = fopen (ah_name, "r")) == 0) {
	    complain (1, "Can't open ah file %s\n", ah_name);
	} else {
	    xdrstdio_create (&xdr_in, file, XDR_DECODE);
	    ii = 1;
	    if (xdr_gogethead (ii, &ah_header, &xdr_in) != 0) {
		complain (1, "Can't read ah header %s\n", ah_name);
	    } else if (ah2db (&xdr_in, &ah_header, ah_name, timemin, db) != 0) {
		complain (0, "AH file %s not incorporated.\n",
			  ah_name);
	    }
	    xdr_destroy (&xdr_in);
	    if ( fclose (file)) { 
		complain (1, "Failed to close file %s", ah_name ) ; 
	    }
	}
    }
    return 0;
}

