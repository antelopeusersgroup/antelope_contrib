#include <stdio.h>
#include <sys/param.h>

#include "stock.h"
#include "db.h"
#include "sac.h"

#define SAC_NULL_FLOAT -12345.0
#define SAC_NULL_INT   -12345
#define SAC_NULL_STRING   "-12345  "
#define SAC_STRING_LENGTH 8

void
usage ()
{
    fprintf (stderr, "\nUsage: [-v] %s sac_file [ sac_file .. ] database_name\n",
	     Program_Name);
    banner(Program_Name, 0L) ;
    exit (1);
}

int
swap_sac (sac_t * sachdr)
{
    float           x;
    int             intel;
    x = sachdr->delta;
    if (x == SAC_NULL_FLOAT
	    || (x > 1e-8 && x < 1e3)) {
	/* no swap */

#ifndef WORDS_BIGENDIAN
	intel = 1;
#else
	intel = 0;
#endif
    } else {
	/* swap */
	char           *s;
	s = (char *) sachdr;
	swap4 (s, s, 110);

#ifdef WORDS_BIGENDIAN
	intel = 1;
#else
	intel = 0;
#endif
    }
    return intel;
}

int
sac2db (char *sac_name, sac_t * sac, int intel, Dbptr db)
{
    double          samprate,
                    reftime,
                    time,
                    evtime,
                    endtime,
                    calib;
    long            nsamp;
    char            sta[15],
                    chan[15];
    double          hang,
                    vang;
    char            dir[STRSZ],
                    base[STRSZ];
    char            upper[STRSZ];
    Dbptr           dbe,
                    dbwfdisc,
                    dbo,
                    dba;
    long            i,
                    n;
    double          lat,
                    lon,
                    depth;
    long            arid,
                    evid,
                    orid;
    char            iphase[15];
    Tbl            *pieces;
    char            instype[20],
                    evname[20];
    char           *segtype;
    long            record,
                    norigin,
                    result;
    long            size;



    /* if ( sac->iftype != ITIME ) return -1 ;  */
    if (sac->nzyear == SAC_NULL_INT
	    || sac->nzjday == SAC_NULL_INT
	    || sac->nzhour == SAC_NULL_INT
	    || sac->nzmin == SAC_NULL_INT
	    || sac->nzsec == SAC_NULL_INT
	    || sac->nzmsec == SAC_NULL_INT) {
	elog_complain(0, "time not properly specified in %s\n", sac_name);
	return -1;
    }
    if (sac->nzyear < 100) {
	if (sac->nzyear < 70) {
	    sac->nzyear += 2000;
	} else {
	    sac->nzyear += 1900;
	}
	elog_complain(0, "NZYEAR has value in the first century\n\tchanging it to %d.", sac->nzyear);
    }
    reftime = h2e (sac->nzyear, sac->nzjday, sac->nzhour, sac->nzmin,
		   (double) sac->nzsec + .001 * (double) sac->nzmsec);
    if (sac->b != SAC_NULL_FLOAT) {
	time = reftime + sac->b;
    }
    if (sac->npts == SAC_NULL_INT) {
	elog_complain(0, "# data points (npts) undefined in %s\n", sac_name);
	return -1;
    }
    nsamp = sac->npts;


    if (sac->delta == SAC_NULL_FLOAT) {
	elog_complain(0, "Sample rate (1/delta) undefined in %s\n", sac_name);
	return -1;
    }
    samprate = 1.0 / sac->delta;

    endtime = time + (nsamp - 1) / samprate;

    calib = 0.0;		       /* the sac->scale field is NOT used!! */

    dirbase (sac_name, dir, base);

    if (strncmp (sac->kstnm, SAC_NULL_STRING, SAC_STRING_LENGTH) == 0) {
	elog_complain(0, "station name (kstnm) undefined in %s\n", sac_name);
	return -1;
    }
    copystrip (sta, sac->kstnm, SAC_STRING_LENGTH);

    if (strncmp (sac->kcmpnm, SAC_NULL_STRING, SAC_STRING_LENGTH) != 0) {
	copystrip (chan, sac->kcmpnm, SAC_STRING_LENGTH);
    } else {
	strcpy (upper, base);
	str2upper (upper);
	pieces = split (upper, '.');
	n = maxtbl (pieces);
	for (i = 0; i < n; i++) {
	    if (strcmp (gettbl (pieces, i), sta) == 0) {
		break;
	    }
	}
	switch (n - i) {

	  case 2:		       /* time.sta.chan */
	    strcpy (chan, gettbl (pieces, i + 1));
	    break;

	  case 3:		       /* time.sta.component.chan */
	    strcpy (chan, gettbl (pieces, i + 2));
	    strcat (chan, gettbl (pieces, i + 1));
	    break;

	  default:
	    elog_complain(0, "can't figure out channel name for %s\n", sac_name);
	    freetbl (pieces, 0);
	    return -1;
	}
	freetbl (pieces, 0);
    }

    if (strncmp (sac->kinst, SAC_NULL_STRING, SAC_STRING_LENGTH) != 0) {
	copystrip (instype, sac->kinst, SAC_STRING_LENGTH);
    } else {
	strcpy (instype, "-");
    }

    switch (sac->idep) {
      case IACC:
	segtype = "A";
	break;
      case IVEL:
	segtype = "V";
	break;
      case IDISP:
	segtype = "D";
	break;
      default:
	segtype = "-";
	break;
    }

    dbwfdisc = dblookup (db, 0, "wfdisc", 0, 0);
    dbwfdisc = dblookup (dbwfdisc, 0, 0, "dir", 0);
    dbquery (dbwfdisc, dbFIELD_SIZE, &size);
    if (strlen (dir) > size) {
	elog_complain(0, "'%s' is too large for %ld chars in dir field in wfdisc: please shorten it", dir, size);
	return -1;
    }
    dbwfdisc = dblookup (dbwfdisc, 0, 0, "dfile", 0);
    dbquery (dbwfdisc, dbFIELD_SIZE, &size);
    if (strlen (base) > size) {
	elog_complain(0, "'%s' is too large for %ld chars in dfile field in wfdisc: please shorten it", base, size);
	return -1;
    }
    if ((dbwfdisc.record = dbaddnull (dbwfdisc)) < 0) {
	elog_die(0, "couldn't add new record to wfdisc for '%s'\n", sac_name);
    }
    if (dbputv (dbwfdisc, 0,
		"sta", sta,
		"chan", chan,
		"time", time,
		"jdate", yearday (time),
		"endtime", endtime,
		"wfid", dbnextid (dbwfdisc, "wfid"),
		"nsamp", nsamp,
		"instype", instype,
		"segtype", segtype,
		"samprate", samprate,
		"calib", calib,
		"datatype", intel ? "ic" : "sc",
		"dir", dir,
		"dfile", base,
		NULL
		) < 0) {
	elog_die(0, "couldn't write to new null record #%ld in wfdisc for '%s'\n",
	     dbwfdisc.record, sac_name);
    } else {
	elog_clear_register(0);
    }

    if (sac->stla == SAC_NULL_FLOAT) {
	elog_complain(0, "station latitude is null in %s\n", sac_name);
    }
    if (sac->stlo == SAC_NULL_FLOAT) {
	elog_complain(0, "station longitude is null in %s\n", sac_name);
    }
    if (sac->stel == SAC_NULL_FLOAT) {
	elog_complain(0, "station elevation is null in %s\n", sac_name);
    }
    if (sac->user[7] != SAC_NULL_FLOAT
	    && sac->user[8] != SAC_NULL_FLOAT) {
	result = dbaddv (db, "site",
			 "sta", sta,
			 "ondate", yearday (reftime),
			 "lat", (double) sac->stla,
			 "lon", (double) sac->stlo,
			 "elev", (double) sac->stel / 1000.0,
			 "dnorth", (double) sac->user[7],
			 "deast", (double) sac->user[8],
			 NULL);
    } else {
	result = dbaddv (db, "site",
			 "sta", sta,
			 "ondate", yearday (reftime),
			 "lat", (double) sac->stla,
			 "lon", (double) sac->stlo,
			 "elev", (double) sac->stel / 1000.0,
			 NULL);
    }
    if (result < 0) {
	elog_complain(0, "couldn't add sta='%s' ondate=%ld lat=%10.3f lon=%10.3f elev=%10.3f from %s to site\n",
		  sta, yearday (reftime), sac->stla, sac->stlo, sac->stel / 1000.0, sac_name);
    } else {
	elog_clear_register(0);
    }

    if (sac->cmpaz != SAC_NULL_FLOAT
	    && sac->cmpinc != SAC_NULL_FLOAT) {
	hang = sac->cmpaz;
	vang = sac->cmpinc;
    } else {
	elog_complain(0, "channel orientation not present in %s\n", sac_name);
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
    }


    if (sac->stdp == SAC_NULL_FLOAT) {
	record = dbaddv (db, "sitechan",
			 "sta", sta,
			 "chan", chan,
			 "hang", hang,
			 "vang", vang,
			 "ondate", yearday (reftime),
			 NULL);
    } else {
	record = dbaddv (db, "sitechan",
			 "sta", sta,
			 "chan", chan,
			 "hang", hang,
			 "vang", vang,
			 "ondate", yearday (reftime),
			 "edepth", (double) sac->stdp / 1000.0,
			 NULL);
    }

    if (record < 0) {
	elog_complain(0, "couldn't add sta='%s' chan='%s' ondate=%ld hang=%10.3f vang=%10.3f edepth=%10.3f from %s to sitechan\n",
		  sta, chan, yearday (reftime), hang, vang, sac->stdp / 1000.0, sac_name);
    } else {
	elog_clear_register(0);
    }

    if (sac->evla != SAC_NULL_FLOAT
	    && sac->evlo != SAC_NULL_FLOAT
	) {
	lat = sac->evla;
	lon = sac->evlo;
	if (sac->o == SAC_NULL_FLOAT) {
	    if (sac->iztype != IO) {
		elog_complain(0, "Origin time not specified in %s, using reference time instead.\n", sac_name);
	    }
	    evtime = reftime;
	} else {
	    evtime = reftime + sac->o;
	}

	depth = sac->evdp / 1000.0;
	dbo = dblookup (db, 0, "origin", 0, 0);
	dbquery (dbo, dbRECORD_COUNT, &norigin);
	if (sac->evdp == SAC_NULL_FLOAT) {
	    dbo.record = dbNULL;
	    dbget (dbo, 0);
	    dbo.record = dbSCRATCH;
	    dbo.record = dbputv (dbo, 0,
				 "time", evtime,
				 "lat", lat,
				 "lon", lon,
				 "depth", 0.0,
				"nass", 0L,
				"ndef", 0L,
				 "jdate", yearday (evtime),
				 NULL);
	    dbo.record = dbadd (dbo, 0);
	} else
	    dbo.record = dbaddv (dbo, 0,
				 "time", evtime,
				 "lat", lat,
				 "lon", lon,
				 "depth", depth,
				"nass", 0L,
				"ndef", 0L,
				 "jdate", yearday (evtime),
				 NULL);

	if (dbo.record < 0) {
	    elog_complain(0, "couldn't add time=%s lat=%10.3f lon=%10.3f depth=%10.3f from %s to origin\n",
		      strtime (evtime), lat, lon, depth, sac_name);
	} else if (dbo.record >= norigin) {
	    /* add new event only when new origin has been added */
	    elog_clear_register(0);
	    dbgetv (dbo, 0, "orid", &orid, NULL);
	    dbe = dblookup (db, 0, "event", 0, 0);
	    copystrip (evname, sac->ko, SAC_STRING_LENGTH);
	    if (strncmp (evname, SAC_NULL_STRING, strlen (evname)) != 0) {
		dbe.record = dbaddv (dbe, 0,
				     "evname", evname,
				     "prefor", orid,
				     NULL);
	    } else {
		dbe.record = dbaddv (dbe, 0,
				     "prefor", orid,
				     NULL);
	    }

	    if (dbe.record < 0) {
		elog_complain(0, "couldn't add a new event for file %s\n", sac_name);
	    } else {
		elog_clear_register(0);
		dbgetv (dbe, 0, "evid", &evid, NULL);
		dbputv (dbo, "origin", "evid", evid, NULL);
	    }
	}
    } else {
	orid = -1;
    }

    dba = dblookup (db, 0, "arrival", 0, 0);
    if (sac->a != SAC_NULL_FLOAT) {
	if (strncmp (sac->ka, SAC_NULL_STRING, SAC_STRING_LENGTH) != 0
		&& !blank (sac->ka)) {
	    copystrip (iphase, sac->ka, SAC_STRING_LENGTH);
	} else {
	    strcpy (iphase, "SACA");
	}
	if ((dba.record = dbaddv (dba, "arrival",
				  "time", sac->a + reftime,
				  "sta", sta,
				  "iphase", iphase,
				  NULL)) < 0) {
	    elog_complain(0, "couldn't add sta='%s' time=%s iphase='%s' from %s to arrival\n",
		      sta, strtime (sac->a + reftime), iphase, sac_name);
	} else {
	    elog_clear_register(0);
	    dbputv (dba, 0, "chan", chan, NULL);
	}
    }
    for (i = 0; i < 10; i++) {
	if (sac->t[i] != SAC_NULL_FLOAT) {
	    if (strncmp (sac->kt[i], SAC_NULL_STRING, SAC_STRING_LENGTH) != 0
		    && !blank (sac->kt[i])) {
		copystrip (iphase, sac->kt[i], SAC_STRING_LENGTH);
	    } else {
		sprintf (iphase, "SAC%ld", i);
	    }

	    dba.record = dbaddv (dba, "arrival",
				 "time", sac->t[i] + reftime,
				 "sta", sta,
				 "iphase", iphase,
				 NULL);

	    if (dba.record < 0) {
		elog_complain(0, "couldn't add sta='%s' time=%s iphase='%s' from %s to arrival\n",
			  sta, strtime (sac->a + reftime), iphase, sac_name);
	    } else {
		dbputv (dba, 0, "chan", chan, NULL);
		if (orid >= 0) {
		    elog_clear_register(0);
		    dbgetv (dba, 0, "arid", &arid, NULL);
		    if (dbaddv (db, "assoc",
				"arid", arid,
				"orid", orid,
				"sta", sta,
				"phase", iphase,
				NULL) < 0) {
			elog_complain(0, "couldn't add arid=%ld orid=%ld %s to assoc\n",
				  arid, orid, sac_name);
		    } else {
			elog_clear_register(0);
		    }
		}
	    }
	}
    }

    return 0;
}

int
main (int argc, char **argv)
{
    int             c,
                    verbose = 0,
                    errflg = 0;
    long            i;
    char           *database_name,
                   *sac_name;
    char            absp[MAXPATHLEN];
    sac_t           sac_header;
    FILE           *file;
    Dbptr           db;
    int             intel,
                    absflag;

    elog_init (argc, argv);

    while ((c = getopt (argc, argv, "vV")) != -1) {
	switch (c) {

	  case 'v':
	    verbose++;
	    break;

	  case 'V':
	    usage() ;
	    break;

	  case '?':
	    errflg++;
	    break;
	}
    }

    if (errflg || argc - optind < 2)
	usage ();

    database_name = argv[argc - 1];

    /* flag for absolute path's in wfdisc */
    absflag = (strchr (database_name, '/') != 0);

    dbopen (database_name, "r+", &db);

    for (i = optind; i < argc - 1; i++) {
	if (verbose) {
	    printf (">%s\n", argv[i]);
	}
	if (absflag) {
	    abspath (argv[i], absp);
	    sac_name = absp;
	} else {
	    sac_name = argv[i];
	}
	if ((file = fopen (sac_name, "r")) == 0)
	    elog_complain(1, "Can't open sac file %s\n", sac_name);
	else {
	    if (fread (&sac_header, sizeof (sac_t), 1, file) != 1) {
		elog_complain(1, "Can't read sac file %s\n", sac_name);
	    } else {
		intel = swap_sac (&sac_header);
		if (sac2db (sac_name, &sac_header, intel, db) != 0)
		    elog_complain(0, "SAC file %s not incorporated.\n",
			      sac_name);
	    }
	    fclose (file);
	}
    }
    return 0;
}

/* $Id$ */
