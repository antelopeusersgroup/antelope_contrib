
/*
 *	These subroutine constitute a database merging facility
 *	that allow new candidate tuples to be merged into an
 *	existing database in an efficient manner.
 */

#include <stdio.h>
#include <string.h>

#include "db.h"
#include "arrays.h"

/*
 *	These are the "comparison" structures for each table in
 *	the database. Each structure contains the info necessary
 *	to determine if two tuples logically match within the
 *	database.
 */

struct affiliation_cmp_ {
	char sta[8];
	char net[16];
	Dbptr db;
	int ituple;
};

struct site_cmp_ {
	char sta[8];
	int ondate;
	int offdate;
	double lat;
	double lon;
	double elev;
        double dnorth;
        double deast;
        double edepth;
	Dbptr db;
	int ituple;
};

struct sitechan_cmp_ {
	char sta[8];
	char chan[16];
	int ondate;
	int offdate;
	double hang;
	double vang;
	double edepth;
	Dbptr db;
	int ituple;
	int chanid;
};

struct instrument_cmp_ {
	char dir[128];
	char dfile[64];
	char rsptype[8];
	double samprate;
	double ncalib;
	double ncalper;
	Dbptr db;
	int ituple;
	int inid;
};

struct sensor_cmp_ {
	char sta[8];
	char chan[16];
	int inid;
	int chanid;
	double time;
	double endtime;
	double calratio;
	double calper;
	double tshift;
	Dbptr db;
	int ituple;
};

/*
 *	These are proceedures for creating the comparison
 *	structures.
 */

struct affiliation_cmp_ *create_acmp();
struct site_cmp_ *create_scmp();
struct sitechan_cmp_ *create_sccmp();
struct instrument_cmp_ *create_icmp();
struct sensor_cmp_ *create_sncmp();

struct affiliation_cmp_ *
create_acmp (sta, net, db, ituple)

char *       sta;
char *            net;
Dbptr                  db;
int                        ituple;

{
	struct affiliation_cmp_ *acmp;

	acmp = (struct affiliation_cmp_ *) malloc (sizeof(struct affiliation_cmp_));
	if (acmp == NULL) return (NULL);
	strcpy (acmp->sta, sta);
	strcpy (acmp->net, net);
	acmp->db = db;
	acmp->ituple = ituple;
	return (acmp);
}

struct site_cmp_ *
create_scmp (sta, ondate, offdate, lat, lon, elev, dnorth, deast, edepth, db, ituple)

char *       sta;
int               ondate;
int                       offdate;
double                             lat;
double                                  lon;
double                                       elev;
double                             dnorth;
double                                  deast;
double                                       edepth;
Dbptr                                              db;
int                                                    ituple;

{
	struct site_cmp_ *scmp;

	scmp = (struct site_cmp_ *) malloc (sizeof(struct site_cmp_));
	if (scmp == NULL) return (NULL);
	strcpy (scmp->sta, sta);
	scmp->ondate = ondate;
	scmp->offdate = offdate;
	scmp->lat = lat;
	scmp->lon = lon;
	scmp->elev = elev;
	scmp->dnorth = dnorth;
	scmp->deast = deast;
	scmp->edepth = edepth;
	scmp->db = db;
	scmp->ituple = ituple;
	return (scmp);
}

struct sitechan_cmp_ *
create_sccmp (sta, chan, ondate, offdate, hang, vang, edepth, db, ituple, chanid)

char *       sta;
char *             chan;
int                      ondate;
int                              offdate;
double                                    hang;
double                                         vang;
double                                              edepth;
Dbptr                                                     db;
int                                                           ituple;
int                                                                      chanid;

{
	struct sitechan_cmp_ *sccmp;

	sccmp = (struct sitechan_cmp_ *) malloc (sizeof(struct sitechan_cmp_));
	if (sccmp == NULL) return (NULL);
	strcpy (sccmp->sta, sta);
	strcpy (sccmp->chan, chan);
	sccmp->ondate = ondate;
	sccmp->offdate = offdate;
	sccmp->hang = hang;
	sccmp->vang = vang;
	sccmp->edepth = edepth;
	sccmp->db = db;
	sccmp->ituple = ituple;
	sccmp->chanid = chanid;
	return (sccmp);
}

struct instrument_cmp_ *
create_icmp (dir, dfile, rsptype, samprate, ncalib, ncalper, db, ituple, inid)

char *       dir;
char *            dfile;
char *                   rsptype;
double                            samprate;
double                                      ncalib;
double                                              ncalper;
Dbptr                                                        db;
int                                                              ituple;
int                                                                      inid;

{
	struct instrument_cmp_ *icmp;

	icmp = (struct instrument_cmp_ *) malloc (sizeof(struct instrument_cmp_));
	if (icmp == NULL) return (NULL);
	strcpy (icmp->dir, dir);
	strcpy (icmp->dfile, dfile);
	strcpy (icmp->rsptype, rsptype);
	icmp->samprate = samprate;
	icmp->ncalib = ncalib;
	icmp->ncalper = ncalper;
	icmp->db = db;
	icmp->ituple = ituple;
	icmp->inid = inid;
	return (icmp);
}

struct sensor_cmp_ *
create_sncmp (sta, chan, time, endtime, inid, chanid, calratio, calper, tshift, db, ituple)

char *       sta;
char *             chan;
double                   time;
double                         endtime;
int                                     inid;
int                                          chanid;
double                                               calratio;
double                                                         calper;
double                                                                  tshift;
Dbptr                                                                          db;
int                                                                                ituple;

{
	struct sensor_cmp_ *sncmp;

	sncmp = (struct sensor_cmp_ *) malloc (sizeof(struct sensor_cmp_));
	if (sncmp == NULL) return (NULL);
	strcpy (sncmp->sta, sta);
	strcpy (sncmp->chan, chan);
	sncmp->time = time;
	sncmp->endtime = endtime;
	sncmp->inid = inid;
	sncmp->chanid = chanid;
	sncmp->calratio = calratio;
	sncmp->calper = calper;
	sncmp->tshift = tshift;
	sncmp->db = db;
	sncmp->ituple = ituple;
	return (sncmp);
}

/*
 *	These are the comparison proceedures for each table.
 */

int
compare_affiliation (s1, s2)

struct affiliation_cmp_ *s1, *s2;

{
	return(strcmp(s1->sta, s2->sta));
}

int
compare_site (s1, s2)

struct site_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	if (s1->offdate < s2->ondate) return (-1);
	if (s1->ondate > s2->offdate) return (1);
	if (s1->ondate == s2->offdate) 
		if (!match(s1->lat,s2->lat,4) || !match(s1->lon,s2->lon,4) || 
                    !match(s1->elev,s2->elev,4) || !match(s1->dnorth,s2->dnorth,4) || 
                    !match(s1->deast,s2->deast,4) || !match(s1->edepth,s2->edepth,4)) 
                    return (1);
	if (s2->ondate == s1->offdate) 
		if (!match(s1->lat,s2->lat,4) || !match(s1->lon,s2->lon,4) || 
                    !match(s1->elev,s2->elev,4) || !match(s1->dnorth,s2->dnorth,4) ||
                    !match(s1->deast,s2->deast,4) || !match(s1->edepth,s2->edepth,4))
                     return (-1);
	return (0);
}

int
compare_sitechan (s1, s2)

struct sitechan_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	if (s1->offdate < s2->ondate) return (-1);
	if (s1->ondate > s2->offdate) return (1);
	if (s1->ondate == s2->offdate) 
		if (!match(s1->hang,s2->hang,1) || !match(s1->vang,s2->vang,1) 
				|| !match(s1->edepth,s2->edepth,4)) return (1);
	if (s2->ondate == s1->offdate) 
		if (!match(s1->hang,s2->hang,1) || !match(s1->vang,s2->vang,1) 
				|| !match(s1->edepth,s2->edepth,4)) return (-1);
	return (0);
}

int
compare_instrument (s1, s2)

struct instrument_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->dfile, s2->dfile);
	if (i) return (i);
	i = strcmp(s1->dir, s2->dir);
	if (i) return (i);
	i = strcmp(s1->rsptype, s2->rsptype);
	if (i) return (i);
	i = dbldif(s1->samprate,s2->samprate,7);
	if (i) return (i);
	i = dbldif(s1->ncalib,s2->ncalib,6);
	if (i) return (i);
	i = dbldif(s1->ncalper,s2->ncalper,6);
	if (i) return (i);
	return (0);
}

int
compare_sensor (s1, s2)

struct sensor_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	if (s1->endtime < s2->endtime-1.0) return (-1);
	if (s1->time > s2->endtime+1.0) return (1);
	if (s1->time == s2->endtime) 
		if (s1->inid != s2->inid || s1->chanid != s2->chanid
			|| !match(s1->calratio,s2->calratio,6) || !match(s1->calper,s2->calper,6) 
				|| !match(s1->tshift,s2->tshift,2)) return (1);
	if (s2->time == s1->endtime) 
		if (s1->inid != s2->inid || s1->chanid != s2->chanid
			|| !match(s1->calratio,s2->calratio,6) || !match(s1->calper,s2->calper,6) 
				|| !match(s1->tshift,s2->tshift,2)) return (-1);
	return (0);
}

int
match (dbl1, dbl2, n)
 
double dbl1, dbl2;
int n;
 
{
        double t;
        int i;
 
        for (i=0,t=0.5; i<n; i++) t *= 0.1;
        if (dbl1 > dbl2-t && dbl1 < dbl2+t) return (1);
        return (0);
}

int
dbldif (dbl1, dbl2, n)
 
double dbl1, dbl2;
int n;
 
{
        double t;
        int i;
 
        for (i=0,t=0.5; i<n; i++) t *= 0.1;
        if (dbl1 < dbl2-t) return (-1);
        if (dbl1 > dbl2+t) return (1);
        return (0);
}

/*
 *	These are the merging subroutines for each table.
 */

int
merge_affiliation (db, sta, net )

Dbptr           db;
char *              sta;
char *                    net;

{
	Dbvalue value;

	static Stbl *affiliation_stbl=NULL;
	static int ok_write;
	struct affiliation_cmp_ *acmp;
	struct affiliation_cmp_ *acmp2;
	char *ptr;

	int compare_affiliation();

/*
 *	Create affiliation sorted table
 */
	if (affiliation_stbl == NULL) {
		int n;

		affiliation_stbl = newstbl (compare_affiliation);
		if (affiliation_stbl == NULL) {
			fprintf (stderr, "merge_affiliation: Error creating affiliation sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "affiliation", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], neti[32];

			dbgetv (db, 0,	"sta", stai,
					"net", neti,
					0);
			if (!merge_affiliation (db, stai, neti )) {
				fprintf (stderr, "merge_affiliation: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing affiliation table.
 */
        db = dblookup (db, 0, "affiliation", 0, 0);
	acmp = create_acmp (sta, net, db, -1);
	if (acmp == NULL) {
		fprintf (stderr, "merge_affiliation: Malloc error.\n");
		return (0);
	}
/*
 *	Add affiliation compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = addstbl (affiliation_stbl, acmp);
	if (ptr == NULL) {
		fprintf (stderr, "merge_affiliation: Error return from addstbl.\n");
		return (0);
	}
	acmp2 = (struct affiliation_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (acmp != acmp2) {
		if (strcmp(acmp->net, acmp2->net)) {
			fprintf (stderr, 
		"merge_affiliation: sta match with mismatch in net.\n");
			free (acmp);
			return (0);
		}
		free (acmp);
		return (1);
	}
/*
 *	No match in sorted table.
 *	Add new affiliation tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		acmp->ituple = value.i-1;
 		return (1);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"net", net,
		0);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	acmp->ituple = value.i-1;
	return (1);
}

int
merge_site (db, sta, ondate, offdate, lat, lon, elev, edepth, staname, statype,
		refsta, dnorth, deast )

Dbptr     db;
char *        sta;
int                ondate;
int                        offdate;
double                              lat;
double                                   lon;
double                                        elev;
double                                        edepth;
char *                                              staname;
char *                                                       statype;
char *         refsta;
double                  dnorth;
double                          deast;

{
	int i, n;
	Dbvalue value;
	int offdat;

	static Stbl *site_stbl=NULL;
	static int ok_write;
	struct site_cmp_ *scmp;
	struct site_cmp_ *scmp2;
	char *ptr;

	int compare_site();

/*
 *	Create site sorted table
 */

	if (site_stbl == NULL) {
		int n;

		site_stbl = newstbl (compare_site);
		if (site_stbl == NULL) {
			fprintf (stderr, "merge_site: Error creating site sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "site", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], stanamei[64], statypei[8], refstai[32];
			double lati, loni, elevi, dnorthi, deasti;
			int ondatei, offdatei;

			dbgetv (db, 0,	"sta", stai,
					"ondate", &ondatei,
					"offdate", &offdatei,
					"lat", &lati,
					"lon", &loni,
					"elev", &elevi,
					"staname", stanamei,
					"statype", statypei,
					"refsta", refstai,
					"dnorth", &dnorthi,
					"deast", &deasti,
					0);
			if (!merge_site (db, stai, ondatei, offdatei, lati, loni, elevi, edepth, stanamei, statypei,
										refstai, dnorthi, deasti )) {
				fprintf (stderr, "merge_site: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing site table.
 */
        db = dblookup (db, 0, "site", 0, 0);
	offdat = offdate;
	if (offdat < 0) offdat = 9999999;
	scmp = create_scmp (sta, ondate, offdat, lat, lon, elev, dnorth, deast, edepth, db, -1);
	if (scmp == NULL) {
		fprintf (stderr, "merge_site: Malloc error.\n");
		return (0);
	}
/*
 *	Add site compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = addstbl (site_stbl, scmp);
	if (ptr == NULL) {
		fprintf (stderr, "merge_site: Error return from addstbl.\n");
		return (0);
	}
	scmp2 = (struct site_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (scmp != scmp2) {
		if (scmp->ondate >= scmp2->ondate && scmp->offdate <= scmp2->offdate) {
			if (!match(scmp->lat,scmp2->lat,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in lat.\n");
				free (scmp);
				return (0);
			}
			if (!match(scmp->lon,scmp2->lon,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in lon.\n");
				free (scmp);
				return (0);
			}
			if (!match(scmp->elev,scmp2->elev,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in elev.\n");
				free (scmp);
				return (0);
			}
			if (!match(scmp->dnorth,scmp2->dnorth,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in dnorth.\n");
				free (scmp);
				return (0);
			}
			if (!match(scmp->deast,scmp2->deast,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in deast.\n");
				free (scmp);
				return (0);
			}
			if (!match(scmp->edepth,scmp2->edepth,4)) {
				fprintf (stderr, 
		"merge_site: sta-ondate-offdate match with mismatch in edepth.\n");
				free (scmp);
				return (0);
			}
			free (scmp);
			return (1);
		}
		if (scmp->ondate >= scmp2->ondate && scmp->ondate < scmp2->offdate) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			free (scmp);
			return (0);
		}
		if (scmp->offdate > scmp2->ondate && scmp->offdate <= scmp2->offdate) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			free (scmp);
			return (0);
		}
		if (scmp->ondate < scmp2->ondate && scmp->offdate > scmp2->offdate) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			free (scmp);
			return (0);
		}
		if (scmp->ondate == scmp2->offdate) {
		     if (match(scmp->lat,scmp2->lat,4) && match(scmp->lon,scmp2->lon,4) && 
                        match(scmp->elev,scmp2->elev,4) && match(scmp->dnorth,scmp2->dnorth,4) &&
                        match(scmp->deast,scmp2->deast,4) && match(scmp->edepth,scmp2->edepth,4)) {
				scmp2->offdate = offdat;
				scmp2->db.record = scmp2->ituple;
				dbputv (scmp2->db, 0,
					"offdate", offdate,
					0);
				free (scmp);
				return (1);
			}
		}
		if (scmp->offdate == scmp2->ondate) {
			if (match(scmp->lat,scmp2->lat,4) && match(scmp->lon,scmp2->lon,4) 
			&& match(scmp->elev,scmp2->elev,4) && match(scmp->dnorth,scmp2->dnorth,4)
                        && match(scmp->deast,scmp2->deast,4) && match(scmp->edepth,scmp2->edepth,4)
                ) {
				scmp2->ondate = ondate;
				scmp2->db.record = scmp2->ituple;
				dbputv (scmp2->db, 0,
					"ondate", ondate,
					0);
				free (scmp);
				return (1);
			}
		}
	}
/*
 *	No match in sorted table.
 *	Add new site tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		scmp->ituple = value.i-1;
 		return (1);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"ondate", ondate,
		"offdate", offdate,
		"lat", lat,
		"lon", lon,
		"elev", elev,
		"dnorth", dnorth,
		"deast", deast,
		"staname", staname,
		"statype", statype,
		"refsta", refsta,
		"dnorth", dnorth,
		"deast", deast,
		0);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	scmp->ituple = value.i-1;
	return (1);
}

int
merge_sitechan (db, sta, chan, ondate, offdate, ctype, edepth, hang, vang,
		descrip )

Dbptr     db;
char *        sta;
char *                 chan;
int                          ondate;
int                                  offdate;
char *                                        ctype;
double                                               edepth;
double                                                       hang;
double                                                             vang;
char *          descrip;

{
	int i, n;
	Dbvalue value;
	int offdat, chanid;

	static Stbl *sitechan_stbl=NULL;
	static int ok_write;
	static int chanidi;
	struct sitechan_cmp_ *sccmp;
	struct sitechan_cmp_ *sccmp2;
	char *ptr;

	int compare_sitechan();

/*
 *	Create sitechan sorted table
 */
	if (sitechan_stbl == NULL) {
		int n;

		sitechan_stbl = newstbl (compare_sitechan);
		if (sitechan_stbl == NULL) {
			fprintf (stderr, "merge_sitechan: Error creating sitechan sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "sitechan", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], chani[32], ctypei[8], descripi[64];
			double edepthi, hangi, vangi;
			int ondatei, offdatei;

			dbgetv (db, 0,	"sta", stai,
					"chan", chani,
					"ondate", &ondatei,
					"offdate", &offdatei,
					"ctype", ctypei,
					"edepth", &edepthi,
					"hang", &hangi,
					"vang", &vangi,
					"descrip", descripi,
					"chanid", &chanidi,
					0);
			if (!merge_sitechan (db, stai, chani, ondatei, offdatei, ctypei, edepthi, hangi, vangi,
			                descripi )) {
				fprintf (stderr, "merge_sitechan: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing sitechan table.
 */
        db = dblookup (db, 0, "sitechan", 0, 0);
	offdat = offdate;
	if (offdat < 0) offdat = 9999999;
	sccmp = create_sccmp (sta, chan, ondate, offdat, hang, vang, edepth, db, -1, -1);
	if (sccmp == NULL) {
		fprintf (stderr, "merge_sitechan: Malloc error.\n");
		return (0);
	}
/*
 *	Add sitechan compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = addstbl (sitechan_stbl, sccmp);
	if (ptr == NULL) {
		fprintf (stderr, "merge_sitechan: Error return from addstbl.\n");
		return (0);
	}
	sccmp2 = (struct sitechan_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (sccmp != sccmp2) {
		if (sccmp->ondate >= sccmp2->ondate && sccmp->offdate <= sccmp2->offdate) {
			if (!match(sccmp->hang,sccmp2->hang,1)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ondate-offdate match with mismatch in hang.\n");
				free (sccmp);
				return (0);
			}
			if (!match(sccmp->vang,sccmp2->vang,1)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ondate-offdate match with mismatch in vang.\n");
				free (sccmp);
				return (0);
			}
			if (!match(sccmp->edepth,sccmp2->edepth,4)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ondate-offdate match with mismatch in edepth.\n");
				free (sccmp);
				return (0);
			}
			free (sccmp);
			return (sccmp2->chanid);
		}
		if (sccmp->ondate >= sccmp2->ondate && sccmp->ondate < sccmp2->offdate) {
			fprintf (stderr, "merge_sitechan: overlap in site times.\n");
			free (sccmp);
			return (0);
		}
		if (sccmp->offdate > sccmp2->ondate && sccmp->offdate <= sccmp2->offdate) {
			fprintf (stderr, "merge_sitechan: overlap in site times.\n");
			free (sccmp);
			return (0);
		}
		if (sccmp->ondate < sccmp2->ondate && sccmp->offdate > sccmp2->offdate) {
			fprintf (stderr, "merge_sitechan: overlap in site times.\n");
			free (sccmp);
			return (0);
		}
		if (sccmp->ondate == sccmp2->offdate) {
			if (match(sccmp->hang,sccmp2->hang,1) && match(sccmp->vang,sccmp2->vang,1) 
							&& match(sccmp->edepth,sccmp2->edepth,4)) {
				sccmp2->offdate = offdat;
				sccmp2->db.record = sccmp2->ituple;
				dbputv (sccmp2->db, 0,
					"offdate", offdate,
					0);
				free (sccmp);
				return (sccmp2->chanid);
			}
		}
		if (sccmp->offdate == sccmp2->ondate) {
			if (match(sccmp->hang,sccmp2->hang,1) && match(sccmp->vang,sccmp2->vang,1) 
							&& match(sccmp->edepth,sccmp2->edepth,4)) {
				sccmp2->ondate = ondate;
				sccmp2->db.record = sccmp2->ituple;
				dbputv (sccmp2->db, 0,
					"ondate", ondate,
					0);
				free (sccmp);
				return (sccmp2->chanid);
			}
		}
	}
/*
 *	No match in sorted table.
 *	Add new sitechan tuple.
 */

	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		sccmp->ituple = value.i-1;
		sccmp->chanid = chanidi;
		return (chanidi);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	chanid = dbnextid(db, "chanid");
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"ondate", ondate,
		"chanid", chanid,
		"offdate", offdate,
		"ctype", ctype,
		"edepth", edepth,
		"hang", hang,
		"vang", vang,
		"descrip", descrip,
		0);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	sccmp->ituple = value.i-1;
	sccmp->chanid = chanid;
	return (chanid);
}

int
merge_sensor (db, sta, chan, time, endtime, inid, chanid, jdate,
		calratio, calper, tshift, instant )

Dbptr     db;
char *        sta;
char *               chan;
double                     time;
double                          endtime;
int                                      inid;
int                                            chanid;
int                                                    jdate;
double          calratio;
double                    calper;
double                             tshift;
char *                                   instant;

{
	int i, n;
	Dbvalue value;

	static Stbl *sensor_stbl=NULL;
	static int ok_write;
	struct sensor_cmp_ *sncmp;
	struct sensor_cmp_ *sncmp2;
	char *ptr;

	int compare_sensor();

/*
 *	Create sensor sorted table
 */
	if (sensor_stbl == NULL) {
		int n;

		sensor_stbl = newstbl (compare_sensor);
		if (sensor_stbl == NULL) {
			fprintf (stderr, "merge_sensor: Error creating sensor sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "sensor", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], chani[32], instanti[8];
			double timei, endtimei, calratioi, calperi, tshifti;
			int inidi, chanidi, jdatei;

			dbgetv (db, 0,	"sta", stai,
					"chan", chani,
					"time", &timei,
					"endtime", &endtimei,
					"inid", &inidi,
					"chanid", &chanidi,
					"jdate", &jdatei,
					"calratio", &calratioi,
					"calper", &calperi,
					"tshift", &tshifti,
					"instant", instanti,
					0);
			if (!merge_sensor (db, stai, chani, timei, endtimei, inidi, chanidi, jdatei,
			                calratioi, calperi, tshifti, instanti )) {
				fprintf (stderr, "merge_sensor: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing sensor table.
 */
        db = dblookup (db, 0, "sensor", 0, 0);
	sncmp = create_sncmp (sta, chan, time, endtime, inid, chanid, calratio, calper, tshift, db, -1);
	if (sncmp == NULL) {
		fprintf (stderr, "merge_sensor: Malloc error.\n");
		return (0);
	}
/*
 *	Add sensor compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = addstbl (sensor_stbl, sncmp);
	if (ptr == NULL) {
		fprintf (stderr, "merge_sensor: Error return from addstbl.\n");
		return (0);
	}
	sncmp2 = (struct sensor_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (sncmp != sncmp2) {
		if (sncmp->time >= sncmp2->time && sncmp->endtime <= sncmp2->endtime) {
			if (sncmp->inid != sncmp2->inid) {
				fprintf (stderr, 
		"merge_sensor: sta-ondate-offdate match with mismatch in inid.\n");
				free (sncmp);
				return (0);
			}
			if (sncmp->chanid != sncmp2->chanid) {
				fprintf (stderr, 
		"merge_sensor: sta-ondate-offdate match with mismatch in chanid.\n");
				free (sncmp);
				return (0);
			}
			if (!match(sncmp->calratio,sncmp2->calratio,6)) {
				fprintf (stderr, 
		"merge_sensor: sta-ondate-offdate match with mismatch in calratio.\n");
				free (sncmp);
				return (0);
			}
			if (!match(sncmp->tshift,sncmp2->tshift,2)) {
				fprintf (stderr, 
		"merge_sensor: sta-ondate-offdate match with mismatch in tshift.\n");
				free (sncmp);
				return (0);
			}
			if (!match(sncmp->calper,sncmp2->calper,6)) {
				fprintf (stderr, 
		"merge_sensor: sta-ondate-offdate match with mismatch in calper.\n");
				free (sncmp);
				return (0);
			}
			free (sncmp);
			return (1);
		}
		if (sncmp->time >= sncmp2->time && sncmp->time < sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: overlap in sensor times.\n");
			free (sncmp);
			return (0);
		}
		if (sncmp->endtime > sncmp2->time && sncmp->endtime <= sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: overlap in sensor times.\n");
			free (sncmp);
			return (0);
		}
		if (sncmp->time < sncmp2->time && sncmp->endtime > sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: overlap in sensor times.\n");
			free (sncmp);
			return (0);
		}
		if (sncmp->time == sncmp2->endtime) {
			if (match(sncmp->calratio,sncmp2->calratio,6) && match(sncmp->calper,sncmp2->calper,6) 
					&& match(sncmp->tshift,sncmp2->tshift,2) && sncmp->inid == sncmp2->inid
					&& sncmp->chanid == sncmp2->chanid) {
				sncmp2->endtime = endtime;
				sncmp2->db.record = sncmp2->ituple;
				dbputv (sncmp2->db, 0,
					"endtime", endtime,
					0);
				free (sncmp);
				return (1);
			}
		}
		if (sncmp->endtime == sncmp2->time) {
			if (match(sncmp->calratio,sncmp2->calratio,6) && match(sncmp->calper,sncmp2->calper,6) 
					&& match(sncmp->tshift,sncmp2->tshift,2) && sncmp->inid == sncmp2->inid
					&& sncmp->chanid == sncmp2->chanid) {
				sncmp2->time = time;
				sncmp2->db.record = sncmp2->ituple;
				dbputv (sncmp2->db, 0,
					"time", time,
					"jdate", jdate,
					0);
				free (sncmp);
				return (1);
			}
		}
	}

/*
 *	No match in sorted table.
 *	Add new sensor tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		sncmp->ituple = value.i-1;
		return (1);
 	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"time", time,
		"endtime", endtime,
		"inid", inid,
		"chanid", chanid,
		"jdate", jdate,
		"calratio", calratio,
		"calper", calper,
		"tshift", tshift,
		"instant", instant,
		0);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	sncmp->ituple = value.i-1;
	return (1);
}

int
merge_instrument (db, insname, instype, band, digital, samprate, ncalib, ncalper,
		dir, dfile, rsptype )

Dbptr           db;
char *              insname;
char *                       instype;
char *                                band;
char *                                      digital;
double                                               samprate;
double                                                         ncalib;
double                                                                 ncalper;
char *          dir;
char *               dfile;
char *                      rsptype;

{
	Dbvalue value;
	int inid;

	static Stbl *instrument_stbl=NULL;
	static int ok_write;
	static int inidi;
	struct instrument_cmp_ *icmp;
	struct instrument_cmp_ *icmp2;
	char *ptr;

	int compare_instrument();

/*
 *	Create instrument sorted table
 */
	if (instrument_stbl == NULL) {
		int n;

		instrument_stbl = newstbl (compare_instrument);
		if (instrument_stbl == NULL) {
			fprintf (stderr, "merge_instrument: Error creating instrument sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "instrument", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char insnamei[64], instypei[32], bandi[8], digitali[8];
			char diri[256], dfilei[256], rsptypei[16];
			double sampratei, ncalibi, ncalperi;

			dbgetv (db, 0,	"insname", insnamei,
					"instype", instypei,
					"band", bandi,
					"digital", digitali,
					"samprate", &sampratei,
					"ncalib", &ncalibi,
					"ncalper", &ncalperi,
					"dir", diri,
					"dfile", dfilei,
					"rsptype", rsptypei,
					"inid", &inidi,
					0);
			if (!merge_instrument (db, insnamei, instypei, bandi, digitali, sampratei, ncalibi, ncalperi,
			                diri, dfilei, rsptypei )) {
				fprintf (stderr, "merge_instrument: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing instrument table.
 */
        db = dblookup (db, 0, "instrument", 0, 0);
	icmp = create_icmp (dir, dfile, rsptype, samprate, ncalib, ncalper, db, -1, -1);
	if (icmp == NULL) {
		fprintf (stderr, "merge_instrument: Malloc error.\n");
		return (0);
	}
/*
 *	Add instrument compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = addstbl (instrument_stbl, icmp);
	if (ptr == NULL) {
		fprintf (stderr, "merge_instrument: Error return from addstbl.\n");
		return (0);
	}
	icmp2 = (struct instrument_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (icmp != icmp2) {
		free (icmp);
		return (icmp2->inid);
	}
/*
 *	No match in sorted table.
 *	Add new instrument tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		icmp->ituple = value.i-1;
		icmp->inid = inidi;
		return (icmp->inid);
 	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	inid = dbnextid(db, "inid");
	dbputv (db, 0,
		"inid", inid,
		"insname", insname,
		"instype", instype,
		"band", band,
		"digital", digital,
		"samprate", samprate,
		"ncalib", ncalib,
		"ncalper", ncalper,
		"dir", dir,
		"dfile", dfile,
		"rsptype", rsptype,
		0);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	icmp->ituple = value.i-1;
	icmp->inid = inid;
	return (icmp->inid);
}

/* $Id$ */
