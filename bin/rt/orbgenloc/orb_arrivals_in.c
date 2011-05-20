/* Copyright (c) 1997 Boulder Real Time Technologies, Inc. */
/* All rights reserved */
 
/* This software module is wholly owned by Boulder Real Time 
   Technologies, Inc. Any use of this software module without
   express written permission from Boulder Real Time Technologies,
   Inc. is prohibited. */

/* The original version of this code was written by Danny Harvey.
Modified 1/30/98 by G Pavlis in the following ways:

1.  Name changed from orbin (too general) to orb_arrivals_in
2.  The "Hypocenter" typedef in the original code had to undergo
a name change to avoid collision with existing "Hypocenter" 
type defined in location.h.  Now has name ORB_Hypocenter.
Note this was also moved to a local include file for this program
called orbgenloc.h.  
3.  added the orbhypo_to_genloc which converts ORB_Hypocenter structure
to that used by genloc.  (see below)
4.  Added code to avoid an infinite loop if a matching arrival row did
not follow an assoc row.  
5.  Call me a wimp, but I added comments.

Note this granularity provides a useful modularity to this code anyway
given the fluidity of development of this system.  (this is the third
version of an input model for this program)  i.e. orb_arrivals_in 
is a module that returns a special object (now called the ORB_Hypocenter)
containing all the info needed to build the input structures required
by genloc functions.  The function orbhypo_to_genloc is the translator
that does this.  This is not very efficient, but it does improve modularity.

*/

#include <stdio.h>

#include "stock.h"
#include "arrays.h"
#include "orb.h"
#include "db.h"
#include "xtra.h"
#include "location.h"
#include "orbgenloc.h"

/* Input routine for db records for orbgenloc.  

Arguments:
orb - input orb 
dbtmp - temporary database used by orbpkt2db
hyp - returned data structure (see orbgenloc.h)
last_pktid - if nonzero, call orbseek to start at that location
opt - orbgenloc control structure (see orbgenloc.h)


Returns:

-1 = error, data returned is invalid
0 = aok
1 = inconsistency error.  Data are certainly incomplete and should
be ignored.

IMPORTANT:  this routine mallocs a single block of memory to 
hold the hyp->assocs structure.  This MUST be freed externally.

*/
int
orb_arrivals_in (int orb, Dbptr dbtmp, ORB_Hypocenter *hyp, 
int *last_pktid, RTlocate_Options opt)

{
	int pktid, nbytes, bufsize=0;
	char *packet=NULL;
	char srcname[64];
	double time;
	Dbptr db;
	char *table_name;
	int evid, orid, arid, prefor;
	char auth[64];
	int n;
	int number_skipped;

	if (*last_pktid > -1) orbseek (orb, *last_pktid);

	/* The algorithm here is to the following sequence:
	(1)  hunt for a db packet that is an event record.
	(2)  if the event record is auth=orbassoc, start processing, otherwise
	     ignore it.
	(3)  next hunt for an origin record from orbassoc skipping any
	     other embedded origin records (needed in case other locators 
	     are running)
	(4)  Then grab interleaved assoc->arrival rows.  The algorithm 
	     is smart and hunts for an arrival that matches the arid 
	     of the previous assoc record skipping any other db records
	     that do not match.  

	Important assumptions of this are that only one process is emitting
	assoc records into the orb. In addition, this program will block
	and wait forever if an error causes a missing arrival row to never
	appear that has an arid matching a previous output assoc row.  
	We may need a timeout or skip count parameter to avoid this
	potential pitfall.  
	*/

	/* This loop is part (1) of the algorithm */
	while (1) {
		if(orbreap (orb, &pktid, srcname, &time, 
					&packet, &nbytes, &bufsize)) 
		{
			elog_log(0,"orbreap error at packet id %d\nContinuing\n",pktid);
			continue;
		}
		if (strncmp(srcname, "/db/", 4)) continue;
		db = orbpkt2db (packet, nbytes, dbtmp);
		dbquery (db, dbTABLE_NAME, &table_name);
		if (strcmp(table_name, "event")) continue;
		if (dbgetv (db, 0,
				"auth", hyp->auth,
				"evid", &(hyp->evid),
				"prefor", &prefor,
				0) == dbINVALID) {
			elog_log(0, "orbin: dbgetv() error.\n");
			return (-1);
		}
		if (strcmp(hyp->auth, "orbassoc")) continue;
		*last_pktid = pktid;

		/* This is part (2) */
		while (1) {
			orbreap (orb, &pktid, srcname, &time, 
					&packet, &nbytes, &bufsize);
			if (strncmp(srcname, "/db/", 4)) continue;
			db = orbpkt2db (packet, nbytes, dbtmp);
			dbquery (db, dbTABLE_NAME, &table_name);
			if (strcmp(table_name, "origin")) continue;
			if (dbgetv (db, 0,
					"auth", auth,
					"evid", &evid,
					"orid", &(hyp->orid),
					"nass", &(hyp->nass),
					"ndef", &(hyp->ndef),
					"time", &(hyp->time),
					"lat", &(hyp->lat),
					"lon", &(hyp->lon),
					"depth", &(hyp->depth),
					0) == dbINVALID) {
				elog_log(0, "orbin: dbgetv() error.\n");
				return (-1);
			}
			if (strcmp(auth, "orbassoc")) continue;
			if (evid != hyp->evid) continue;
			if (prefor > 0 && prefor != hyp->orid) continue;
			if (hyp->assocs == NULL || hyp->nass > hyp->assocs_size) {
				if (hyp->assocs) free (hyp->assocs);
				hyp->assocs = (Association *) malloc (hyp->nass*sizeof(Association));
				if (hyp->assocs == NULL) {
					elog_log(1, "orbin: malloc() error.\n");
					return (-1);
				}
				hyp->assocs_size = hyp->nass;
			}

			/* This is parts (3) and (4) for assoc->arrival */
			n = 0;
			while (1) {
				orbreap (orb, &pktid, srcname, &time, 
						&packet, &nbytes, &bufsize);
				if (strncmp(srcname, "/db/", 4)) continue;
				db = orbpkt2db (packet, nbytes, dbtmp);
				dbquery (db, dbTABLE_NAME, &table_name);
				if (strcmp(table_name, "assoc")) continue;
				if (dbgetv (db, 0,
						"orid", &orid,
						"arid", &(hyp->assocs[n].arid),
						"delta", &(hyp->assocs[n].delta),
						"seaz", &(hyp->assocs[n].seaz),
						"esaz", &(hyp->assocs[n].esaz),
						"timeres", &(hyp->assocs[n].timeres),
						"timedef", hyp->assocs[n].timedef,
						0) == dbINVALID) {
					elog_log(0, "orbin: dbgetv() error.\n");
					return (-1);
				}
				if (orid != hyp->orid) continue;
				/* This is the safe code to avoid an infinite loop */
				number_skipped = 0;
				while (number_skipped <= opt.db_record_skip_timeout) {
					orbreap (orb, &pktid, srcname, &time, 
							&packet, &nbytes, &bufsize);
					if (strncmp(srcname, "/db/", 4)) continue;
					db = orbpkt2db (packet, nbytes, dbtmp);
					dbquery (db, dbTABLE_NAME, &table_name);
					if (strcmp(table_name, "arrival")) 
					{
						++number_skipped;
						continue;
					}
					if (dbgetv (db, 0,
							"arid", &arid,
							"time", &(hyp->assocs[n].time),
							"sta", hyp->assocs[n].sta,
							"chan", hyp->assocs[n].chan,
							"iphase", hyp->assocs[n].iphase,
							0) == dbINVALID) {
						elog_log(0, "orbin: dbgetv() error.\n");
						return (-1);
					}
					if (arid == hyp->assocs[n].arid) break;
				}
				if(number_skipped >= opt.db_record_skip_timeout) 
				{
					elog_log(0,"Record skipping limit reached while hunting for arrival row to match assoc row.\nResynching\nOne or more events were probably skipped\n");
					return(1);
				}
				n++; 
				if (n >= hyp->nass) break;
			}
			return (0);
		}
	}
}
/* This is the translation routine similar to dbload_arrival_table and load_arrival_table
in libgenloc.

Arguments:

hyp - input structure
stations - associative array of station structures
arrphase - associate array of phase handles.

Function returns tbl of Arrival objects used by genloc routines. 

Author:  G Pavlis
written:  january 30, 1998
*/
Tbl *orbhypo_to_genloc(ORB_Hypocenter *hyp, Arr *arrphase, Arr *stations)
{
	Arrival *a;
	Tbl *t;

	int i;

	t = newtbl(0);
	for(i=0;i<hyp->nass;++i)
	{
		a = (Arrival *) malloc(sizeof(Arrival));
		if(a == NULL)
		   elog_die(1,"orbhypo_to_genloc cannot malloc Arrival structure\n");
		a->sta = (Station *) getarr(stations,hyp->assocs[i].sta);
		if(a->sta == NULL)
		{
			elog_complain(1,"Cannot find coordinates for station %s\n%s phase arrival for this station skipped\n",
				hyp->assocs[i].sta, hyp->assocs[i].iphase);
			free(a);
			continue;
		}				
		a->arid = hyp->assocs[i].arid;
		a->time = hyp->assocs[i].time;
		a->phase = (Phase_handle *) getarr(arrphase,
					hyp->assocs[i].iphase);
		if(a->phase == NULL)
		{
		    if ( strcmp(hyp->assocs[i].iphase, "D") != 0 ) { 
			elog_complain(1,"Don't know how to handle phase '%s'"
				" -- Arrival at %s at time %lf skipped\n",
				hyp->assocs[i].iphase,hyp->assocs[i].sta,
				hyp->assocs[i].time);
		    } 
		    free(a);
		    continue;
		}
		/* the current real-time system has no uncertainty 
		estimate on the picks so we always use the default */
		a->deltat = (double)a->phase->deltat0;
                pushtbl(t,a);
	}
	return(t);
}


