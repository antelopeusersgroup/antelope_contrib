#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "db.h"
#include "stock.h"
#include "coords.h"
#include "arrays.h"
#include "orb.h"
#include "location.h"
#include "elog.h"

#define DEFAULT_PFFILE "orbgenloc"
#define NULL_ORB -1  /* used to signal a record not be sent to orb */

static void usage (char *Program_Name)
{
	die(0,"Usage:  %s orbserver [-pf pffile]\n", 
		Program_Name);
}
/* Temporary, special function to replace read_arrival for
pf records produced by orbtrigger.  This is mostly a format
difference.

*/
Tbl *read_orbtrigger_arrivals(Pf *pf, Arr *phases, Arr *stations)
{
	Tbl *tin, *tout;
	Arrival *a;
	char *row;
	int i;

	char phase_name[20],sta[12];

	tin = pfget_tbl(pf,"arrivals");

	if(tin == NULL) return(newtbl(0));

	tout = newtbl(0);	

	/* Now we just read each tbl entry string returned by 
	pfget_tbl, convert the string values to entries in the
	Arrival structure, and load the pointers into the tbl
	we will ultimately return */
	for(i=0;i<maxtbl(tin);++i)
	{
		row = gettbl(tin,i);
		a = (Arrival *)malloc(sizeof(Arrival));
		if(a == NULL) die(1,"read_orbtrigger_arrivals:  cannot alloc memory for Arrival structure\n");
		sscanf(row,"%*s %s %*s %s %lf",sta,phase_name,&(a->time));
		a->arid = -1;
		a->sta = (Station *) getarr(stations,sta);
		if(a->sta == NULL)
		{
			complain(1,"Warning (read_orbtrigger_arrivals):  Can't find coordinates for station %s\n%s phase arrival for this station skipped\n",
				sta, phase_name);
			free(a);
			continue;
		}
		a->phase = (Phase_handle *) getarr(phases,phase_name);
		if(a->phase == NULL)
		{
			complain(1,"Warning (read_orbtrigger_arrivals):  No phase handle for phase name %s\nArrival for station %s skipped\n",
				phase_name,sta);
			free(a);
			continue;
		}
		/* Always use the detault deltat here */
		a->deltat = (double)a->phase->deltat0;
		pushtbl(tout,a);
	}
	return(tout);
}
/* This function reads all parameters special to this program.  
It provides a simple way to keep all this together. */

typedef struct RTlocate_Options
{
	char *working_directory;
	char *work_db;
	char *logdir;
	int save_arrival,save_event;  /* booleans that are ! of actual parameters*/
	
} RTlocate_Options;
RTlocate_Options parse_rt_options (Pf *pf)
{
	RTlocate_Options o;
	o.working_directory = pfget_string(pf,"RT_working_directory");
	if(o.working_directory == NULL) o.working_directory = strdup(".");
	o.work_db=pfget_string(pf,"RT_working_db");
	if(o.work_db  == NULL) o.work_db = strdup("orbgenloc");
	o.logdir=pfget_string(pf,"RT_logfile_directory");
	if(o.logdir == NULL) o.logdir = strdup(".");
	o.save_arrival = !(pfget_boolean(pf,"do_not_save_arrival"));
	o.save_event = !(pfget_boolean(pf,"do_not_save_event"));
	return(o);
}
typedef struct RTids
{
	int orid;
	int evid;
	int arid;
} RTids;
/* For this version we handle database ids through this routine.  Database ids can be
be reset by passing a message through the parameter file to reset the counter for that
id to a specific value.  Otherwise this program simply increments these counters based
on it's working db tables.  

Here we handle orid and evid.  There are two parameters:  (1) a boolean used to 
enable a hard reset, and (2) a parameter with the id name to set the value.  Nothing
is changed if the boolean is false.  When it is true, the value is reset and the 
boolean is reset to false.  
*/
RTids set_ids(RTids ids, Pf *pf)
{
	if(pfget_boolean(pf,"reset_orid"))
	{
		ids.orid = pfget_int(pf,"orid");
		pfput_boolean(pf,"reset_orid",0);
	}
	if(pfget_boolean(pf,"reset_evid"))
	{
		ids.evid = pfget_int(pf,"evid");
		pfput_boolean(pf,"reset_evid",0);
	}
	if(pfget_boolean(pf,"reset_arid"))
	{
		ids.evid = pfget_int(pf,"arid");
		pfput_boolean(pf,"reset_arid",0);
	}
	return(ids);
}
	

/* This little function returns the latest time in the arrival table
in db.  It returns 0 if the table is empty, which is useable directly 
later to always start at the beginning of the ring buffer.  The only 
arg is the db pointer for the db examined */
void get_startup_values(Dbptr db, double *t0, int *orid)
{
	Tbl *sortkeys;
	int nrows;

	db = dblookup (db, 0, "origin", 0,0);
	dbquery(db,dbRECORD_COUNT, &nrows);
	if(nrows <= 0)
	{
		complain(0,"Empty origin table:  starting processing at beginning of ring buffer\n");
		*t0 = 0.0;
		*orid = 0;
	}
	else
	{
		sortkeys = newtbl(0);
		pushtbl (sortkeys,"time");
		db = dbsort(db,sortkeys,0,0);
		freetbl(sortkeys,free);
		db.record = nrows-1;
		if(dbgetv(db,0,"time",t0,"orid",orid,0) == dbINVALID)
			die(1,"Startup error with dbgetv in reading latest time from working db\n");
		elog_notify(0,
		  "STARTUP:  working db origin table has %d rows\nResuming orbgenloc processing at time %s and orid %d\n",
			nrows, strtime(*t0),*orid);

	}
}
/* This routine applies time t0 defined by function get_startup_values. 
It examines the arrival data in pf and uses a simple velocity time scaling
to decide if this event has already been processed.  That is, since t0 is
an origin time, it examines the arrival times and returns true if any 
arrival in the list is less than t0+delta_max where delta_max is an input
parameter.  delta_max should be defined as the maximum transit time across
the network defined in the input parameter list to this program.  
*/
int event_already_processed(double t0,Pf *pf)
{
	double delta_max,time;
	Tbl *t;
	char *s;
	int i;

	delta_max = pfget_double(pf,"maximum_network_transit_time");
	t0 += delta_max;
	t = pfget_tbl(pf,"arrivals");
	if(t == NULL)
	{
		complain(0,"No arrivals found in pf packet encountered during startup scan\nItem skipped\n");
		return(1);
	}
	for(i=0;i<maxtbl(t);++i)
	{
		s = gettbl(t,i);
/* This is the format of arrival records used by sgnloc 
		sscanf(s,"%*s%*s%lf",&time);
For now we use the format from orbtrigger 
*/
		sscanf(s,"%*s%*s%*s%*s%lf",&time);

		if(time < t0) return(1);
	}
	return(0);

}
RTids save_arrival(RTids id, Dbptr db, Tbl *ta, Tbl *tu, int orb)
{
	Arrival *a;
	Slowness_vector *u;
	int n;
	int recstart, recend;  /* range of record numbers for this
				event */
	int i;
	/* To allow us to insert slowness data records we 
	write the arrival records to the db first, then send them
	to the orb if this is desired.  The problem relates to
	the fact that we have to connect slowness measures with
	a given arid to a particular record.  thus we first
	write the arrival records, insert the slowness 
	data, then write to orb.*/

	db = dblookup(db,0,"arrival",0,0);

	n=maxtbl(ta);
	for(i=0;i<n;i++)
	{
		a=(Arrival*)gettbl(ta,i);
		if(a->arid <= 0) 
		{
			a->arid = dbnextid(db,"arid");
			id.arid = a->arid;
		}
		/* We are going to lose the channel code here.
		avoiding this would cause a cascade of related headaches*/
		recend = dbaddv(db,0,
			"sta",a->sta->name,
			"time",a->time,
			"arid",a->arid,
			"jdate",yearday(a->time),
			"iphase",a->phase->name,0);
		if(recend==dbINVALID)
			complain(0,"dbaddv error writing arrival record for arid %d -- phase %s at time %lf to working db\n",
					a->arid, a->phase->name, a->time);
		else if(i==0)
			recstart = recend;

	}
	/* Now we add the slowness vector data with dbputv matching arid
	fields.  When arids are not defined slowness vectors data will
	not be written into the arrival table and a warning will be issued.*/
	n=maxtbl(tu);
	for(i=0;i<n;++i)
	{
		u = (Slowness_vector *)gettbl(tu,i);
		if(u->arid <= 0)
		{
			complain(0,"Slowness vector measurement from array %s for phase %s has no arid.\nArrival record not saved\n",
				u->array->name, u->phase->name);
		}
		else
		{
			for(db.record=recstart;db.record<recend;++db.record)
			{
				int testarid;
				double azimuth, slow;
				dbgetv(db,0,"arid",testarid,0);
				if(u->arid == testarid)
				{
					slow = sqrt((u->uy)*(u->uy)
						+(u->uy)*(u->uy));
					azimuth = atan2 ( u->uy, u->ux );
					slow = deg2km(slow);
					azimuth=deg(azimuth);
					if(dbputv(db,0,
						"slow",slow,
						"azimuth",azimuth,0)
						== dbINVALID)
					   complain(0,"dbputv error adding slowness vector data to arrival for arid %d\n",
						u->arid);
					break;
				}
			}
			if(db.record >= recend)
				complain(0,"Cannot find matching arrival record for slowness vector with arid %d\nCannot save slowness vector data\n",
					u->arid);
		}
	}
	if(orb != NULL_ORB)
	{
		for(db.record=recstart;db.record<recend;++db.record)
		{
			if(dbget(db,0) == dbINVALID)
			{
				complain(0,"cannot fetch record %d from working arrival table\nrecord not send to orb\n",
					db.record);
			}
			else
			{
				if(db2orbpkt(db,orb)) 
					complain(0,"Error writing arrival record from record %d of working db to orb\n",
						db.record);
			}
		}
	}
	return(id);
}

RTids save_origin(RTids id, Dbptr db,int depth_fixed,Hypocenter h,int orb)
{
	int ndef;
	char dtype[2];
        char algorithm[16]="genloc-nlls";
	int origin_record;
	int nextorid,nextevid;
	int grn, srn;

	db = dblookup(db,0,"origin",0,0);
        if(depth_fixed)
        {
                ndef = h.degrees_of_freedom + 3;
                strcpy(dtype,"r");
        }
        else
        {
                ndef = h.degrees_of_freedom + 4;
                strcpy(dtype,"f");
        }
	nextorid = dbnextid(db,"orid");
	nextevid = dbnextid(db,"evid");
	++(id.orid);
	++(id.evid);
	/* normally the following pair of tests are both false.  They should only be entered
	when ids are reset from outside */
	if(id.orid != nextorid)
		if(id.orid < nextorid)
		{
			complain(0,"Illegal orid reset requested\nCurrent orid = %d;  requested change to %d illegal (must be > current)\n",
				nextorid, id.orid);
			id.orid = nextorid;
		}
		else
		{
			--(id.orid);
			elog_notify(0,"Reset orid to %d",id.orid);
		}
	/* Identical code, could be shortened with a function, but so what */
	if(id.evid != nextevid)
		if(id.evid < nextevid)
		{
			complain(0,"Illegal evid reset requested\nCurrent evid = %d;  requested change to %d illegal (must be > current)",
				nextevid, id.evid);
			id.evid = nextevid;
		}
		else
		{
			--(id.evid);
			elog_notify(0,"Reset evid to %d",id.evid);
		}
	grn = grnumber(h.lat, h.lon) ;
	srn = srnumber ( grn ) ;
	db.record = dbSCRATCH;
	if((dbputv(db,0,
                "lat",h.lat,
                "lon",h.lon,
                "depth",h.z,
                "time",h.time,
                "orid",id.orid,
		"evid",id.evid,
		"grn", grn,
		"srn", srn,
		"ndef",ndef,
		"algorithm",algorithm,
		"auth",algorithm,0))
			 == dbINVALID)
	{
		complain(0,"dbputv error while building origin record for orid %d\nNo results saved\n",
			id.orid);
	}
	else
	{
		if(save_dbrecord(db,orb))
			complain(0,"Errors saving orid %d\n",id.orid);
	}
	return(id);
}
/* Special function used in orbgenloc for repeated task of writing a 
record to working db then sending the same record to the orb.  It
does this in a way that is relatively bombproof.  

Arguments:
	db - input db pointer.  It is ASSUMED that db.record is dbSCRATCH
		and the relevant record has been copied onto the scratch
		record before calling this function.  This record is 
		added to the working db and a complaint is issued if the
		add fails.
	orb - orb descriptor to send the scratch record of db to.  If
		set to NULL_ORB (defined above) the record is not written
		to the orb.

Returns 0 if all worked.  Returns negative of number of failures 
otherwise with messages placed in error log.  
Author:  Gary L. Pavlis
Written:  May 1997
*/
int save_dbrecord(Dbptr db, int orb)
{
	char *table_name;
	int record;
	int ret_code=0;
	record = dbadd(db,0);
	if(record == dbINVALID)
	{
		dbquery(db,dbTABLE_NAME,table_name);
		register_error(0,"Cannot add new record to table %s in working db\n",
			table_name);
		--ret_code;
	}
	if(orb == NULL_ORB) return(ret_code);

	if(db2orbpkt(db,orb)) 
	{
		dbquery(db,dbTABLE_NAME,table_name);
		register_error(0,"Error writing a record for table %s to orb\nWorking db record number = %d\n",
			table_name,record);
		--ret_code;
	}
	return(ret_code);
}
	

	
/* This functions adds one new record to working db event table.  
The only fields really set here are the id fields and auth.  

Author:  Gary L. Pavlis
Written:  May 1997
*/

void save_event(RTids id, Dbptr db, int orb)
{
	
	/*altered in output by this program */
	int prefor;
	char *auth="orbgenloc";
	double lddate;

	db=dblookup(db,0,"event",0,0);
	db.record = dbSCRATCH;

	if(dbputv(db,"event",
                "evid",id.evid,
		"prefor",id.orid,
                "auth",auth,
			0) == dbINVALID)
	{
		complain(0,"save_event: dbputv error writing event record for orid %d\nNo event record saved\n",
				id.orid);
	}
	else
	{
		if(save_dbrecord(db,orb))
			complain(0,"Error saving event record for orid %d\n",
				id.orid);
	}
}
/*This function adds a single row to the origerr table for this event.

arguements:
	orid - orid assign to this solution
	h - Hypocenter object for this solution
	dbo - output db

This version doesn't set much of this large table.  Eventually, it 
needs to include all the error terms, but that function is not 
written yet. 

Author:  Gary L. Pavlis
Written:  February 1997
*/
void save_origerr(int orid, Hypocenter h, Dbptr db, int orb)
{
	double sdobs; 

	db = dblookup(db,0,"origerr",0,0);
	db.record = dbSCRATCH;

	/* Bad news here is that we can't save the more useful 
	statistics that this program calculates.  css3.0 only allows
	sdobs = sswr/ndgf */

	sdobs = h.rms_raw;
	if(dbputv(db,0,
		"orid",orid,
		"sdobs",sdobs,
			0) == dbINVALID)
	{
		complain(0,"save_origerr: dbaddv error writing origerr record for orid %d\norigerr record not saved anywhere\n",
				orid);
	}
	else
	{
		if(save_dbrecord(db,orb))
			complain(0,"Error saving origerr record for orid %d\n",
				orid);
	}

}
/* Do nothing function used to avoid double free in u_tmp arr 
below */
void free_nothing(void *x)
{
}
void save_assoc(Tbl *ta, Tbl *tu, 
	int orid, char *vmodel, Arr *station, Arr *arrays, Hypocenter hypo, 
	Dbptr db, int orb)
{
        int arid;
        char sta[8];
        char phase[10];
        double belief;
        double delta;
        double seaz;
        double esaz;
        double timeres;
        double azres;
        double slores;
        double wgt;
        char timedef[2],slodef[2], azdef[2];
        Arr *u_arr;
	char key_arid[20];
	Tbl *udregs; 
        int i,n;
 
        double r, w, reswt,uxresid, uyresid;
        double stalat, stalon;
        double ux, uy, azimuth;
        double duphi;
	Arrival *a;
	Slowness_vector *u;

	/* We build an associative array keyed to arid for
	all the slowness vector measurements. 
	Then in the loop below we can efficiently find any
	slowness vectors associated with the same arid as
	an Arrival.  The overhead in this is significant, but
	it makes it completely general and open ended.  */
	n = maxtbl(tu);
	u_arr = newarr(0);
	for(i=0;i<n;i++)
	{
		Slowness_vector *utmp;
		utmp = (Slowness_vector *)gettbl(tu,i);
		sprintf(key_arid,"%d",utmp->arid);
		setarr(u_arr,key_arid,utmp);
	}
	db = dblookup(db,0,"assoc",0,0);
	db.record = dbSCRATCH;

	n=maxtbl(ta);
	for(i=0;i<n;i++)
	{
		a=(Arrival*)gettbl(ta,i);
		dist(rad(hypo.lat),rad(hypo.lon),
		  rad(a->sta->lat),rad(a->sta->lon),&delta,&esaz);
		dist(rad(a->sta->lat),rad(a->sta->lon),
		  rad(hypo.lat),rad(hypo.lon),&delta,&seaz);
		sprintf(key_arid,"%d",a->arid);
		u = (Slowness_vector *) getarr(u_arr,key_arid);
		if(u == NULL)
		{
		    if(dbputv(db,0,
			"orid",orid,
			"arid",a->arid,
			"sta",a->sta->name,
			"phase",a->phase->name,
			"delta",deg(delta),
			"seaz",deg(seaz),
			"esaz",deg(esaz),
			"timeres",(double)a->res.raw_residual,
			"timedef","d",
			"vmodel",vmodel,
			"wgt",(double)a->res.residual_weight,
		  	0)<0)
		    {
			  complain(0,
			    "Can't add assoc record for station %s arid = %d orid = %d to working db scratch record\nRecord skipped and not saved anywhere\n",
				a->sta->name,a->arid,orid);
			  continue;
		    }
		}
		else
		{
			slores = deg2km(sqrt(sqr(u->xres.raw_residual) 
				+ sqr(u->yres.raw_residual)));
			azimuth = atan2 ( u->uy, u->ux ) ;
			duphi = (u->ux*cos(azimuth) 
				- u->uy*sin(azimuth)) 
				/ sqrt(sqr(u->ux)+ sqr(u->uy)) ;
			azres = deg(duphi);
			if(dbputv(db,"assoc",
				"orid",orid,
				"arid",a->arid,
				"sta",a->sta->name,
				"phase",a->phase->name,
				"delta",deg(delta),
				"seaz",deg(seaz),
				"esaz",deg(esaz),
				"timeres",(double)a->res.raw_residual,
				"timedef","d",
				"vmodel",vmodel,
				"slores",slores,
				"slodef","d",
				"azres",azres,
				"azdef","d",
				"wgt",(double)a->res.residual_weight,
		  	  0)<0)
			{
			  	complain(0,
				  "Can't add assoc record for station %s arid = %d orid = %d to working db scratch record\nRecord skipped and not saved anywhere\n",
				a->sta->name,a->arid,orid);
				delarr(u_arr,key_arid);
				continue;
			}
			/* We delete this entry from u_arr, then we
			can scan below for the dregs easily */
			delarr(u_arr,key_arid);
		}
		if(save_dbrecord(db,orb))
			complain(0,"Error saving assoc record for arid %d\n",
				a->arid);
	}
	/* Since it is possible that slowness vectors can be measured
	with no arrival time, we need to take care of that possibility.
	We do that by checking for dregs in u_arr not removed with
	delarr calls above */
	udregs = keysarr(u_arr);

	n = maxtbl(udregs);
	for(i=0;i<n;i++)
	{
		char *key;
		key = gettbl(udregs,i);
		u = (Slowness_vector *) getarr(u_arr,key);
                dist(rad(hypo.lat),rad(hypo.lon),
                  rad(u->array->lat),rad(u->array->lon),&delta,&esaz);
                dist(rad(u->array->lat),rad(u->array->lon),
                  rad(hypo.lat),rad(hypo.lon),&delta,&seaz);
                slores = deg2km(sqrt(sqr(u->xres.raw_residual)
                          + sqr(u->yres.raw_residual)));
                azimuth = atan2 ( u->uy, u->ux ) ;
                duphi = (u->ux*cos(azimuth)
                          - u->uy*sin(azimuth))
                          / sqrt(sqr(u->ux)+ sqr(u->uy)) ;
                azres = deg(duphi);
		/* The residual weight extraction from the ux component is 
		not ideal here because it could be wrong.  It is unavoidable
		due to polar-cartesian conversion */
		if(dbputv(db,"assoc",
			"orid",orid,
			"arid",u->arid,
			"sta",u->array->name,
			"phase",u->phase->name,
			"delta",deg(delta),
			"seaz",deg(seaz),
			"esaz",deg(esaz),
			"timedef","n",
			"vmodel",vmodel,
			"slores",slores,
			"slodef","d",
			"azres",azres,
			"azdef","d",
			"wgt",(double)u->xres.residual_weight,
	  	  0)<0)
		{
		  	complain(0,"Can't add assoc record for array slowness vector with %s arid = %d and orid = %d to working db scratch record\nNothing saved\n",
			u->array->name,u->arid,orid);
			continue;
		}
		if(save_dbrecord(db,orb))
			complain(0,"Error saving assoc record for arid %d\n",
				u->arid);		

	}
	/* We must not use regular free here, or later we could try
	to free the same area twice.  That is, u_tmp contains keyed
	version of the pointers stored in tu.  This releases only
	the Arr structures, but leaves the pointers to be freed 
	later */
	freetbl(udregs,free_nothing);
	freearr(u_arr,free_nothing);
}
char *format_hypo(Hypocenter *h)
{
        char *s;
        s = malloc(512);
        if(s == NULL) die(1,"malloc error for hypocenter output tabulation\n");
        sprintf(s,"%g %g %g %g %g %g %g %g %g %g %g %g %g %g %d %d",
                h->lat0,h->lon0,h->z0,h->t0,
                h->lat,h->lon,h->z,h->time,
                h->dx,h->dy,h->dz,
                h->rms_raw, h->rms_weighted, h->interquartile,
                h->number_data,h->degrees_of_freedom);
        return(s);
}
int write_to_logfile(RTlocate_Options rtopts, int orid,
	Pf *pf, Tbl *converge_history, Tbl *reason_converged,Tbl *residual)
{
	char fname[256];
	FILE *fp;
	/* We write the logfile output as a pf object copying
	code from sgnloc */
	Pf *pflog;
	Tbl *t;  /* formatted output tbl of hypo convergence history */
	char *line; 
	int i;
	Hypocenter *hypos;

	/* Here we save the pf state */
	sprintf(fname,"%s/orid%d.pf",rtopts.logdir,orid);
	pfwrite(fname,pf);

	/* Here we encapsulate the convergence history information into
	a second pf object, and write this onto the same pf file */
        t = newtbl(maxtbl(converge_history));
        for(i=0;i<maxtbl(converge_history);++i)
        {
                hypos = (Hypocenter *)gettbl(converge_history,i);
                line = format_hypo(hypos);
                pushtbl(t,line);
        }
        pflog = pfnew(PFFILE);
        pfput_tbl(pflog,"convergence_history",t);
  
        printf("Reasons for convergence:\n");
        for(i=0;i<maxtbl(reason_converged);++i)
                printf("%s\n",gettbl(reason_converged,i));
        pfput_tbl(pflog,"convergence_criteria",reason_converged);
        pfput_tbl(pflog,"residuals",residual);
	/* We use pf2string rather than pfwrite since I'm not 
	sure what pfwrite will do when the file already exists. 
	Here we just append the new pf info to the end of the
	stuff written above. */
	if((fp=fopen(fname,"a")) == NULL)
	{
		complain(1,"Cannot open log file %s\n",fname);
		return(1);
	}
	fseek(fp,0L,SEEK_END);
        line = pf2string(pflog);
	fwrite(line,1,strlen(line),fp);
	fclose(fp);
	free(line);
	pffree(pflog);
	freetbl(t,free);
	return(0);	
}
RTids compute_location(RTlocate_Options rtopts, RTids id, 
	Arr *stations, Arr *arrays, Arr *phases, Pf *pf,
	Dbptr db, int orbout)
{
	Tbl *ta,*tu;  /* Arrival and slowness tables respectively */
	int nrecords;
	Hypocenter h0;
	int ret_code;
	Tbl *converge_history,*reason_converged,*residual;
	Hypocenter *hypo;
	Location_options o;
	RTlocate_Options rt_opts;
	int niterations;
	char *vmodel;
	int i;
	char *s;

	/* it is a little inefficient to reparse all the options every
	time this function is called, but it makes it very flexible.*/
	o = parse_options_pf (pf);
	rt_opts = parse_rt_options(pf);

	/* Now read data from pf object */

/* This is the libgenloc function for this, call orbtrigger version
defined above instead 
	ta = read_arrivals(pf,phases,stations);
*/
	ta = read_orbtrigger_arrivals(pf,phases,stations);
	tu = read_slowness_vectors(pf,phases,arrays);
	vmodel = pfget_string(pf,"velocity_model_name");


	/* Location with Generic Gauss_Newton code */
	h0 = initial_locate(ta, tu, o, pf);
	ret_code = ggnloc(h0,ta,tu,o,
				&converge_history,&reason_converged,&residual);
        if(ret_code < 0)
        {
                complain(0,"ggnloc failed to produce a solution for orid %d\n",id.orid);
        }
        else
	{
		if(ret_code > 0)
			complain(0,"Warning:  %d travel time calculator failures in ggnloc\nSolution ok for orid %d\n",
                                ret_code,id.orid);
		niterations = maxtbl(converge_history);
                hypo = (Hypocenter *)gettbl(converge_history,niterations-1);
		if(rt_opts.save_arrival)
			id = save_arrival(id,db,ta,tu,orbout);
		else
			id = save_arrival(id,db,ta,tu,NULL_ORB);
                id = save_origin(id,db,o.fix[3],*hypo,orbout);
		if(rt_opts.save_event)
                	save_event(id,db,orbout);
		else
			save_event(id,db,NULL_ORB);
                save_origerr(id.orid,*hypo,db,orbout);
		save_assoc(ta, tu, id.orid, vmodel, stations, arrays, 
			*hypo, db,orbout);
		elog_notify(0,"orid %d converged in %d iterations\n",
				id.orid,niterations);
		elog_notify(0,"Reason(s) for convergence:  \n");
		for(i=0;i<maxtbl(reason_converged);++i)
                        elog_notify(0,"%s",gettbl(reason_converged,i));
		elog_notify(0,"\n");
		s=format_hypo(hypo);
		elog_notify(0,"%s\n",s);
		free(s);
		write_to_logfile(rtopts, id.orid,
			pf, converge_history, reason_converged,residual);
	}

	if(maxtbl(converge_history)>0)freetbl(converge_history,free);
	if(maxtbl(reason_converged)>0)freetbl(reason_converged,free);
	if(maxtbl(residual)>0)freetbl(residual,free);
	destroy_data_tables(tu, ta);
	return(id);
}

int main (int argc, char **argv)
{
	char *version="1.0 (May 1997)";
	Dbptr wdb;
	char *orbname;
	char *pffile=NULL;

	Pf *pf;  /* Input pf object handle */
	Arr *arr_sta, *arr_a;
	Arr *arr_phase;
	int i;

	double t0;  /* Start time established from working db or start of orb*/
	double time;  /* working time */
	int last_orid, orid;  /* orid equiv to t0 and time */
	int orbin,orbout;  /* We establish both a read and write connection
				on seperate sockets so we can use orbreap on
				the input */
	/* These are args to orbreap */
	int pktid;
	char srcname[ORBSRCNAME_SIZE];
	double pkttime;
	char *packet=0;
	int nbytes,bufsize=0;  /* packet and bufsize are initialized this way
				for automatic allocs in orbreap--copied from
				orb2db*/
	char orbselect_string[40];  /* holds target selection for orbselect*/
	int orid_used;
	RTids ids;
	RTlocate_Options rt_opts;
	char *target_name;  /*Target name of this process used to 
			select pf packets from orb */

	ids.orid = 0;  ids.evid=0;  ids.arid=0;
	
	elog_init(argc, argv);
	elog_notify(0,"%s version %s\n", argv[0],version);
	if(argc < 2) usage(argv[0]);
	orbname = argv[1];

	for(i=2;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			pffile = argv[i];
		}
		else
		{
/* For this kind of program it seems wise to make it a fatal error to 
have the arguments botched */
			complain(0,"Unrecognized argument %s\n",argv[i]);
			usage(argv[0]);
		}
	}
        /* set default this way*/
        if(pffile == NULL) pffile = strdup(DEFAULT_PFFILE);

	/* parse parameter file and form all the genloc control and
	internal static data structures */
	i = pfread(pffile,&pf);
	if(i != 0) die(1,"Pfread error\n");
	arr_sta = load_station_table(pf);
	arr_a = load_array_table(pf);
 	arr_phase = parse_phase_parameter_file(pf);

	rt_opts = parse_rt_options(pf);
	target_name=pfget_string(pf,"target_name");
	if(target_name == NULL) target_name = strdup("orbgenloc");
	sprintf(orbselect_string,"/pf/%s",target_name);

	if(chdir(rt_opts.working_directory)) 
		die(1,"Cannot chdir to working directory %s\n",
			rt_opts.working_directory);
	if(dbopen(rt_opts.work_db,"r+",&wdb) == dbINVALID)
                die(1,"Unable to open working database %s/%s\n",
			rt_opts.working_directory,rt_opts.work_db);
	get_startup_values(wdb,&t0,&last_orid);


	/* Now we open the orb server connections */
	if( (orbin=orbopen(orbname,"r")) < 0)
		die(0,"Cannot open ring buffer %s for reading\n",orbname);
	if( (orbout=orbopen(orbname,"w")) < 0)
		die(0,"Cannot open ring buffer %s for writing\n",orbname);

	/* This selects only pf packets */
	if(orbselect(orbin,orbselect_string) < 1)
		die(0,"Cannot select any pf records from ring buffer %s\n",
			orbname);
	if(orbseek(orbin,ORBOLDEST)<0)
		die(0,"orbseek on startup to oldest pf record failed\n");
	/* Enter an infinite loop that never exits unless there is an error*/
	do {
		if(orbreap (orbin, &pktid, srcname, &pkttime, &packet,
			&nbytes, &bufsize)) 
		{
			die (1,"orbreap failed on %s\n",orbname);
		}
		orbpkt2pf(packet,nbytes,&pf);
		/* This routine looks at arrival times and decides if this 
		event has been processed before.  Keys off t0 and input 
		control parameter defined based on network size */
		if(event_already_processed(t0,pf)) continue;
		/* This is how we reset database ids if necessary.  These are passed
		through the parameter file.  Note when skipping on restart this is
		bypassed*/
		ids = set_ids(ids,pf);
	
		/* Compute the location, and return the orid actually used
		for comparison with the orid in pf.  This is necessary in
		case orid list from associator gets out of sync with this
		program */
		ids = compute_location(rt_opts, ids, arr_sta,arr_a,arr_phase,
						pf,wdb,orbout);
	}while (!pfget_boolean(pf,"shutdown"));

}
	
