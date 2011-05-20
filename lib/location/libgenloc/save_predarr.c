#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "location.h" 
#define KMPERDEG 111.320
/* This set of functions are used to save data in a new table defined
in the schema definition presently called "takeoff".  For locations this
only relevant table is one called "predarr" for predicted arrivals.  

Written:  December 28, 1998
Author:  G Pavlis
*/

/* This first little function returns 0 if the table defined by
the string t is a valid table name for the database pointed
to by the database pointer db.  Otherwise it returns a 1.
This allows a general way to safely check for the validity of 
extension tables to a common schema like css3.0.  General usage would be:

if(dbtable_invalid(db,t)) elog_die(0,"blah blah");

Generally this routine should be called once for a given table, but
it can be called repeatedly at the cost of efficiency.  The later
is specially bad if the table involved is hit often.  
*/
int dbtable_invalid(Dbptr db,char *t)
{
	Dbptr dbcheck;

	dbcheck = dblookup(db,0,t,0,0);
	if(dbcheck.table == dbINVALID)
		return(-1);
	else
		return(0);
}
/* returns true if the table t is empty */
int dbtable_empty(Dbptr db,char *t)
{
	long nrows;
	db=dblookup(db,0,t,0,0);
	dbquery(db,dbRECORD_COUNT,&nrows);
	if(nrows>0) 
		return(0);
	else
		return(1);
}

/* This collection of small functions compute the parameters
stored in predarr from quantities computed by the travel time
calculators.  This involves a mix of base quantities (like
the slowness vector ) and items computed from the partial derivatives.
*/

/* The "dip" angle here is the takeoff angle of the ray from 
the source measured positive downward.  The returned 
value is in degrees.   It is cleanly computed from the time
partial derivatives as these are directly related to this angle.*/
double compute_dip(Travel_Time_Function_Output t)
{
	double uhorizontal;
	double dip;

	uhorizontal = hypot(t.dtdx,t.dtdy);
	dip = deg(atan2(t.dtdz,uhorizontal));
	return(dip);
}

/* The esaz is computed from the dtdx and dtdy values here and 
returned as an azimuth in degrees */
double compute_esaz(Travel_Time_Function_Output t)
{
	double esaz;
	esaz = atan2(-(t.dtdx),-(t.dtdy));
	esaz = deg(esaz);
	if(esaz < 0.0) esaz += 360.0;
	return(esaz);
}

/* seaz is trivial to computer from the slowness vector.  Note, however,
that seaz is a backazimuth, so we need to be cautious of sign 
conventions.  The computed slowness vector points in the direction
of propagation, not the backazimuth so we have to flip the sign.*/
double compute_seaz(Slowness_Function_Output slow)
{
	double seaz;
	seaz = atan2( -(slow.ux), -(slow.uy));
	seaz = deg(seaz);
	if(seaz < 0.0 ) seaz += 360.0;
	return(seaz);
}

/* ema is much harder to compute because of the decision to allow
variable surface velocities at each station.  The analytic result
is simple:  p = sin(theta)/v -- Snell's law with p = magnitude of
the computed slowness vector.  The complexity is in the db lookup
scheme and making the routine bombproof.  The lookup uses dbmatches.
When a lookup fails we revert to a default making a guess for P 
or S based on the phase name.  We do this fairly intelligently 
by searching backward from the end of the phase name for the first
occurence of P or S.  This should work correctly for any phase
I can think of except Lg.  I handle it specially for that reason.

This routine assumes that we have already verified that db has
the stavel table defined.  If it is empty all the matches fail, but
it should still function.  Otherwise bad things will happen. 

Normal return is a nonegative angle of the computed emergence angle
in degrees.  A negative angle should be trapped as an error as the
result is meaningless.
*/
#define DEFAULT_VP  5.0
#define DEFAULT_VS  3.5

double compute_ema (Slowness_vector *u, Slowness_Function_Output *ucalc, 
		Dbptr db, char *vmodel)
{
	Dbptr dbm, dbsv;
	static Tbl *matches;
	static Tbl *kmatch;   /* Used to hold keys for dbmatches.  */
	char *key;
	double velocity;
	static Hook *hook=0;
	int nmatches;
	char cphase;  /* store last char of phase name here */
	int name_len;
	int i; 
	double utotal, ema; 
	
	dbsv = dblookup(db,0,"stavel",0,0);
	dbm = dblookup(db,0,"stavel",0,"dbSCRATCH");
	dbputv(dbm,0,"sta",u->array->name,"phase",u->phase->name,"vmodel",vmodel,NULL );
	/* It seems to be necessary to explicitly define the match keys.  Using natural
	keys defined with null patterns for dbmatches did not work correctly. */
	kmatch = newtbl(0);
	key = strdup("sta");
	pushtbl(kmatch,key);
	key = strdup("phase");
	pushtbl(kmatch,key);
	key = strdup("vmodel");
	pushtbl(kmatch,key);

	dbmatches(dbm,dbsv,&kmatch,&kmatch,&hook,&matches);
	freetbl(kmatch,free);
	nmatches = maxtbl(matches);
	if(nmatches >= 1)
	{
		if(nmatches > 1) elog_log(0,"warning(compute_ema):  multiple records found in stavel table with same primary key\n");  
		dbsv.record = (long) gettbl(matches,0);
		dbgetv(dbsv,0,"velocity",&velocity,NULL );
	}
	else
	{
		/* No matches are found when we end up here.  Revert to default and 
		decide to us P or S velocity by hunting backward from the string
		looking for the first occurence of P or S*/
		if(!strcmp(u->phase->name,"Lg"))
			velocity = DEFAULT_VS;
		else
		{
			name_len = strlen(u->phase->name);
			velocity = DEFAULT_VS;  /* This is the default default */
			for(i=name_len-1;i>=0;i--)
			{
				cphase = u->phase->name[i];
				if(cphase == 'S'){
					velocity = DEFAULT_VS;
					break;
				}
				else if(cphase == 'P'){
					velocity = DEFAULT_VP;
					break;
				}
			}
			elog_log(0,"compute_ema:  no matching entry found in stavel table for station/phase/vmodel = %s/%s/%s\n"
			"Reverting to default surface velocity of %f\n",
				u->array->name,u->phase->name,vmodel,velocity);
		}
		freetbl(matches,0);
	}
	/* now the actual computation is pretty trivial */
	utotal = hypot(ucalc->ux,ucalc->uy);
	ema = asin(utotal*velocity);
	return(deg(ema));
}

/* This function computes the attributes of the predarr table.  
This is seriously complicated by the fact that the timedef and 
slowdef attributes in the css3.0 schema allow independently turning
off time and slowness residuals.  The way we solve this here is to
first save results for all the entries in the Arrival tbl keeping
a temporary list of all the arid values found in the Arrival tbl.  
We then use a stupid linear search algorithm against each arid found
in the Slowness_vector table and only compute a new predarr row if
the arid found for that Slowness vector did not match any found in arid.
The linear search is acceptable here under an assumption that arrival
data will outnumber slowness vector measurements in most locations. 

This algorithm looks for the stavel table that is also defined in the
takeoff schema extensions.  This table defines surface velocities to 
use in computing emergence angles (ema).  If an entry is not found
for a given phase in stavel, the program tries to decide if this is a
P or S phase (by looking at the last character in the phase name) and 
using a default (hardwired below).  This will generate a complain error
because the values in predarr are not necessarily consistent with vmodel
in this situation.

arguments:
	db - database to save results to
	atbl - Tbl of Arrival structures used in genloc
	utbl - Tbl of Slowness_vector structures comparable to a
	h - hypocenter estimate to be used to compute predarr rows
	orid - orid database id assigned to this solution.
	vmodel - name of velocity model being used (key of stavel)

Returns 0 for success, anything else indicates and error.  A positive
return indicates the number of warning messages issued.  A negative
return indicates nothing was written because the predarr table could
not be written due to either:  (1) undefined in the schema or (2) 
table is not writeable. 
Author:  G Pavlis
*/
int save_predarr( Dbptr db,  Tbl *atbl, Tbl *utbl, 
	Hypocenter h, int orid, char *vmodel)
{
	int natimes;
	int nslow;
	long *allarids;
	int errors=0;

	Arrival *atimes;
	Arrival a;
	Station station;
	Slowness_vector *slow;
	Slowness_vector u;
	Seismic_Array sta_array;
	
	Travel_Time_Function_Output tto;
	Slowness_Function_Output u_calc;
	/* These are the fields computed here and saved in predarr */
	double time, utotal, seaz, esaz, ema, dip;

	int stavel_ok;  /* set to 1 if stavel is found valid.  Function
		returns immediately if the predarr table is not defined */

	double ema_null=-1.0;
	int i,k;

	if(dbtable_invalid(db,"predarr"))
	{
		elog_log(0,"predarr table not defined in schema for this database\n");
		return(-1);
	}
	if(dbtable_invalid(db,"stavel"))
	{
		elog_log(0,"stavel table is not defined.  Using defaults for all stations and phases\n");
		stavel_ok = 0;
	}
	else if(dbtable_empty(db,"stavel"))
	{
		stavel_ok=0;  /* do this silently */
	}
	else 
	{
		stavel_ok = 1;
		/* This is the correct way to find the null value for a field*/
		db = dblookup(db,0,"predarr",0,0);
		db.record = dbNULL;
		dbgetv(db,0,"ema",&ema_null,NULL );
	}

	natimes = maxtbl(atbl);
	nslow = maxtbl(utbl);
	if(natimes > 0)
	{
		allarids = (long *)calloc(natimes,sizeof(long));
		if(allarids == NULL) elog_die(0,"save_predarr cannot alloc %d long integers\n",
					natimes);
	}
	/* These quantities need to be explicitly initialized at the top
	of this loop.  They stay fixed for the whole loop.  Only the
	sta_array set really matters. */
	u.ux = 0.0;
	u.uy = 0.0;
	u.array = &sta_array;
	db = dblookup(db,0,"predarr",0,0);

	for(i=0;i<natimes;++i)
	{
		atimes = (Arrival *) gettbl(atbl,i);
		/* discard - phases as predarr makes no sense for them */
		if(strstr(atimes->phase->name,"-")!=NULL) continue;
		allarids[i] = atimes->arid;
		tto = calculate_travel_time(*atimes,h,ALL);
		if(tto.time == TIME_INVALID)
		{
			elog_log(0,"save_predarr failed to compute predicted arrival times for station %s and phase %s\n", 
				atimes->sta->name, atimes->phase->name);
			++errors;
			continue;
		}
		time = h.time + tto.time;
		dip = compute_dip(tto);
		esaz = compute_esaz(tto);

		/* This loads a fake slowness vector for each station.
		Note this is different from the location code and could
		fail in some situations when the location computation 
		did not.   In those situations, slow, ema, and esaz
		cannot be computed.  */
		u.arid = atimes->arid;
		strcpy(u.array->name,atimes->sta->name);
		u.array->lat = atimes->sta->lat;
		u.array->lon = atimes->sta->lon;
		u.array->elev = atimes->sta->elev;
		u.phase = atimes->phase;
		
		u_calc = calculate_slowness_vector(u,h,RESIDUALS_ONLY);
		if(u_calc.ux == SLOWNESS_INVALID)
		{
			elog_log(0,"predarr failed to compute slowness vector for station %s and phase %s\n",
				atimes->sta->name, atimes->phase->name);
			++errors;
			if(dbaddv(db,0,
				"arid",atimes->arid,
				"orid",orid,
				"time",time,
				"esaz",esaz,
				"dip",dip,
				NULL ) == dbINVALID)
			{
				elog_log(0,"dbaddv on predarr table for station %s and phase %s\n",
					atimes->sta->name, atimes->phase->name);
				++errors;
			}
				
		}
		else
		{
			utotal = hypot(u_calc.ux,u_calc.uy);
			/* css3.0 stores slowness is s/deg so we have
			to convert */
			utotal *= KMPERDEG;
			seaz = compute_seaz(u_calc);
			if(stavel_ok) ema = compute_ema(&u,&u_calc,db,vmodel);
			if(ema < 0.0) ema = ema_null;
			if(dbaddv(db,0,
				"arid",atimes->arid,
				"orid",orid,
				"time",time,
				"slow",utotal,
				"seaz",seaz,
				"ema", ema,
				"esaz",esaz,
				"dip",dip,
				NULL ) == dbINVALID)
			{
				elog_log(0,"dbaddv on predarr table for station %s and phase %s\n",
					atimes->sta->name, atimes->phase->name);
				++errors;
			}
		}

	}
	/* Nww we have to hunt for slowness measurements that have not
	travel time computed above.  Here we used a dumb linear search
	against arid for reasons give above.  The rest of the algorithm
	closely parallels the above loop, except the role of arrival 
	and slowness are reversed.  */
	a.time = 0.0;
	a.sta = &station;
	for(i=0;i<nslow;i++)
	{
		slow = (Slowness_vector *) gettbl(utbl,i);
		for(k=0;k<natimes;k++) 
			if(allarids[k] == slow->arid) break;
		if(k>=natimes)
		{
		/* Only when the above search fails will we actually do anything 
		in this loop */
			a.arid = slow->arid;
			strcpy(a.sta->name,slow->array->name);
			a.sta->lat = slow->array->lat;
			a.sta->lon = slow->array->lon;
			a.sta->elev = slow->array->elev;
			a.phase = slow->phase;
			tto = calculate_travel_time(a,h,ALL);
			if(tto.time == TIME_INVALID)
			{
				elog_log(0,"save_predarr failed to compute predicted arrival times for station %s and phase %s\n", 
					atimes->sta->name, atimes->phase->name);
				++errors;
				continue;
			}
			time = h.time + tto.time;
			dip = compute_dip(tto);
					
			u_calc = calculate_slowness_vector(*slow,h,RESIDUALS_ONLY);
			if(u_calc.ux == SLOWNESS_INVALID)
			{
				elog_log(0,"predarr failed to compute slowness vector for station %s and phase %s\n",
					atimes->sta->name, atimes->phase->name);
				++errors;
				if(dbaddv(db,0,
					"arid",atimes->arid,
					"orid",orid,
					"time",time,
					"esaz",esaz,
					"dip",dip,
					NULL ) == dbINVALID)
				{
				    elog_log(0,"dbaddv on predarr table for station %s and phase %s\n",
					atimes->sta->name, atimes->phase->name);
				    ++errors;
				}
				
			}
			else
			{
				utotal = hypot(u_calc.ux,u_calc.uy);
				utotal *= KMPERDEG;
				seaz = compute_seaz(u_calc);
				if(stavel_ok) ema = compute_ema(&u,&u_calc,db,vmodel);
				if(ema < 0.0) ema = ema_null;
				if(dbaddv(db,0,
					"arid",atimes->arid,
					"orid",orid,
					"time",time,
					"slow",utotal,
					"seaz",seaz,
					"ema", ema,
					"esaz",esaz,
					"dip",dip,
					NULL ) == dbINVALID)
				{
				    elog_log(0,"dbaddv on predarr table for station %s and phase %s\n",
					atimes->sta->name, atimes->phase->name);
				    ++errors;
				}
			}
		}
	}
	free(allarids);
	return(errors);
}
 
 
