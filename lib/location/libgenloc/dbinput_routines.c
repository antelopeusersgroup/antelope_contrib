#include <math.h>
#include <string.h>
#include "stock.h"
#include "arrays.h"
#include "db.h"
#include "location.h"
/* This function scans an input database view from row
row_start to row_end.  It builds a station table based on these
data producing a new entry whenever the station name changes.  
This was originally designed to work with the relocate program.
There db is a view made by joining site with the assoc and arrival
tables, and the row range is determined by assoc.  It further
assumes this view has been sorted so all station entries are 
in adjacent rows. 

This routine could also be used if db points at a plain site
table provided one of two things are done first.  (1) the 
site table is sorted by station so that all the station ondate/offdate
entries for that station are together, or (2) the site table is
reduced to a simple table with only one row per station.  

Author:  Gary L. Pavlis
Date:  January 1997
*/

Arr *dbload_station_table(Dbptr db,int row_start,int row_end,Pf *pf)
{
	Arr *a;
        Station *s;
	char *prog="dbform_station_table";
	/* Values set by dbgetv from row of view */
	double lat, lon, elev;
	char staname[12];
	char laststa[12];
	double elev_datum;

	a = newarr(0);
        elev_datum = pfget_double_wdef(pf,"elevation_datum",0.0);
	
	for(db.record=row_start;db.record<row_end;++db.record)
	{
		if((dbgetv( db, 0,
			"site.sta",staname,
			"site.lat",&lat,
			"site.lon",&lon,
			"site.elev",&elev,
			NULL )) == dbINVALID) elog_die(1,"%s:  dbgetv error\n",prog);
		if(db.record == row_start) strcpy(laststa,staname);
		if( (db.record == row_start) || strcmp(staname,laststa))
		{
			s = (Station *) malloc(sizeof(Station));
                	if(s == NULL) 
				elog_die(1,"%s:  Cannot malloc Station structure\n",
					prog);
			strcpy(laststa,staname);
			strcpy(s->name,staname);
			s->lat = lat;
			s->lon = lon;
			elev -= elev_datum;
			s->elev = elev;
			setarr(a,s->name,s);
		}
	}
	return(a);
}
/* this is a parallel routine to dbload_station_table.  In fact, the
code is identical except it returns an associative array of 
Seismic_Array pointers rather than pointers to Station structures.  
This is consistent with CSS3.0 perspective that any station is a
candidate for making slowness measurements, even though that is 
rarely the case in practice.  It makes this table larger than 
necessary, but simplifies a lot of things and makes this code
of more general use.  

Author:  Gary L. Pavlis
Written:  January 1997
*/ 
Arr *dbload_array_table(Dbptr db,int row_start,int row_end,Pf *pf)
{
	Arr *a;
        Seismic_Array *s;
	char *prog="dbform_station_table";
	/* Values set by dbgetv from row of view */
	double lat, lon, elev;
	char staname[12];
	char laststa[12];
	double elev_datum;

	a = newarr(0);
        elev_datum = pfget_double_wdef(pf,"elevation_datum",0.0);

	for(db.record=row_start;db.record<row_end;++db.record)
	{
		if((dbgetv( db, 0,
			"sta",staname,
			"lat",&lat,
			"lon",&lon,
			"elev",&elev,
			NULL )) == dbINVALID) elog_die(1,"%s:  dbgetv error\n",prog);
		if(db.record == row_start) strcpy(laststa,staname);
		if( (db.record == row_start) || strcmp(staname,laststa))
		{
			s = (Seismic_Array *) malloc(sizeof(Seismic_Array));
                	if(s == NULL) 
				elog_die(1,"%s:  Cannot malloc Seismic_Array structure\n",
					prog);
			strcpy(laststa,staname);
			strcpy(s->name,staname);
			s->lat = lat;
			s->lon = lon;
			elev -= elev_datum;
			s->elev = elev;
			setarr(a,s->name,s);
		}
	}
	return(a);
}
/* Helper for below.  Initialize residual structure to appropriate
   values */
void InitializeResidual(Residual *r)
{
    r->weighted_residual=0.0;
    r->raw_residual=0.0;
    r->residual_weight=1.0;
    r->other_weights=1.0;
}
/* Loads and forms Tbl of pointers to arrival structures  (this is
what is returned) from input database view pointed to by 
db.  The routine scans the db view from row_start to row_end
allocating memory for one arrival structure for each row, and 
loading these into a tbl.  It requires input from preexisting
associative arrays stations and arrphases.  The arrival structure
has a pointer to a proper entry for a Station structure that is
cross referenced and set here using getarr.  The same occurs for
arrphase for the Phase handle that is keyed to the arrival phase
filed (P, S, etc.)  


Author:  Gary L. Pavlis
Written:  January 1997
Fix:  Dec. 1998 -- added logic to check timedef and skip an arrival
if the time field is turned off. 
Modified:  Sept 2003
Now gets phase from assoc.phase.  Previously used arrival.iphase
which is not the proper use of the database.
Previous version said it could use a raw arrival table, but this is
no longer true with this change.  I do not believe this will
cause problems unless someone else has used this code.
*/ 


Tbl *dbload_arrival_table(Dbptr db,int row_start,int row_end,
	Arr *stations, Arr *arrphase)
{
	Arrival *a;
	char *prog="dbform_arrival_table";
	Tbl *t;

	/* Values set by dbgetv from row of view */
	char staname[12];
	double time, deltat;
	char phase_name[10];
	char timedef[2];
	

	t = newtbl(0);

	for(db.record=row_start;db.record<row_end;++db.record)
	{
		a = (Arrival *) malloc(sizeof(Arrival));
		if(a == NULL)
				elog_die(1,"%s:  Cannot malloc Arrival structure\n",
					prog);
                InitializeResidual(&(a->res));
		if((dbgetv( db, 0,
			"arid",&(a->arid),
			"sta",staname,
			"assoc.phase",phase_name,
			"arrival.time",&time,
			"arrival.deltim",&deltat,
			"timedef",timedef,
			NULL )) == dbINVALID) elog_die(1,"%s:  dbgetv error\n",prog);

		/* Added to skip arrivals with timedef set to turn off */
		if(!strcmp(timedef,"n"))
		{
			free(a);
			continue;
		}

		a->sta = (Station *) getarr(stations,staname);
		/* This error would be fatal for the relocate program, but
		this function might be used for other interfaces to this
		code so we only make it a warning */
		if(a->sta == NULL)
		{
			elog_complain(1,"%s:  Cannot find coordinates for station %s\n%s phase arrival for this station skipped\n",
				prog, staname, phase_name);
			free(a);
			continue;
		}				
		a->time = time;
		a->phase = (Phase_handle *) getarr(arrphase,phase_name);
		if(a->phase == NULL)
		{
			elog_complain(1,"%s:  Don't know how to handle phase %s\nArrival at %s at time %f skipped\n",
				prog,phase_name,staname,time);
			free(a);
			continue;
		}
		/* This assumes nonset values of the uncertainty in an arrival
		table are returned as a negative number.  Because how one 
		defines a nonset value in a database is variable, this could
		produce problems if this were changed */
		a->deltat = deltat;
		if( (a->deltat) <= 0.0 ) a->deltat = (double)a->phase->deltat0;
                pushtbl(t,a);
	}
	return(t);
}
/* Parallel routine for slowness vector measurements.  The argument list
and the general approach is identical to dbload_arrival_table above.  
The exception is the arrays argument which is an Arr of Seismic_Array 
pointers rather than Station pointers.  The more important difference is
that each row of the database table will have a valid arrival
time, but not all stations measure slowness vectors.  Only arrays measure
slowness vectors.  This is handled implicitly here in a way that MUST
be recognized.  The approach is to check the value of the "slow" attribute
in the arrival table.  If this value is set, the program assumes this is
a valid slowness value.  It then access the slowness fields and converts
them to slowness components used internally in genloc.  

Author:  Gary L. Pavlis
Written:  January 1997
*/ 
#define KMPERDEG 111.320

Tbl *dbload_slowness_table(Dbptr db,int row_start,int row_end,
	Arr *arrays, Arr *arrphase)
{
	Slowness_vector *u;
	char *prog="dbform_slowness_table";
	Tbl *t;

	/* Values set by dbgetv from row of view */
	char staname[14];
	double slow,azimuth;
	double delslo;
	char phase_name[10];
	long arid;
	

	t = newtbl(0);

	for(db.record=row_start;db.record<row_end;++db.record)
	{
	    int retcode;
	    char slodef[2],azdef[2];
	    /* First make sure def is set for slowness vectors */
	    retcode=dbgetv(db,0,"slodef",slodef,"azdef",azdef,NULL );
	    if(retcode==dbINVALID) elog_die(1,"%s: dbgetv error in read_slowness_vector\n",prog);
	    /* don't load data unless slowness vector is defined */
	    if( (!strcmp(slodef,"d")) && (!strcmp(azdef,"d")) )
	    {
		/* I (glp) am inserting a prejudice here.  Uncertainties
		in azimuth are hard to convert to a useable form in genloc
		because of the exclusive use of cartesian components rather
		than the polar form.  The approach here is to use the value
		of delslo, when set, and use the same value for both components.
		This is reasonable for most arrays with a nearly round beam
		pattern, but will be quite wrong in colored noise.*/

		retcode=dbgetv( db, 0,
			"arid",&arid,
			"sta",staname,
			"phase",phase_name,
			"slow",&slow,
			"delslo",&delslo,
			"azimuth",&azimuth,NULL );
		if(retcode==dbINVALID) elog_die(1,"%s: dbgetv error in read_slowness_vector\n",prog);

		/* The current schema says slow is set to -1.0 for a null
		value.  I'll use a safer test for >= 0 since negative 
		slowness is always meaningless. */
		if(slow >= 0.0 )
		{
			u = (Slowness_vector *) malloc(sizeof(Slowness_vector));
			if(u == NULL)
				elog_die(1,"%s:  Cannot malloc Slowness_vector structure\n",
					prog);
			u->arid=arid;
			u->array = (Seismic_Array *)  getarr(arrays,staname);

		/* This error would be fatal for the relocate program, but
		this function might be used for other interfaces to this
		code so we only make it a warning */
			if(u->array == NULL)
			{
				elog_complain(1,"%s:  Cannot find coordinates for station %s\n%s phase arrival for this station skipped\n",
				    prog, staname, phase_name);
				free(u);
				continue;
			}
			u->phase = (Phase_handle *) getarr(arrphase,phase_name);
			if(u->phase == NULL)
			{
				elog_complain(1,"Warning (%s):  No phase handle for phase name %s\nSlowness vector for array %s skipped\n",
				   prog,phase_name,staname);
				free(u);
				continue;
			}

		/* CSS3.0 records slowness vectors in polar form with 
		the magnitude measured in second/degree and azimuth in
		degrees.  genloc uses only components internally in
		units of seconds/km.  We now convert the slow, azimuth
		values to ux and uy in s/km.  Oh, another possible
		confusion.  For all the data I've seen, azimuth is 
		really the receiver backazimuth which points 180 
		degrees away from the propagation direction.  Internally
		I use the azimuth, so this formula makes that conversion
		too through the negative sign on each component.*/
			slow /= KMPERDEG;
			u->ux = -slow*sin(azimuth*M_PI/180.0);
			u->uy = -slow*cos(azimuth*M_PI/180.0);
		/* her we set the uncertainties equal to delslo as noted above */
			if(delslo > 0.0)
			{
				u->deltaux = delslo/KMPERDEG;
				u->deltauy = delslo/KMPERDEG;
			}
			else
			{
				u->deltaux = (double)u->phase->deltau0;
				u->deltauy = (double)u->phase->deltau0;
			}
                	pushtbl(t,u);
		}
	    }
	}
	return(t);
}



/* $Id$ */
