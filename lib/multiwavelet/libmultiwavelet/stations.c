/* This collection of functions read basic input needed for multiwavelet
array processing code.  Although I haven't written them all yet they
will probably all have a pf pointer as an argument.

Author of all:  G Pavlis
Written:  August 19, 1998 +
*/
#include <stdio.h>
#include <stdlib.h>
#include "multiwavelet.h"


/* small companion initializer to function below */
void initialize_MWstation(MWstation *s,int nbands)
{
	int i;
	s->sta = NULL;
	s->lat = 0.0;
	s->lon = 0.0;
	s->elev = 0.0;
	s->dnorth = 0.0;
	s->deast = 0.0;
	s->refsta = NULL;
	s->clock_is_bad = 0;
	s->weights = NULL;
	s->current_weight_base = 1.0;
	s->residual_weight = 1.0;
	s->vp0 = 5.0;
	s->vs0 = 3.5;  
	s->initial_static = 0.0;
	s->elevation_static = 0.0;
	s->plane_wave_static = 0.0;
	s->residual_static = 0.0;
}

/* free routine from MWstation objects */
void free_MWstation(MWstation *s)
{
	free(s->weights);
	free(s->refsta);  /* warning -- it would be easy to evolve
				code to make this pointer be 
				single valued for many s */
	free(s->sta);
	free(s);
}

/* This function establishes the list of all stations that will actually
be used for processing.  I creates an associative array of MWstation 
objects keyed by the station name.  Note it is important to realize
the structure this function creates is NOT completely filled in, but
every element is at least initialized.  (see above).  The NULL pointers
are especially dangerous if not filled in.  

The main element of the MWstation structure that this function fills in
is the weights vector.  This vector is an array of weights used for
forming the stack in a given frequency band.  That is, weight[i] is
the weight given traces for this station in wavelet band i.

arguments:
	pf = input pf object to be parsed
	nbands = number of frequency bands to use in processing = length
		of weights vector created in s->weights

The weights array is created cautiously using nbands.  The input line
is parsed and if there are insufficient weights listed in the input
a diagnostic will be issued and the undefined weights set to 1.0.
If there are more numbers listed than required the list will be silently
truncated.  

History:
Created summer 1999

Modified march 2000

Added code for stations with bad timing.  
*/
Arr *create_station_objects(Pf *pf, int nbands)
{
	int i,j,k;
	Arr *a;
	Tbl *t;
	MWstation *s;
	char *sta;
	char *line,*word;
	char *white=" \t";

	t = pfget_tbl(pf,"station_weights");
	if(t == NULL) elog_die(0,"station_weights table not in parameter file\n");

	a = newarr(0);

	for(i=0;i<maxtbl(t);i++)
	{
		line = gettbl(t,i);
		sta = strtok(line,white);

		s = (MWstation *)malloc(sizeof(MWstation));
		if(s==NULL) 
		  elog_die(0,"Cannot malloc MWstation structure for station %s\n",sta);
		initialize_MWstation(s,nbands);
		allot(double *,s->weights,nbands);
		s->sta = strdup(sta);

		for(j=0;j<nbands;j++) 
		{
			word = strtok(NULL,white);
			if(word == NULL)
			{
				elog_notify(0,"error in station_weights parameter inputfor station %s\nExpected list of %d weights, found only %d\n",
					sta,nbands, j);
				if(j==0)
				{
					elog_notify(0,"No weights defined in any band for station %s\nSetting all weights to 1.0\n",
						sta);
					for(k=0;k<nbands;++k)s->weights[k]=1.0;
				}
				else
				{
					elog_notify(0,"Setting weights for station %s above band %d to %lf\n",
						sta,j-1,s->weights[j-1]);
					for(k=j;k<nbands;++k)
						s->weights[k]=s->weights[j-1];
				}
				break;
			}
			s->weights[j] = atof(word);
		}
		/* we set the clock_is_bad variable false here and depend
		upon the genloc bad clock definitions to set a station as
		being always bad with this flag */
		s->clock_is_bad = 0;
		setarr(a,s->sta,s);
	}

	return(a);
}

/* This little function loads the v0 field of the MWstation structure.  
Note that stations not listed in this table that appear in the defining
station_weights table will be silently given the default (defined
in the initializer above.

The function returns the number of stations it set v0 for (not necessarily
the same as the number in the list -- complains if this happens).  Calling
program should check this value against the number it expected and complain
if they do not match. */

int load_surface_velocity(Pf *pf, Arr *a)
{
	int nset;
	Tbl *t;
	char sta[10];
	double vp,vs;
	char *line;
	MWstation *s;
	int i;

	t = pfget_tbl(pf,"surface_velocities");
	if(t == NULL) elog_die(0,"surface_velocities table missing from parameter file\n");

	for(i=0,nset=0;i<maxtbl(t);i++)
	{
		line = gettbl(t,i);
		if((sscanf(line,"%s %lf %lf",sta,&vp,&vs)) != 3)
		{
			elog_notify(0,"Syntax error reading line from surface_velocity table\nOffending line->%s\n",
				line);
			continue;
		}
		s = (MWstation *)getarr(a,sta);
		if(s == NULL) 
		{
			elog_notify(0,"Station %s listed in surface_velocity table not found in master table\n",
				sta);
			continue;
		}
		s->vp0 = vp;
		s->vs0 = vs;
		++nset;
	}
	return(nset);
}

/* This short little function returns a character string defining
the reference array station name.  It is simple because the
function above that sets the s Arr, requires that all 
entries have the same refsta.  Consequently, all this does is
grab the first item in the list. */

char *get_refsta(Arr *s)
{
	char *ref;
	MWstation *station;
	char *key;
	Tbl *t;

	t = keysarr(s);
	key = gettbl(t,0);
	station = (MWstation *) getarr(s,key);
	ref = strdup(station->refsta);
	freetbl(t,0);
	return(ref);
}

/* This function fills in the station geometry information in the MWstation 
structures indexed via the associative arrray a.  db is the input database.
The routine looks up the site table (css3.0 specific) only.  
This routine is slightly overkill in how it handles the keying of site
under css3.0.  That is, since site is keyed by sta/ondate:offdate technically
a station can move it's location and retain the same station name.  Common
practice is to not do this, however, so we probably don't have to be 
as fussy as is done here.  The algorithm isn't perfect, but is should
work find unless the station does, in fact, move during the time spanned
by the data being processed.  Since the Arr used here keys by only the
station name that would be an impossible problem to deal with.  If that
happens a warning gets issued and the routine continues.  

The routine passes through the full list of stations passed in through
the associative array "a".  Stations in that list will be deleted 
under two circumstances:
process stations will be deleted in one of two situations:
1.  The station is not found in the database.
2.  If deast and dnorth are both zero and this stations != refsta 
(reference station) it is assumed the dnorth and deast values have
not been set in the database.  

The time field is used only if multiple matches are found for a 
station name in the site table.  If that happens the routine hunts
for the first entry that has an ondate after the input defined in "time".

This function returns the number of stations that had the coordinates
set in the MWstation structure.  The calling program should trap the
condition that the count returned is <= 1 (as a minimum).  1 is the 
magic number instead of 0 because the value for refsta will be set, 
although it would normally be zero.  It should probably also 
check the initial length of the Arr and compare it to the count
returned and call complain if they don't match.  This will lead to a
dump of errors internally with elog_notify.

A fatal error occurs if the refsta variable ever changes.  This violates
implicit rules in css3.0, but for this program it is necessary because
it is designed to focus on one array only.  With datascope this is 
not a barrier because one only needs to subset the site table and
use it while running this program.  Since this program is based
on datascope, I viewed this a a non problem.

Author:  G Pavlis
*/ 

int load_station_geometry(Dbptr db, Arr *a, double time)
{
	Dbptr dbs;
	int i,j,nset;
	Tbl *t;
	char *key;
	int yrday;
	Dbptr dbscr;
	static Hook *hook=NULL;
	Tbl *match_tbl;
	MWstation *s;
	int ondate,offdate;
	char refsta[10];  /*This is used as input buffer that
				is duped to store in each s object*/
	char refsta0[10];  /* Used for comparison to guarantee there
				is no change in refsta */

	yrday = yearday(time);
	t = keysarr(a);
	db = dblookup(db,0,"site",0,0);
	dbs = dblookup(db,0,"site",0,0);
	dbs.record = dbSCRATCH;	
	dbaddnull(dbs);

	for(i=0,nset=0;i<maxtbl(t);i++)
	{
		int nmatch;
		key = gettbl(t,i);
		s = (MWstation *)getarr(a,key);
		dbputv(dbs,0,"sta",s->sta,0);
		nmatch = dbmatches(dbs,db,0,0,&hook,&match_tbl);
		if(nmatch == dbINVALID)
			elog_die(0,"dbmatches error looking for entries for station %s\n",s->sta);
		else if(nmatch < 1)
		{
			elog_notify(0,"load_station_geometry:  cannot find station %s in site table -- deleted from geometry list\n",
				s->sta);
			delarr(a,s->sta);
			free_MWstation(s);
		}
		else
		{
			/* when when there is only one match, we just use it, otherwise
			we have to search for the correct entry based on ondate/offdate */

			if(nmatch == 1)
			{
				db.record = (int )gettbl(match_tbl,0);
			}
			else
			{
				for(j=0;j<maxtbl(match_tbl);j++)
				{
					db.record = (int)gettbl(match_tbl,j);
					if(dbgetv(db,0,"ondate",&ondate,
							"offdate",&offdate,0)
						== dbINVALID)
					{
						elog_notify(0,"load_station_geometry:  dbgetv error while searching for ondate/offdate match for station %s\nBlundering on\n", 
							s->sta);
						continue;
					}
					if((yrday >= ondate) && (yrday <= offdate)) break;
				}
			}
			/* note if the date match fails,we still end up here 
			using the last record in the db */
			freetbl(match_tbl,0);
			if(dbgetv(db,0,
				"lat",&(s->lat),
				"lon",&(s->lon),
				"elev",&(s->elev),
				"dnorth",&(s->dnorth),
				"deast",&(s->deast),
				"refsta",refsta,
				0) == dbINVALID)
			{
				elog_notify(0,"load_station_geometry:  dbgetv error reading record %d for station %s in site table\nStation deleted from list\n",
					db.record,s->sta);
				delarr(a,s->sta);
				free_MWstation(s);
			}
			else
			{
				if(nset==0) strcpy(refsta0,refsta);
				s->refsta = strdup(refsta0);
				if(!strcmp(s->sta,refsta0))
				{
					if(((s->dnorth)!=0.0) 
						|| ((s->deast)!=0.0))
						elog_die(0,"load_station_geometry:  reference station in site table must have dnorth and deast set to 0.0\nFound refstat = %s with (dnorth,deast)=(%lf,%lf)\n",
							s->sta,s->dnorth,
							s->deast);
					++nset;
				}
				else
				/* This deletes stations != refsta with dnorth 
				and deast not set properly.  */
				{
					if(((s->dnorth)==0.0) && ((s->deast)==0.0))
					{
						elog_notify(0,"load_station_geometry:  unset dnorth and deast entries for station %s\nStation deleted from list\n",
							s->sta);
						delarr(a,s->sta);
						free_MWstation(s);
					}
					else if(strcmp(refsta0,refsta))
						elog_die(0,"load_station_geometry:  reference station change not allowed.\nAll stations to be processed must have a common refsta in site table\n");
					else
						++nset;
				}
			}
		}
	}		
	return(nset);
}
int load_initial_statics(Pf *pf, Arr *a)
{
	int nset;
	Tbl *t;
	char sta[10];
	double statics;
	char *line;
	MWstation *s;
	int i;

	t = pfget_tbl(pf,"initial_statics");
	if(t == NULL) 
	{
		elog_notify(0,"initial_statics table missing from parameter file.  initial statics all set to 0.0\n");
		return(0);
	}

	for(i=0,nset=0;i<maxtbl(t);i++)
	{
		line = gettbl(t,i);
		if((sscanf(line,"%s %lf %lf",sta,&statics)) != 2)
		{
			elog_notify(0,"Syntax error reading line from initial_statics Tbl\nOffending line->%s\n",
				line);
			continue;
		}
		s = (MWstation *)getarr(a,sta);
		if(s == NULL) 
		{
			elog_notify(0,"Station %s listed in initial_statics table not found in master table\n",
				sta);
			continue;
		}
		elog_log(0,"Setting initials static for %s to %lf\n",
				sta,statics);
		s->initial_static = statics;
		++nset;
	}
	return(nset);
}


/* This is the function actually called to build and fill the multiwavelet station objects.
It basically calls all three functions defined above in the correct sequence.  This 
was an intentional modularization done to simply code maintenance by separating out some
parts linked to different input objects (i.e. pfs versus dbs).  

The function returns an assembled Arr containing pointers to MWstation objects keyed
by the station name.  

Arguments:  
	db - Database pointer to css3.0 database with a site table
	pf - parameter object previously loaded 
	time - epoch time of start of this data set.  

The "time" variable is needed due to a peculiarity of how site is keyed in css3.0.
(see the load_station_geometry function comments above for details.) 

Author:  G. Pavlis
Written:  March 1999
*/
Arr *build_station_objects(Dbptr db, Pf *pf, double time)
{
	int nsta_pf,nsta;  /* nsta_pf is the number of stations defined in the 
				parameter file descriptions while nsta 
				is used for comparison from each function */
	int nbands;  /* number of frequency bands (needed to define weight vectors */

	Arr *a;
	char *refsta;  
	MWstation *s;

	nbands = pfget_int(pf,"number_frequency_bands");

	a = create_station_objects(pf,nbands);

	nsta_pf = cntarr(a);
	nsta = load_surface_velocity(pf, a);
	if(nsta != nsta_pf)
		elog_complain(0,"Surface velocity defined for only %d of %d stations\n",
			nsta,nsta_pf);

	nsta = load_station_geometry(db,a,time);
	if(nsta<=0) 
		elog_die(0,"load_station_geometry failed to match any stations in parameter file with database site table\n");
	if(nsta != nsta_pf)
		elog_complain(0,"Missing entries in site table\nMatched only %d of %d defined in the parameter file\n",
			nsta,nsta_pf);

	/* This is an extra sanity check.  We require refsta to be in the
	site table or all hell will break loose. */
	refsta = get_refsta(a);
	s = (MWstation *)getarr(a,refsta);
	if(s == NULL) elog_die(0,"Station site table error:  reference station %s must appear in site table\n",
		refsta);
	free(refsta);

	nsta = load_initial_statics(pf, a);
	elog_log(0,"Set initial statics on %d stations\n",nsta);
	return(a);
}
