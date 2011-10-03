#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <strings.h>
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "db.h"
#include "elog.h"
#include "location.h"
#include "tt.h"

/* This set of routines implement regionally variable velocity models
using an extended database schema and the generic travel time 
interface (ttcalc - tt(3)) in datascope.  

Author:  Gary L. Pavlis, Kent Lindquist
Written:  July 1999
*/

/*This is a complicated geometric object used internally to 
define a region here.   The object is a polygonal shape in
map view with nvertices defined by the x and y vectors.
(i.e.ordered pairs (x[i],y[i])).  The volume has vertical sides
broken up into nlevels horizontal planes at levels defined
by the ceiling and floor vectors.  i.e. there is a slice
between ceiling[0] and floor[0], ceiling[1] and floor[1], etc.
sitecors is an array of Arr pointers that index station corrections
loaded for each volume keyed by station and phase name.
Note the polygon is defined in a multiplexed type format
in the poly vector */
typedef struct TTregions_volume {
	char *name;  /* the name of this region */
	long level;   /*hierarchic level -- 1 is lowest, higher numbers
			get higher precedence. */
	int nvertices;  
	double *lat,*lon;
	int nlevels;  
	double *ceiling, *floor;
	Arr **sitecors;
	char *method;  /*full method string passed to ttcalc*/
	char *modelname;
} TTregions_volume;

/* Each tt function uses a hook stored in static memory to 
keep track of what it chooses.  This is the hook used for 
this calculator */
typedef struct TTregions_hook {
	char name[30];  /* name of this regional set -- not a single model */
	TTregions_volume *current;  /*We keep the current region 
				and only search the full list when
				a location is not inside this region */
	double lat, lon, depth;  /* We always check this and don't 
				invoke a region search unless the 
				source location (defined here) changes.*/
	Tbl *regions;  /* We maintain a list of all TTregions_volume 
		pointers in this tbl */
	Arr *TThooks;  /* keyed list of hooks to open methods */
} TTregions_hook;

/* These are function prototypes for Kent's winding number algorithm
code */
int is_inside(double, double, double *, int);

/* Shared libraries in Solaris will call an initialization routine 
by this name when the library is first accessed.  This is the
initialization routine for this calculator.  It reads a database
name from the environment using a default if the variable is 
not set.  This is a near duplicate of a routine in libtt1dcvl.
Note this uses an algorithm that may fail with evolution of datascope.
We assume dbopen on an already open database will not return a proper
db pointer and not fail when the model database is already open.  This 
will happen whenever this routine calls the libtt1dcvl calculators.  
*/
static Dbptr modeldb;
#define VMODEL_DBNAME_CUSTOM "VELOCITY_MODEL_DATABASE"
#define VMODEL_DBNAME_DEFAULT "vmodel"

static void ttregions_init();

int _ttregions_init_ran = 0;

void __attribute__ ((constructor))
_ttregions_init()
{
    ttregions_init() ;

    _ttregions_init_ran++;
}

void ttregions_init()
{
	char *dbpath;
	char *dbname;

	dbname=getenv(VMODEL_DBNAME_CUSTOM);
	if(dbname==NULL)
		dbpath = datapath (NULL,"tables/genloc/db",VMODEL_DBNAME_DEFAULT,NULL);
	else
		dbpath = datapath (NULL,"tables/genloc/db",dbname,NULL);
	
	if (dbpath == NULL) { 
	    elog_die( 0, "ttregions database open failed\n" ) ; 
	}
	if(dbopen(dbpath,"r",&modeldb) == dbINVALID) {
	    elog_die(0,"Could not open velocity model database '%s' during libttregions initialization\n"
	    	  "Exiting because all calls to this calculator will fail\n",
		  dbpath);
	}
}


static void free_TTregions_volume(void *rp)
{
	TTregions_volume *r = (TTregions_volume *) rp;
	int i;
	free(r->name);
	free(r->lat); free(r->lon);
	for(i=0;i<r->nlevels;++i)
		if( (r->sitecors[i]) != NULL) freearr((r->sitecors[i]),free);
	free(r->ceiling);  free(r->floor);
	free(r->method);
	free(r);
}

static void ttregions_free_hook(void *hp)
{
	TTregions_hook *h = (TTregions_hook *) hp;

	if( h->regions != NULL ) {
		freetbl(h->regions,free_TTregions_volume);
	}
}


/* This small function sets the sta:phase key used to index
sitecor entries in a given regname.  */
char *make_sta_phase_key(char *sta,char *phase)
{
        int size;
        char *key;
        size = strlen(sta) + strlen(phase) + 2;
        allot(char*,key,size);
        strcpy(key,sta);
        strcat(key,":");
        strcat(key,phase);
        return(key);
}
/* This little function takes a shorthand name for the method
(stored in the algorithm field of the regmodel table) and expands
it to full string expected for "method" field in ttcalc interface.
To simplify life for the user this is extracted from another 
small table called ttcalc.
*/
char *get_full_method_string(char *shortname)
{
	Dbptr db;
	char *method;
	char workstr[100];
	char libname[32],ttentry[32],uentry[32];
	db = dblookup(modeldb,0,"ttcalc",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,"ttregions:  cannot find ttcalc table\n"
			      "Assuming short form of method string for ttcalc=%s\n",
			shortname);
		method = strdup(shortname);
	}
	else
	{
		/* We use the lookup trick here because this table
		is keyed by the algorithm field */
		db = dblookup(db,0,"ttcalc","algorithm",shortname);
		if(db.record == dbINVALID)
		{
			elog_notify(0,"ttregions:  no entry in ttcalc table for algorithm = %s\n"
				      "Attempting to use short form = %s\n",
				      shortname, shortname);
			method = strdup(shortname);
		}
		else
		{
			dbgetv(db,0,"libname",libname,
				"ttentry",ttentry,
				"uentry", uentry, NULL);
			strcpy(workstr,libname);
			strcat(workstr," ");
			strcat(workstr,ttentry);
			strcat(workstr," ");
			strcat(workstr,uentry);
			method=strdup(workstr);
		}
	}
	return(method);
}	

/* This routine is called whenever a new model set is defined.  
This will always be invoked in the first call the ttcalc pointing
at this calculator AND anytime the model set name changes.  

The procedure here loads all the polygon information into memory
for use by the program.  This is essential because the winding 
number algorithm has to choose regions and needs these data in
memory.  We DO NOT load all the sitecor data, but these are loaded
as needed (lazy initialization).  This was done because the sitecor
tables could get huge if the number of regions and number of stations
is large (the table will be of size of the order of number of stations
time number of phases times number of regions).  This will occassionally
bog down the calculator as whenever a new region is encountered we have
to hit the db again, but we'll start with this algorithm anyway.

The function returns a pointer to the Tbl stored in the hook used 
by this calculator.

*/

Tbl *load_regions(char *modelset)
{
	Dbptr dbs, dbreg, dbreg2, dbsitecor,dbsc2;  
	char sstring[40];  
	long nrecs;
	Tbl *sortkeys, *keyreg, *keysc;
	char regname[21],algorithm[16],modname[21];
	char *database_filename;
	long level;
	static Tbl *result, *reg_match, *sc_match;
	int nmatch_reg,nmatch_sc;
	static Hook *reg_hook=NULL, *sc_hook=NULL;
	int i;
	TTregions_volume *r;
	int ndepths;
	double test,ceiling;

	dbquery(modeldb,dbDATABASE_FILENAME,&database_filename);
	sprintf(sstring,"modelset =~ /%s/",modelset);
	dbs = dblookup(modeldb,0,"regmodel",0,0);
	dbs = dbsubset(dbs,sstring,0);
	dbquery(dbs,dbRECORD_COUNT,&nrecs);
	if(nrecs <= 0)
	{
		elog_complain(0,"ttregions:  model set '%s' not found in model database '%s'"
				"\nCheck setting of environment variable '%s', which specifies "
				"a velocity-model database name, and make sure that database "
				"is present in the tables/genloc/db subdirectory of $ANTELOPE/data "
				"or of a directory listed in the DATAPATH environment variable\n",
				modelset,database_filename,VMODEL_DBNAME_CUSTOM);
		return(NULL);
	}
	dbreg = dblookup(modeldb,0,"regions",0,0);
	dbsitecor = dblookup(modeldb,0,"sitecor",0,0);
	if((dbreg.record == dbINVALID) || (dbsitecor.record == dbINVALID))
	{
		elog_complain(0,"ttregions:  database error in velocity-model database '%s'\n"
				"regions and sitecor tables are required\n"
				"Check setting of environment variable '%s', which specifies "
				"a velocity-model database name, and make sure that database "
				"is present in the tables/genloc/db subdirectory of $ANTELOPE/data "
				"or of a directory listed in the DATAPATH environment variable\n",
				database_filename,VMODEL_DBNAME_CUSTOM);
		return(NULL);
	}
	/* It is necessary to sort the regions table to make sure
	the vertices are ordered correctly. */
	sortkeys = strtbl("regname","vertex",NULL);
	dbreg2 = dbsort(dbreg,sortkeys,0,0);
	clrtbl(sortkeys,0);

	/* similarly we have to sort the sitecor table to make
	a linear load below work correctly */
	sortkeys = strtbl("modname","regname","ceiling","sta",NULL);
	dbsc2 = dbsort(dbsitecor,sortkeys,0,0);
	freetbl(sortkeys,0);

	/* We now loop through the regmodel table for this modelset 
	running dbmatches against each row against the sorted
	regions table and the sitecor table.  Before the loop
	we set up the key tbls for dbmatches explicitly rather
	than depend upon primary keys */
	keyreg = strtbl("regname",NULL);
	keysc = strtbl("regname","modname",NULL);
	result=newtbl(nrecs);
	for(dbs.record=0;dbs.record<nrecs;++dbs.record)
	{
		if(dbgetv(dbs,0,"regname",regname,
			"level",&level,
			"algorithm",&algorithm,
			"modname",modname,
			NULL) == dbINVALID)
		{
			elog_complain(0,"dbgetv error on record %ld of subsetted regmodel table for model set '%s'"
					"In database '%s'\nTrucation of model data likely\n",
					dbs.record, modelset, database_filename);
			continue;
		}
		nmatch_reg = dbmatches(dbs,dbreg2,&keyreg,&keyreg,
			&reg_hook,&reg_match);
		nmatch_sc = dbmatches(dbs,dbsc2,&keysc,&keysc,
			&sc_hook,&sc_match);
		/* count the number of distinct depth intervals 
		for this region.  Not very elegant, but since
		this code is only run at initialization should not
		matter. */
		if(nmatch_sc > 0)
		{
			for(i=0,ndepths=0,test=-999999.0;
				i<maxtbl(sc_match);++i)
			{
				dbsc2.record = (long )gettbl(sc_match,i);
				if(dbgetv(dbsc2,0,"ceiling",&ceiling,NULL)
					== dbINVALID)
				{
					elog_complain(0,"dbgetv error reading sitecor table in database '%s'\n"
							"Station corrections read will probably be wrong\n",
							database_filename);
					break;
				}
				if(test != ceiling)
				{
					++ndepths;
					test = ceiling;
				}
			}
				
		}
		else
		{
			ndepths = 1;
		}
		if(nmatch_reg > 0)
		{
			allot(TTregions_volume *,r,1);
			allot(double *,r->lat,nmatch_reg);
			allot(double *,r->lon,nmatch_reg);
			allot(Arr **,r->sitecors,ndepths);
			allot(double *,r->ceiling,ndepths);
			allot(double *,r->floor,ndepths);
			r->nvertices = nmatch_reg;
			r->name = strdup(regname);
			r->modelname = strdup(modname);
			r->level = level;
			r->nlevels = ndepths;
			r->method = get_full_method_string(algorithm);
			/* First we run through the regions table */
			for(i=0;i<maxtbl(reg_match);++i)
			{
				dbreg2.record = (long)gettbl(reg_match,i);
				if(dbgetv(dbreg2,0,
					"lat",(r->lat)+i,
					"lon",(r->lon)+i,
					NULL) == dbINVALID)
				{
					r->nvertices = i;
					elog_notify(0,"dbgetv error on regions table of database '%s' for region '%s'\n"
						      "Polygon truncated to %d points\n",
						      database_filename,r->name,i);
				}
			}
			/* The explicitly null pointers for the sitecors
			vector of Arr are used to trigger loading a 
			sitecor entry in the lazy lazy initialization */
			if(nmatch_sc > 0)
			{
				for(i=0;i<ndepths;++i) r->sitecors[i] = NULL;
			}
			else
			{
			/* enter here if there are no station corrections
			defined for this region.  We create an empty arr
			to distinguish this from just not being loaded.
			Then when a function asks for corrections for 
	 		a station/phase it will correctly return nothing
			instead of seg faulting */
				r->nlevels = 1;
				r->sitecors[0] = newarr(0);
				r->ceiling[0] = -10.0;
				r->floor[0] = 1000.0;
			}
			pushtbl(result,r);
		}
		else
		{
			elog_notify(0,"Warning(libttregions):  No match in regions table of database '%s' "
				      "for regname '%s' defined in regmodel for modelset '%s'\n"
				      "Mismatch on record %ld of sorted regmodel table\n",
				      database_filename, regname, modelset, dbs.record);
		}
		freetbl(reg_match,0);
		freetbl(sc_match,0);
	}
	/* cleanup time */
	dbfree(dbs);  dbfree(dbreg2);  dbfree(dbsc2);
	freetbl(keyreg,0);    freetbl(keysc,0);
	return(result);
}
/* This routine does lazy loading of station correction entries in
a database.  It is closely linked to the load_regions routine above
in that it assumes blindly that the structures it will utilize have
been previously created.  

The basic algorithm here is we lookup the sitecor table
and use dbmatches to find rows that match the input 
modname = char *model, phase = char *phase, and regname = r->name

The function is also passed the argument "r" which is a pointer to 
the ttregions special structure used to define a geometric region
as a 3d prism.  Thus, it is assumed the calling function has
already found the right entry in the table of these structures.
This assumes the routine is called by detecting a NULL pointer to 
the Arr it was searching for.  (this is set in initialization of
load_regions function above).  Note the region name is extracted
from r.  

This is a second generation function produced by fragmentation of
load_regions, so suspect relics of this history if you find a bug.

Author:  G Pavlis
*/
int load_sitecor(TTregions_volume *r, char *model, char *phase)
{
	Dbptr dbsitecor,dbsc2;  
	Tbl *sortkeys, *keysc;
	static Tbl *sc_match;
	int nmatch_sc;
	static Hook  *sc_hook=NULL;
	int i;
	int ndepths=0;
	int idepth=0;
	char sta[8];
	double test,ceiling,floor;
	double *scor;
	char *key;
	char *database_filename;

	dbquery(modeldb,dbDATABASE_FILENAME,&database_filename);
	dbsitecor = dblookup(modeldb,0,"sitecor",0,0);
	if(dbsitecor.record == dbINVALID)
	{
		elog_complain(0,"ttregions:  database error in '%s'\n"
				"Cannot access sitecor table\n"
				"Check setting of environment variable '%s'\n",
			database_filename,VMODEL_DBNAME_CUSTOM);
		return(-1);
	}
	/* We have to sort the sitecor table to make
	a linear load below work correctly */
	sortkeys = strtbl("modname","regname","ceiling","sta",NULL);
	dbsc2 = dbsort(dbsitecor,sortkeys,0,0);
	freetbl(sortkeys,0);

	/* set up for dbmatches by building key tbls and loading
	keys into the scratch record of the sitecor table */
	dbsitecor.record = dbSCRATCH;
	if(dbputv(dbsitecor,0,"modname",model,"regname",r->name,"phase",phase,NULL)
		== dbINVALID)
	{
		elog_complain(0,"ttregions:  dbputv error while trying to load sitecor entries from database '%s' for model='%s',region='%s', and phase='%s'\n"
				"No station corrections loaded for this region\n",
				database_filename,model, r->name, phase);
		return(-2);
	}

	keysc = strtbl("regname","modname",NULL);
	nmatch_sc = dbmatches(dbsitecor,dbsc2,&keysc,&keysc,
			&sc_hook,&sc_match);
	if(nmatch_sc <= 0)
	{
		/* This is likely to be common enough we only log
		it and not view it as an error */
		elog_notify(0,"No sitecor entries found for model:region:phase='%s:%s:%s' in database '%s'\n",model,r->name,phase,database_filename);
		return(0);
	}

	for(i=0,idepth=0;i<maxtbl(sc_match);++i)
	{
		dbsc2.record = (long)gettbl(sc_match,i);
		allot(double *,scor,1);
		if(dbgetv(dbsc2,0,
			 "sta",sta,
			"phase",phase,
			"ceiling",&ceiling,
			"floor",&floor,
			"paramval",scor,NULL)
				  == dbINVALID)
		{
			elog_complain(0,"dbgetv error reading sitecor table record %ld in database '%s'\n"
					"Station correction data probably truncated\n",
					dbsc2.record,database_filename);
			break;
		}
		if(i==0) 
		{
			test = ceiling;
			r->sitecors[0] = newarr(0);
			r->ceiling[0] = ceiling;
			r->floor[0] = floor;
		}
		else if(test != ceiling)
		{
			++idepth;
			/* The line below looks like a clear MISTAKE since ndepths is not computed previously */
			if(idepth >= ndepths)
			{
				elog_complain(0,
				  "sitecor table mismatch in depth count for region '%s' in database '%s'\n"
				  "Overflow of depth arrays prevented at ndepth = %d\n",
					  r->name, database_filename, idepth);
				break;
			}
			r->sitecors[idepth]=newarr(0);
			r->ceiling[idepth] = ceiling;
			r->floor[idepth] = floor;
			test = ceiling;
		}
		key = make_sta_phase_key(sta,phase);
		setarr(r->sitecors[idepth],key,scor);
		free(key);  /*setarr dups the key string*/
	}
	return(0);
				
}
		
#define RADIUS_EARTH 6370.8

/* This routine applies the winding number algorithm to test whether
a give latitude, longitude point (passed through geometry) is inside
the region defined by the rv structure.  A complicating factor is
that the winding number algorithm works only with Cartesian coordinates.
Consequently, the lat,long points defined in rv are first converted
to local coordinate system with an origin at the test point.  This
is based on a polar stereographic projection.  This works only if
the test point and polygon are in the same hemisphere.  For whole
earth problems a different algorithm would need to be used, and 
this projection is not all that accurate for very large regions.  
*/
int point_is_inside(TTregions_volume *rv,TTGeometry *geometry)
{
	double *polygon;  /* polygon is stored multiplexed in
			this vector (i.e. x1,y1,x2,y2, ..., xn,yn) */
	int ncoords;
	int test;  /* return of is_inside function */
	double lat,lon;  /* temporaries used to simplify symbols */
	int i,ii;
	double distance,azimuth;

	ncoords = 2*(rv->nvertices);
	allot(double *,polygon,ncoords);
	lat = geometry->source.lat;
	lon = geometry->source.lon;
	for(i=0,ii=0;i<(rv->nvertices);++i,ii+=2)
	{
		dist(rad(lat),rad(lon),
			rad(rv->lat[i]),rad(rv->lon[i]),&distance,&azimuth);
		if(deg(distance)>90.0)
		{
			elog_log(0,"Polygon for model '%s' located more than 90 degrees from test point at\n"
				   "latitude:  %lf, longitude%lf\n"
				   "Winding number algorithm not called assuming test point is outside polygon\n",
				rv->modelname,lat,lon);
			free(polygon);
			return (0);
		}
		polygon[ii] = RADIUS_EARTH*distance*sin(azimuth);
		polygon[ii+1] = RADIUS_EARTH*distance*cos(azimuth);
	}

	/* first two args here are x and y point to test against polygon.
	Because we translated the origin to the test point, we pass these
	points as zero always. */
	test = is_inside (0.0,0.0,polygon,ncoords);

	free(polygon);
	return(test);
}
/* This is the routine that does a geometric search for what region the
given source falls in.  It uses a dumb, linear search algorithm through
the list of pointers passed through the regions Tbl.  The source 
position used to find a match comes from the geometry structure.  
The routine calls a generic function that answers if the source is
or is not in a particular region.  It maintains a list of matches and
if multiple matches are found, it chooses the region with the highest
"level" defined.  This should allow overlapping regions to be sorted
out properly in most cases, but it adds a large overhead because of the
use of a search over all possible regions.  Future versions might
profit from alternative match functions or a faster but less general
search approach.  
*/


TTregions_volume *search_for_region(TTGeometry *geometry, Tbl *regions)
{
	Tbl *matches;  /* load pointers to all matches into this Tbl */
	int nregions,nmatches;  
	int i;
	TTregions_volume *rv, *rvtest;

	matches = newtbl(0);
	nregions = maxtbl(regions);
	if(nregions<=0)
	{
		elog_complain(0,"Failure in regional calculator initialization\nNo region definitions are loaded\n");
		return(NULL);
	}
	for(i=0;i<nregions;++i)
	{
		rv = (TTregions_volume *)gettbl(regions,i);
		if(point_is_inside(rv,geometry) ) pushtbl(matches,rv);
	}
	nmatches = maxtbl(matches);
	switch(nmatches)
	{
		case(0):
			rv = (TTregions_volume *)gettbl(regions,0);
			elog_complain(0,"Region variable calculator found no match for source at (lat,long) = (%lf, %lf)\nDefaulting to first entry in list with name %s\n",
				geometry->source.lat,geometry->source.lon,rv->name);
			break;
		case(1):
			rv = (TTregions_volume *)gettbl(matches,0);
			break;
		default:
			/* note that this silently uses the first match found if
			multiple matches at the same level are found */
			rv = (TTregions_volume *)gettbl(matches,0);
			for(i=0;i<nmatches;++i)
			{
				rvtest = (TTregions_volume *)gettbl(matches,0);
				if((rvtest->level) > (rv->level) ) rv = rvtest;
			}
	}
	/* this releases the tbl object without destroying the objects
	the tbl actually points at*/
	freetbl(matches,0);
	return(rv);
}
		

/* This routine handles the hook used to store the state of the 
calculator.  The basic algorithm is that a modelset is reloaded
any time the modelset name changes or on startup when hookp is
null.  The more common operation keys on the contents of the 
TTregions_volume structure.  The source location passed in the
"geometry" structure is compared to the current lat,lon, depth
values stored in the hook's TTregions_volume structure.  If 
the coodinates do not match a search routine is called to 
to find the region corresponding to the new location.  Otherwise
the entries in the TTregions_volume structure found in the 
hook are used directly with no further tests.  This should speed
the execution of this function for most applications by minimizing
the number of times the search routine gets called.  It adds a lot
of code as overhead, but this is probably preferable to searching
always.  This will execute fairly fast if the source changes since
the code then just hits three conditionals and returns doing nothing. 
*/

int manage_hook_ttregions (
	char *modelset,
	TTGeometry *geometry,
	Hook **hookp)
{
	TTregions_hook *old;
	TTregions_volume *save;

	if(*hookp != NULL) old = (*hookp)->p;
	if((*hookp == NULL) || 
	   ((old->regions) == NULL) ||
	   (strcmp(modelset,old->name)) )
	{
		if(*hookp != NULL) free_hook(hookp);
		*hookp = new_hook ( ttregions_free_hook );
		allot(TTregions_hook *, old, 1);
		(*hookp)->p = old;
		old->TThooks = newarr(0);
		strcpy(old->name,modelset);
                old->lat = geometry->source.lat;
                old->lon = geometry->source.lon;
                old->depth = geometry->source.z;
		old->regions = load_regions(modelset);
		if((old->regions) == NULL) return(-5);
		old->current = search_for_region(geometry,old->regions);
		if((old->current) == NULL) return(-5);
	}
	/* floating point equality tests are usually evil, but this
	one should be ok provided one doesn't muck with the geometry.
	Note retaining previous region when search fails and 
	related warning message. */
	if( ( (old->lat) != (geometry->source.lat) )
		|| ( (old->lon) != (geometry->source.lon) )
		|| ( (old->depth) != (geometry->source.z) ) )
	{
		save = old->current;  
		old->current = search_for_region(geometry,old->regions);
		if((old->current) == NULL)
		{
			elog_complain(0,"ttregions lookup failed for source at lat=%lf, lon=%lf\nRecovering using previously loaded region named %s\n",
				geometry->source.lat, geometry->source.lon,
				old->current->name);
			old->current = save;
		}
	}
	return(0);
}


int ttregions (
   char *model,
    char *phase_code,
    int mode,
    TTGeometry *geometry,
    Tbl **timesp,
    Hook **hookp
)
{
	int result;
	TTregions_hook *h;
	Hook *current_hook;
	int hook_was_null=0;
	int mode_nosc; 
	TTTime *t;
	

	result = manage_hook_ttregions(model,geometry,hookp);
	if(result < 0) return(result);

	h = (TTregions_hook *)((*hookp)->p);

	/* We have hooks within hooks here.  We maintain a list of 
	hooks for open methods in the regions hook */
	current_hook = (Hook *) getarr(h->TThooks,h->current->method);
	if(current_hook == NULL) hook_was_null=1;
	/* mask off the bits set to turn on station corrections.
	We don't want this function to apply them */
	mode_nosc = mode & (~(TT_APPLY_CORRECTIONS));
	result = ttcalc(h->current->method,h->current->modelname,
			phase_code, mode_nosc, geometry, timesp, &current_hook);
	if(hook_was_null) setarr(h->TThooks,h->current->method,current_hook);

	/* Needs code here for sitecor when requested */
	if(mode & TT_APPLY_CORRECTIONS)
	{
		int idepth;
		char *key;
		double *correction;
		if((h->current->sitecors[0]) == NULL)
		{
			if(load_sitecor(h->current,model,phase_code))
				elog_complain(0,"ttregions:  problem loading station corrections from database for phase %s and model %s\n",
					phase_code,model);
		}
		else
		{
			for(idepth=0;idepth<(h->current->nlevels);++idepth)
			{
				if( ( (h->depth) >= (h->current->ceiling[idepth]) )
					&& ((h->depth) < (h->current->floor[idepth]) ) )
							break;
			}
			if(idepth >= (h->current->nlevels)) idepth = (h->current->nlevels) - 1;
			/* Note this is not general.  It assumes the phase_code is a single 
			phase name.  It will most likely silently do nothing if given 
			a comma separated list */
			key = make_sta_phase_key(geometry->receiver.name,phase_code);
			correction = (double *)getarr(h->current->sitecors[idepth],key);
			free(key);
			if(correction != NULL)
			{
				t = (TTTime *)gettbl(*timesp,0);
				t->value += (*correction);
			}
		}
	}
	return(result);
}
	
int ttregions_ucalc (
   char *model,
    char *phase_code,
    int mode,
    TTGeometry *geometry,
    Tbl **timesp,
    Hook **hookp
)
{
	int result;
	TTregions_hook *h;
	Hook *current_hook;
	int hook_was_null=0;
	int mode_nosc; 
	

	result = manage_hook_ttregions(model,geometry,hookp);
	if(result < 0) return(result);

	h = (TTregions_hook *)((*hookp)->p);

	/* We have hooks within hooks here.  We maintain a list of 
	hooks for open methods in the regions hook */
	current_hook = (Hook *) getarr(h->TThooks,h->current->method);
	if(current_hook == NULL) hook_was_null=1;
	/* mask off the bits set to turn on station corrections.
	We don't want this function to apply them */
	mode_nosc = mode & (~(TT_APPLY_CORRECTIONS));
	result = ucalc(h->current->method,h->current->modelname,
			phase_code, mode_nosc, geometry, timesp, &current_hook);
	if(hook_was_null) setarr(h->TThooks,h->current->method,current_hook);

	if(mode & TT_APPLY_CORRECTIONS)
	{
		elog_notify(0,"ttregions_ucalc:  cannot apply slowness corrections for station %s and phase %s\nSlowness corrections not implemented for this calculator\n",
			geometry->receiver.name, phase_code);
	}
	return(result);
}
	
