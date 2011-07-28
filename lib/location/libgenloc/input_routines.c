#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "stock.h" 
#include "coords.h"
#include "arrays.h"
#include "pf.h"
#include "location.h"

/* If the step_length_scale_factor is < 1.0, chaos can result.  To avoid 
this, we force this parameter to be less than unity.  If one tries to enter
a number larger than 1, this parameter is set to DEFAULT_SLSF, and the 
min_step_length_scale is set to pow(DEFAULT_SLSF,SLSF_POW_MIN);
*/
#define DEFAULT_SLSF 0.75
#define SLSF_POW_MIN  4.0 

/* This simple function puts all the options for initializing the 
options structure in on place.  It returns a copy of that structure */
Location_options load_default_options()
{
	Location_options opt;
	opt.atime_residual_weight = HUBER;
	opt.atime_distance_weight = 1;
	opt.slow_residual_weight = HUBER;
	opt.slow_distance_weight = 1;
	opt.slowness_weight_scale_factor = 1.0;
	opt.min_error_scale = 1.0;
	opt.max_error_scale = 50.0;
	opt.fix[0] = 0;
	opt.fix[1] = 0;
	opt.fix[2] = 0;
	opt.fix[3] = 0;
	opt.generalized_inverse = DAMPED_INVERSE;
	opt.min_relative_damp = 0.000005;
	opt.max_relative_damp = 1.0;
	opt.damp_adjust_factor = 5.0;
	opt.sv_relative_cutoff = 0.001;
	opt.depth_ceiling = 0.0;
	opt.depth_floor = 700.0;
	opt.step_length_scale_factor = 0.5;
	opt.min_step_length_scale = 0.01;
	opt.max_hypo_adjustments = 50;
	opt.relative_rms_convergence = 0.0001;
	opt.dx_convergence = 0.01;
	return(opt);
}
/* These functions allow defaulting of double and ints respectively.*/
double pfget_double_wdef(Pf *pf, char *keyword, double def)
{
	if(pfget_string(pf,keyword) == NULL)
		return(def);
	else
		return(pfget_double(pf,keyword));
 }
int pfget_int_wdef(Pf *pf, char *keyword, int def)
{
	if(pfget_string(pf,keyword) == NULL)
		return(def);
	else
		return(pfget_int(pf,keyword));
 }
int pfget_boolean_wdef(Pf *pf, char *keyword, int def)
{
	if(pfget_string(pf,keyword) == NULL)
		return(def);
	else
		return(pfget_boolean(pf,keyword));
 }

Location_options parse_options_pf (Pf *pf)
{
	Location_options o;
	char *s;  /* string buffer */
	int i;
	int recenter=0;  /* recenter flag */

	o = load_default_options();
	s = pfget_string(pf,"arrival_residual_weight_method");
	/* Note this implicitly accepts the default if the keyword is missing */
	if(s != NULL)
	{
		if(!strcasecmp(s,"bisquare")) o.atime_residual_weight = BISQUARE;
		if(!strcasecmp(s,"thomson")) o.atime_residual_weight = THOMSON;
		if(!strcasecmp(s,"huber")) o.atime_residual_weight = HUBER;
		if(!strcasecmp(s,"none")) o.atime_residual_weight = NONE;
	}
	/* Do the same for slowness */
	s = pfget_string(pf,"slowness_residual_weight_method");
	if(s != NULL)
	{
		if(!strcasecmp(s,"bisquare")) o.slow_residual_weight = BISQUARE;
		if(!strcasecmp(s,"thomson")) o.slow_residual_weight = THOMSON;
		if(!strcasecmp(s,"huber")) o.slow_residual_weight = HUBER;
		if(!strcasecmp(s,"none")) o.slow_residual_weight = NONE;
	}

	o.atime_distance_weight = pfget_boolean_wdef(pf,
		"time_distance_weighting",o.atime_distance_weight);
	o.slow_distance_weight = pfget_boolean_wdef(pf,
			"slowness_distance_weighting",o.slow_distance_weight);
	o.slowness_weight_scale_factor = (float) pfget_double_wdef(pf,
				"slowness_weight_scale_factor",
				o.slowness_weight_scale_factor);
	o.min_error_scale = (float) pfget_double_wdef(pf,
				"min_error_scale",o.min_error_scale);
	o.max_error_scale = (float) pfget_double_wdef(pf,
					"max_error_scale",o.max_error_scale);
	for(i=0;i<4;++i) o.fix[i] = 0;
	if(pfget_boolean_wdef(pf,"fix_longitude",0)) o.fix[0] = 1;
	if(pfget_boolean_wdef(pf,"fix_latitude",0)) o.fix[1] = 1;
	if(pfget_boolean_wdef(pf,"fix_depth",0)) o.fix[2] = 1;
	if(pfget_boolean_wdef(pf,"fix_origin_time",0)) o.fix[3] = 1;
	if(pfget_boolean_wdef(pf,"recenter",0)) recenter = 1;
	s = pfget_string(pf,"generalized_inverse");
	if(s != NULL)
	{
	    if(!strcasecmp(s,"marquardt"))
	    {
		o.min_relative_damp = (float) pfget_double_wdef(pf,
			"min_relative_damp",o.min_relative_damp);
		if(o.min_relative_damp < FLT_EPSILON)
		{
			elog_log(0,"Warning:  minimum relative damping must be at least single precision epsilon.\nParameter file wanted %f\nReset to %f\n",
				o.min_relative_damp, FLT_EPSILON);
			o.min_relative_damp = FLT_EPSILON;
		}
		o.max_relative_damp = (float) pfget_double_wdef(pf,
				"max_relative_damp",o.max_relative_damp);
		o.damp_adjust_factor = (float) pfget_double_wdef(pf,
			"damp_adjust_factor",o.damp_adjust_factor);
		if(recenter)
			o.generalized_inverse = DAMPED_RECENTERED;
		else
			o.generalized_inverse = DAMPED_INVERSE;
	    }
	    if(!strcasecmp(s,"pseudoinverse"))
	    {
		o.sv_relative_cutoff = (float) pfget_double_wdef(pf,
				"singular_value_cutoff",o.sv_relative_cutoff);
		if(recenter)
			o.generalized_inverse = PSEUDO_RECENTERED;
		else
			o.generalized_inverse = PSEUDOINVERSE;
	    }
	}
	o.depth_ceiling = (float) pfget_double_wdef(pf,
				"depth_ceiling",o.depth_ceiling);
	o.depth_floor = (float) pfget_double_wdef(pf,
			"depth_floor",o.depth_floor);
	o.step_length_scale_factor = (float) pfget_double_wdef(pf,
			"step_length_scale_factor",o.step_length_scale_factor);
	o.min_step_length_scale = (float) pfget_double_wdef(pf,
			"min_step_length_scale",o.min_step_length_scale);
	if(o.step_length_scale_factor >= 1.0) 
	{
		elog_log(0,"Step length damping factors reset\nScale factors must be less than 1.0\nResetting to default of %f\n",DEFAULT_SLSF);
		o.step_length_scale_factor = DEFAULT_SLSF;
		if(o.min_step_length_scale >= o.step_length_scale_factor)
		{
			
			o.min_step_length_scale = (float)pow(DEFAULT_SLSF,SLSF_POW_MIN);
			elog_log(0,"Min step length scale also reset to %f\n",
					o.min_step_length_scale);
		}
	}
	if(o.min_step_length_scale >= o.step_length_scale_factor)
	{
		o.min_step_length_scale = (float)pow(o.step_length_scale_factor,
								SLSF_POW_MIN);
		elog_log(0,"Min step length scale is not consistent with step length scale factor parameter\nReset to %f\n",
			o.min_step_length_scale);
	}
	o.max_hypo_adjustments = pfget_int_wdef(pf,
		"maximum_hypocenter_adjustments",o.max_hypo_adjustments);
	o.dx_convergence = (float) pfget_double_wdef(pf,
		"deltax_convergence_size",o.dx_convergence);
	o.relative_rms_convergence = (float) pfget_double_wdef(pf,
		"relative_rms_convergence_value",o.relative_rms_convergence);
	return(o);
}

/* This routine parses the station table loaded through the parameter file
loaded into the input pf object "pf".  The return is an associative array
containing pointers to Station structure objects.  The pf object is assumed
valid.  That is we assume it isn't null.  
*/

Arr *load_station_table(Pf *pf)
{
	Arr *a;
	Tbl *t;
	int i;
	char *value;

	Station *s;

	double elev_datum;

	a = newarr(0);
	elev_datum = pfget_double_wdef(pf,"elevation_datum",0.0);
	t = pfget_tbl(pf,"seismic_stations");
	for(i=0;i<maxtbl(t);++i)
	{
		s = (Station *) malloc(sizeof(Station));
		if(s == NULL) elog_die(1,"load_station_table:  Cannot malloc station structure entry\n");
		value = gettbl(t,i);
		if(sscanf(value,"%s %lf %lf %lf",s->name,
			&(s->lat),&(s->lon),&(s->elev)) != 4)
			elog_complain(1,"Warning(load_station_table):  \
Read error in station tbl read from parameter file\n\
The following line of the station table was skipped\n%s\n",
				value);
		else
		{
			s->elev -= elev_datum;
			setarr(a,s->name,s);
		}
	}
	return(a);
}
/* This is a companion routine to load_station_table for array beam code
tables.  At the moment it is essentially identical to the load_station_table
function, but changes may eventually occur in the beam table that will
make them diverge so I have produced to seperate functions */
Arr *load_array_table(Pf *pf)
{
	Arr *a;
	Tbl *t;
	int i;
	char *value;

	Seismic_Array *s;

        double elev_datum;
 
        a = newarr(0);
        elev_datum = pfget_double_wdef(pf,"elevation_datum",0.0);
	t = pfget_tbl(pf,"seismic_arrays");
	for(i=0;i<maxtbl(t);++i)
	{
		s = (Seismic_Array *) malloc(sizeof(Seismic_Array));
		if(s == NULL) elog_die(1,"load_array_table:  Cannot malloc Seismic_Array structure entry\n");
		value = gettbl(t,i);
		if(sscanf(value,"%s %lf %lf %lf",s->name,
			&(s->lat),&(s->lon),&(s->elev)) != 4)
			elog_complain(1,"Warning(load_array_table):  \
Read error in array tbl read from parameter file\n\
The following line of the array table was skipped\n%s\n",
				value);
		else
		{
                        s->elev -= elev_datum;
			setarr(a,s->name,s);
		}
	}
	return(a);
}



/* These are companion functions to the "get_phase_handle_parameters" function
below.  We list them first to avoid having to put their prototypes in location.h
They do simple repetitious chores that are the norm in boring i/o routines.
The first is a function to read Tbl entries for distance weight functions.
It takes a pointer returned by pfget_tbl in the calling program.  It scans that
tbl, allocs the vectors that define the general weight function, and returns a 
pointer to a distance weight function array that it allocs and sets. 
*/
Distance_weight_function *setup_weight_function(Tbl *t)
{
	int number_entries;
	Distance_weight_function *f;
	char *entry;
	int i,ii;
	
	/* pfget_tbl returns a NULL pointer if the keyword is not found.  In this
	situation, we issue a complaint and blunder on by setting the weight function
	to 1.0 for all distances */
	if(t == NULL)
	{
		elog_complain(1,"error reading parameter file:  missing distance weight function definition\n");
		f = (Distance_weight_function *)calloc(2,sizeof(Distance_weight_function));
		f[0].delta = 0.0;
		f[0].weight = 1.0;
		f[1].delta = 180.0;
		f[1].weight = 1.0;
		ii = 1;
	}
	else
	{		
		number_entries = maxtbl(t);

		/* We alloc + 1 entries in case the last point is not set correctly.  
		We could realloc, but this makes the code simpler */
		if((f=(Distance_weight_function *)calloc(number_entries+1,
				sizeof(Distance_weight_function))) == NULL)
			elog_die(1,"Cannot alloc memory for distance weight function with %d elements\n",
				number_entries);
		for(i=0,ii=0;i<number_entries;++i)
		{
			entry = gettbl(t,i);
			if(sscanf(entry,"%f %f",&f[ii].delta,&f[ii].weight) != 2)
			{
				elog_complain(0,"Warning:  error reading weight function definition\nRead error from scanf while parsing line %d.  Entries should be ordered pairs.\nEntry skipped\n",i);
				continue;
			}
			if(ii==0)
				++ii;
			else	
			{
				if(f[ii].delta > f[ii-1].delta)
					++ii;
				else
					elog_complain(0,"Warning:  error reading weight function definition\nDeleting entries with nonincreasing distance.  Check phase definition parameter file.\n");
			}
		}
	}
	--ii;
	/* Things are really screwed up if this is executed, but this problem should not really
	be a fatal error */
	if(ii<= 0)
	{
		elog_complain(0,"WARNING:  weight function definition totally botched\nSetting weights to 1.0 for all distances\n");
		f[0].delta = 0.0;
		f[0].weight = 1.0;
		f[1].delta = 180.0;
		f[1].weight = 1.0;
		ii = 1;
	}
	if(f[ii].delta < 180.0)
	{
		elog_complain(0,"Warning:  weight function definition incorrect.\n\
Last point in the definition should be set to distance of 180.0\n\
Setting 180.0 with a weight of 0 and continuing\n");
		++ii;
		f[ii].delta = 180.0;
		f[ii].weight = 0.0;
	}
	/* Now set the precomputed slopes*/
	for(i=0;i<ii;++i)
		f[i].slope = (f[i+1].weight - f[i].weight)/(f[i+1].delta - f[i].delta);
	f[ii].slope = 0.0;
	return(f);
}
/* This function reads a table of station corrections read from a pf file and returns a 
pointer to an Arr (associative array structure) filled with pointers to a double.  It 
reads the entries in the input table assuming the first word in each line is the name
code that is to serve as the key and the second entry is a number defining the station
correction connected with entry "name".  (This can be either a station name or a beam
code in this program.)  The routine allocs a float for each entry and stores the pointer
to this double in the Arr structure with name as the key.  
*/

Arr *parse_station_correction_table(Tbl *t)
{
	char key[10];  /* this size limit comes from station codes in css3.0 database definitions*/
	double *value;
	Arr *a;
	int number_entries;
	int i;
	char *entry;

	number_entries = maxtbl(t);
	a = newarr(0);

	for(i=0;i<number_entries;++i)
	{
		/*We should error check malloc and gettbl below, but neither should 
		cause problems unless something is really screwed up so I won't
		to reduce the number of possible error messages.*/
		value = malloc(sizeof(double));
		entry = gettbl(t,i);
		if(sscanf(entry,"%s %lf",key,value) != 2)
			elog_complain(1,"Warning(parse_station_correction_table): Read error\nCould not parse line->%s",entry);
		else
			setarr(a,key,value);
	}
	return(a);
}

/* This routine reads and sets up an associative array of phase handles
by parsing the Pf object passed as an arguement.  The phase handle array is 
alloced here and we return a pointer to the Arr structure that results.  
Outside functions MUST handle
the dealloc process when this function is called more than once to prevent
a memory leak.  

This procedure allows a dynamic flexible interface to a range of possible 
travel time calculators.  It also utilizes associative arrays to efficiently
index station corrections.  New travel time calculators can be interfaced to
this program easily by adding a hook for that code at the point noted below.  

The parameter file structure this procedure utilizes is rather complex
and most easily seen from examples.  The basic point is it uses nexted
Arr parameters to define the phase handle for each phase. 


Author:  Gary Pavlis
Written:  August 1996
*/  
Arr *parse_phase_parameter_file(Pf *pfall)
{
	Phase_handle *p;
	Arr *aout; /* This is the output Arr */
	Pf *pf,*pf_phase;
	Tbl *t,*tv;
	char *key;
	char *string;
	int i;
	pf_phase = NULL;

	pf = NULL;

	aout = newarr(0);
	/* We first use pfget to get a new handle to a pf object that is 
	keyed to the work "phases".  This lowers us one level in the pf
	tree and allows us to parse the handle for each phase seperately. */

	if(pfget(pfall,"phases",(void **)&pf_phase) != PFARR)
	{
		elog_die(1,"Syntax error in parameter file in phase descriptions\n");
	}
	t = pfkeys(pf_phase);

	for(i=0;i<maxtbl(t);++i)
	{
		/* Now we have to call pfget in it's raw form again to get 
		a handle to Pf at the level of the phase name key */
		key = gettbl(t,i);
		if(pfget(pf_phase,key,(void **)&pf) != PFARR)
			elog_die(1,"Syntax error in parameter file description for phase %s\n",key);

		p = malloc(sizeof(Phase_handle));
		if(p == NULL) elog_die(1,"Cannot malloc Phase_handle structure\n");

		p->name = strdup(key);
		tv = pfget_tbl(pf,"time_distance_weight_function");
		if(tv == NULL)
			elog_log(0,"Phase %s, travel time distance weighting input\n",key);
		/* we call this routine even if tv is null because it does a generic recover, and
		calls complain to dump the previous message along with a more generic one */
		
		p->arrival_time = setup_weight_function(tv);

		/* Nearly identical code for ux and uy */

		tv = pfget_tbl(pf,"ux_distance_weight_function");
		if(tv == NULL)
			elog_log(0,"Phase %s, ux time distance weighting input\n",key);
		p->ux = setup_weight_function(tv);
		tv = pfget_tbl(pf,"uy_distance_weight_function");
		if(tv == NULL)
			elog_log(0,"Phase %s, uy time distance weighting input\n",key);
		p->uy = setup_weight_function(tv);
		p->deltat0 = (float) pfget_double(pf,
				"default_time_uncertainty");
		p->deltau0 = (float) pfget_double(pf,
				"default_slowness_uncertainty");
		p->deltat_bound = pfget_double(pf,"dt_bound_factor");
		p->deltau_bound = pfget_double(pf,"du_bound_factor");
		tv = pfget_tbl(pf,"time_station_corrections");
		if(tv == NULL)
		{
			elog_complain(1,"No time station corrections found for phase %s\nUsing 0.0 for all corrections\n",key);
			/* we load an empty Arr structure in this case to
			avoid explicit error tests in later code.  Not
			finding a station correction will be a common 
			occurrence.*/
			p->time_station_corrections = newarr(0);
		}
		else
			
			p->time_station_corrections = 
				parse_station_correction_table(tv);
		/* Repeat almost the same code for ux and uy */
		tv = pfget_tbl(pf,"ux_station_corrections");
		if(tv == NULL)
		{
			elog_complain(1,"No ux station corrections found for phase %s\nUsing 0.0 for all corrections\n",key);
			p->ux_sc = newarr(0);
		}
		else
			
			p->ux_sc = parse_station_correction_table(tv);
		tv = pfget_tbl(pf,"uy_station_corrections");
		if(tv == NULL)
		{
			elog_complain(1,"No uy station corrections found for phase %s\nUsing 0.0 for all corrections\n",key);
			p->uy_sc = newarr(0);
		}
		else
			
			p->uy_sc = parse_station_correction_table(tv);
		
		/* Now it gets real weird.  Here we set up the travel time 
		calculator function pointers.  The interface to a given
		travel time calculator requires four functions, three of
		which are called here.  They are: (1) an init routine, 
		(2) an exec routine (here loaded in into the p structure), 
		and (3) a destroy procedure.  The later must be executed outside
		this function whenever this parameter file is reloaded.
		The others are used in the way shown below.  Note that if
		the init procedure returns a nonzero value, then we post
		a warning and clear this phase.*/

		string = pfget_string(pf,"travel_time_calculator");
		if(strstr(string,"ttlvz") != NULL)
		{
			if(ttlvz_init(key,pf))
			{
				elog_complain(0,"ttlvz can't calculate travel times for phase %s\nData from this phase will be ignored\n",key);
				free(p);
				continue;
			}
			p->ttcalc = ttlvz_time_exec;
			p->ucalc = ttlvz_slow_exec;
		}
		else if(!strcmp(string,"uniform table interpolation"))
		{
			char *tabfil;
			Pf *pftable;
			tabfil = pfget_string(pf,"table_file");
			if(tabfil == NULL)
			{
				elog_complain(0,"Missing parameter table_file\nTable for phase %s not loaded.  Data for this phase will be ignored\n",key);
				continue;
			}

			if(pfload("GENLOC_MODELS", "tables/genloc", tabfil, &pftable) != 0)
			{
				elog_complain(1,"Pfread error reading travel time table parameter file %s\nData for phase %s will be ignored\n",
					tabfil,key);
				continue;
			}

			if(uniform_table_interpolate_init(key,pftable))
			{
				elog_complain(0,"Error in init function for uniform table interpolator for phase %s\nDAta from this phase will be ignored\n",key);
				free(p);
				continue;
			}
			p->ttcalc = uniform_time_table_interpolate;
			p->ucalc = uniform_slowness_table_interpolate;
			pffree(pftable);

		}
                else if(!strcmp(string,"general"))
		{
			if(ttcalc_interface_init(key,pf))
			{
				elog_complain(1,"Cannot initialize generic travel time calculator for phase %s\n"
					   "Data from this phase will be skipped\n", key);
				free(p);
				continue;
			}
			p->ttcalc = ttcalc_interface_exec;
			p->ucalc = ttcalc_interface_slow_exec;
			
		}
		else
		{
			elog_complain(0,"Unrecognized keyword for travel_time_calculator parameter = %s\nData from phase %s will be ignored\n",
					string,key);
			free(p);
			continue;
		}

		pf = NULL;
		setarr(aout,key,p);
	}
	freetbl(t,0);
	return(aout);
}
/* This is the input read routine for arrival time data.  It reads
the data parameter file (previously read with pfread and assumed to
be pointed to by pf), and loads a stack of Arrival structure entries
into a Tbl.  The function returns a Tbl pointer to the results.
An empty tbl pointer is returned if no phases are read.  
Other arguments:
	phases - associative array of phase handles
	stations - associative array containing station table

Author:  Gary Pavlis
Written: August 1996
*/
Tbl *read_arrivals(Pf *pf, Arr *phases, Arr *stations)
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
		if(a == NULL) elog_die(1,"read_arrivals:  cannot alloc memory for Arrival structure\n");
		sscanf(row,"%s %s %lf %lf %d",phase_name,sta,&(a->time),
						&(a->deltat),&(a->arid));
		a->sta = (Station *) getarr(stations,sta);
		if(a->sta == NULL)
		{
			elog_complain(1,"Warning (read_arrivals):  Can't find coordinates for station %s\n%s phase arrival for this station skipped\n",
				sta, phase_name);
			free(a);
			continue;
		}
		a->phase = (Phase_handle *) getarr(phases,phase_name);
		if(a->phase == NULL)
		{
			elog_complain(1,"Warning (read_arrivals):  No phase handle for phase name %s\nArrival for station %s skipped\n",
				phase_name,sta);
			free(a);
			continue;
		}
		/* Here set set deltat to default when this is required */
		if( (a->deltat) <= 0.0 ) a->deltat = (double)a->phase->deltat0;
		pushtbl(tout,a);
	}
	return(tout);
}

#define KMPERDEG 111.320
#define COMPONENTS 1
#define AZIMUTH 2
#define BACKAZIMUTH 3
/* This is the parallel code to read_arrivals for slowness vector data.
It works identically, but the input table has different entries of course.
Arguments are basically identical, except the seismic_array table is not
quite the same beast as a station table, although it differs little.*/
Tbl *read_slowness_vectors(Pf *pf, Arr *phases, Arr *seismic_array)
{
	Tbl *tin, *tout;
	Slowness_vector *u;
	char *row;
	int i;
	double uconv=1.0;
	int form=COMPONENTS;

	char phase_name[20],array_name[12];

	row = pfget_string(pf,"slowness_units");
	if(row != NULL)
	{
		row = strstr(row,"degrees");
		if(row != NULL)  uconv = KMPERDEG;
	}
	row = pfget_string(pf,"slowness_format");
	if(row != NULL)
	{
		if(!strcasecmp(row,"azimuth"))
			form = AZIMUTH;
		else
			form = BACKAZIMUTH;
	}

	tin = pfget_tbl(pf,"slowness_vectors");

	if(tin == NULL) return(newtbl(0));

	tout = newtbl(0);	

	/* Now we just read each tbl entry string returned by 
	pfget_tbl, convert the string values to entries in the
	Slowness_vector structure, and load the pointers into the tbl
	we will ultimately return */
	for(i=0;i<maxtbl(tin);++i)
	{
		double u1, u2;
		row = gettbl(tin,i);
		u = (Slowness_vector *)malloc(sizeof(Slowness_vector));
		if(u == NULL) elog_die(1,"read_arrivals:  cannot alloc memory for Slowness vector structure\n");
		sscanf(row,"%s %s %lf %lf %lf %lf %d",
			phase_name,array_name,&u1,&u2,
			&(u->deltaux),&(u->deltauy),&(u->arid));
		u->array = (Seismic_Array *) getarr(seismic_array,array_name);
		if(u->array == NULL)
		{
			elog_complain(1,"Warning (read_arrivals):  Can't find coordinates for array %s\n%s phase slowness vector for this array skipped\n",
				array_name, phase_name);
			free(u);
			continue;
		}
		u->phase = (Phase_handle *) getarr(phases,phase_name);
		if(u->phase == NULL)
		{
			elog_complain(1,"Warning (read_arrivals):  No phase handle for phase name %s\nSlowness vector for array %s skipped\n",
				phase_name,array_name);
			free(u);
			continue;
		}
		if(form == AZIMUTH)
		{
			u1 /= uconv;
			u->ux = u1*sin(u2*M_PI/180.0);
			u->uy = u1*cos(u2*M_PI/180.0);
			/* This sets deltaux and uy using only the slowness error field
			if it is not null */
			if( (u->deltauy) <= 0.0 ) u->deltauy = (double)u->phase->deltau0;
			u->deltaux = u->deltauy;
		}
		else if(form == BACKAZIMUTH)
		{
			u1 /= uconv;
			u->ux = -u1*sin(u2*M_PI/180.0);
			u->uy = -u1*cos(u2*M_PI/180.0);
			if( (u->deltauy) <= 0.0 ) u->deltauy = (double)u->phase->deltau0;
			u->deltaux = u->deltauy;
		}
		else
		{
			u->ux = u1/uconv;
			u->uy = u2/uconv;
			/* Here set set deltaus to default when this is required */
			if( (u->deltaux) <= 0.0 ) u->deltaux = (double)u->phase->deltau0;
			if( (u->deltauy) <= 0.0 ) u->deltauy = (double)u->phase->deltau0;
		}
		pushtbl(tout,u);
	}
	return(tout);
}
/* Destroy procedure companions to input functions defined above.  For clarity
they more or less parallel the individual input functions destroying objects
alloced by individual functions.

The first is a simple function that clears the network geometry tables meaning
the array of Station and Seismic_Array structures.  This is a trivial call to 
freearr for both arrays, but modularizes the code as one should never be freed
without freeing the other.  
*/
int destroy_network_geometry_tables(Arr *sta,Arr *sarray)
{
	freearr(sta,free);
	freearr(sarray,free);
	return(0);
}
/* This is a messy function to handle the rather complex object defined by
Phase_handle.  Note that this function is passed to freearr because the
normally a group of phase handles are loaded onto into an associative 
array.  Thus, the normal usage of this function would be 
	freearr(a,free_phase_handle);
where a is the Arr pointer for the array of phase handles.

WARNING:  NEVER EVER CALL THIS ROUTINE AND THEN TRY TO USE DATA TBLS
WITHOUT CALLING read_arrivals AND read_slowness_vectors.  The phase
handle pointers willl point nowhere if you attempt this and chaos will
surely result.
*/
void free_phase_handle(void *value)
{
	Phase_handle *p;
	p = (Phase_handle *)value;
	free(p->arrival_time);
	free(p->ux);
	free(p->uy);
	freearr(p->time_station_corrections,free);
	freearr(p->ux_sc,free);
	freearr(p->uy_sc,free);
	free(p->name);
	free(p);
}
/* Simple function to free up data tables.  This is done mostly for 
modularity as both tables are always freed together */
int destroy_data_tables(Tbl *a, Tbl *u)
{
	if(a != NULL) freetbl(a,free);
	if(u != NULL) freetbl(u,free);
	return(0);
}

/* $Id$ */
