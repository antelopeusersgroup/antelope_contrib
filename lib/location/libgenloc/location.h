#define FLT_EPSILON     1.192092896E-07F

/*  ----INCLUDE FILE FOR HYPOCENTER LOCATION PROGRAM ------
WARNING:  ANTELOPE includes must precede this include file or typdefs
like Arr and Tbl will not be defined:  stock.h, arrays.h */

/* This definition probably should have been used to simply some 
later typdefs, but inertia prevails */
typedef struct Point {
	double lat, lon, z;
}  Point;

/* This is the generic structure passed to a travel time calculator 
that contains all the information it would need to calculate 
travel times, slowness vectors,  and/or partial derivatives of either */

typedef struct Ray_Endpoints {
	char *sta;  /* station name.  Inserted in anticipation of 3d, station 
		oriented tables.  */
	double slat, slon, rlat, rlon;  /* source receiver latitude longitude*/
	double sz, rz;  /* source, receiver depth below datum */
} Ray_Endpoints;

/* Structure returned by a generic travel time function.  Not everything
need be filled in by a function. */
typedef struct Travel_Time_Function_Output {
	double time;
	double dtdx, dtdy, dtdz;  
	double dtdtau;  /* origin time -- either 0 or 1 */
} Travel_Time_Function_Output;
typedef struct Slowness_Function_Output {
	double ux, uy;
	double duxdx, duxdy, duxdz;
	double duydx, duydy, duydz;
}Slowness_Function_Output;
/* Definition of a general table to be used for either travel times or
slowness as a function of epicentral distance (X) and source depth (Z).  
This structure assumes the table is uniformly spaced at intervals of 
dx in X and dz in Z with nx points in the X direction and nz points along
Z.  Both tables are assumed to begin at (X,Z) = (0.0,0.0).  
The table matrices hold the tables in [X][Z] order.  Obviously they
must be allocated dynamically with dmatrix or an equivalent. The values
are stored in the values matrix.  An implementation must decide two 
issues: (1) How to specify an undefined value (e.g. S travel times 
beyond the core shadow), and (2) How to flag discontinous points 
(e.g. slowness values (travel time slopes) near a crossover point.)
In this code we utilize a parallel character array with codes that
define properties of the ray path with which the corresponding grid
point is associated.  These definitions follow:
*/
#define TURNING 't'  /* Refracted wave = turning wave for global problems.
			Key point is this is a valid entry, and the 
			ray it is associated with starts out downward from
			the source. */
#define UPWARD 'u'	/*Point is associated with a ray path that begins
			with an upward directed ray path segment.  This
			can include direct waves for deep sources and
			phases like pP. */
#define CROSSOVER 'c'	/* Flags points with a discontinuity in the slope
			of the travel time curve = refraction crossover or
			triplication.  The grid point marked this way implies
			the interval ahead of it in distance contains
			a crossover.  This means interpolating time will
			be ok, but the slope will be discontinuous.*/
#define JUMP 'j'	/*Marks a major jump that will yield problems if
			an attempt is made to interpolate across it. */
#define NOT_OBSERVABLE 'n' /* Marks points where this phase should not
			be observed (e.g. a shadow zone) */

#define TIME_INVALID -1.0 /* returned by travel time calculators when 
			time could not be calculated.*/
#define SLOWNESS_INVALID 1.0e30 /* Same for slowness. scale dependent, but
				a large constant like this should work */
typedef struct XZ_table_uniform {
	double x0, z0;  /* allows first point to not be at 0,0 */
	double dx, dz;
	int nx, nz;
	double **values, **slopes;
	char **branch;
	double *velocity;  
} XZ_table_uniform;
/* comparable structure for rectilinear tables.  That means the grid can
be variably spaced in x and z, but only in a rectilinear fashion.  That is,
x[i], z[j] are the distance and depth respectively for the table values
defined as values[i][j] and slopes[i][j].  Said yet another way, we have
irregular points along the x and z axes, but the grid points are defined
by the intersection of the grid lines these points define. */
typedef struct XZ_table_rectilinear {
	int nx, nz;
	double *x, *z;
	double **values, **slopes;
	char **branch;
	double *velocity;  
} XZ_table_rectilinear;

/* Distance weighting in this code uses a linear interpolation on
an irregularly spaced set of nodes (1d).  This is the base type 
that is alloced.  At the moment they are just xy pairs.  Note that
the final entry for delta MUST be 360.0 (one circle around the earth).
This is used internally as the key to mark the last point.  If 360.0 is
not entered explicitly, the last point will be inserted as (360.0,0.0) */
typedef struct Distance_weight_function {
	float delta;  /* great-circle distance in degrees */
	float weight;  /* Corresponding weight value for node point at delta */
	float slope;  /* precomputed slope of forward difference from each
			delta,weight pair. */
} Distance_weight_function;

/* This handle allows a generalization of phase information. */
typedef struct Phase_handle {
	char *name;  /* phase name */
	Distance_weight_function *arrival_time;  
		/* Weight function assigned to arrival times for this phase */
	Distance_weight_function *ux,*uy;  /* weight functions for slowness */
	float deltat0, deltau0;  /* default uncertainties in measured arrival
				times and slownesses respectively */
	double deltat_bound, deltau_bound;  /* travel time and slowness bound scaling constants
					for this phase -- used in emodel error calculation */
	Arr *time_station_corrections;  /* Associative array of station corrections 
					keyed by station name */
	Arr *ux_sc, *uy_sc;  /* slowness station corrections, another 
				associative array */
	Travel_Time_Function_Output (*ttcalc)();   /* Function to calculate
 		travel times for this phase */
	Slowness_Function_Output (*ucalc) ();  /* Function to calculate 
		slowness vector for this phase */
} Phase_handle;

/* This is used to hold station table as an array of this type of structure */
typedef struct Station {
	char name[12];
	double lat;
	double lon;
	double elev;
} Station;

typedef struct Residual {
    float weighted_residual,
          raw_residual, 
	  residual_weight,
	  other_weights ; 
} Residual ;
/* This structure holds raw arrival time information.  A vector 
of these objects defines the raw arrival time data */
typedef struct Arrival {
	int	arid ;
	int arid2;  /* set only for things like S-P to define
		arid of second element of pair */
	Station *sta; /* pointer back to row of Station structure */
	Phase_handle *phase;  /* handle to phase definition object */
	double time;  /* epoch time of this arrival */
	double deltat;  /* Measurement uncertainty in this phase */
	Residual res ;
} Arrival;

/* Equivalent to Station for Arrays.  They are presently identical, but
divergence may occur here in the future. */
typedef struct Seismic_Array {
	char name[12];
	/* Coordinates of array reference position */
	double lat;
	double lon;
	double elev;
} Seismic_Array;    
/* This structure defines raw slowness measurements by an array */
typedef struct Slowness_vector {
	int	arid ;
	Seismic_Array *array;  /* pointer to row of Seismic_Array table */
	Phase_handle *phase;  /* handle to phase definition object */
	double ux,uy;  /* Slowness vector components */
	double deltaux,deltauy;  /* measurement uncertainty in slowness components*/
	Residual xres, yres ;
} Slowness_vector;
/* A hypocenter location */
typedef struct Hypocenter {
	double  dx,dy,dz,dt;  /* Adjustment vector in km x+east, y+north, 
				z+down, t = time (units = seconds)
				from 0 to lat,long, z, time */
	double lat,lon,z;  /* space coordinates of this hypocenter */
	double time;  /* epoch time of origin time */
	double lat0,lon0,z0,t0;  /* Hypocenter from previous iteration */
	double rms_raw, rms_weighted, interquartile;  /* Residual statistics.
				Notice that the statistics listed here are
				the location at lat,lon,z, time*/
	int number_data, degrees_of_freedom;  /* Actual number of data points
		and effective degrees of freedom = int(sum residual weights)
							- number parameters */
	int used;   /*boolean used in multievent locations only */
} Hypocenter;
/* These structures are used for S-P times (or other minus
phase types.  The time interval structure is used to define
epoch time intervals.  These are used in in the Bad_Clock 
structure to define time intervals that have clocks that should
be considered completely unreliable.  */
typedef struct Time_Interval {
	double tstart;
	double tend;
} Time_Interval;
typedef struct Bad_Clock_ {
	char sta[10];  /* station this tags */
	int alltime;  /* if set nonzero assume clocks is always bad */
	Tbl *badtimes;  /* This tbl contains pointers to 
			Time_Interval structures that define
			time intervals when the clock was bad */
} Bad_Clock;

/* this defines list of options for items below */

/* These are for weighting functions  */
#define NONE  0
#define HUBER 1     /* residual weighting using the Huber formula */
#define THOMSON 2   /* residual weighting using Thomson's redescending formula */
#define BISQUARE 3  /* residual weighting using the Tukey's bisquare function */
#define GENERIC 4   /* This means a weighting function specified as x,y pairs
		 	WARNING:  not presently implemented*/

/* These define type of generalized inverse solution to be used */
#define PSEUDOINVERSE 0
#define DAMPED_INVERSE 1
#define PSEUDO_RECENTERED 2
#define DAMPED_RECENTERED 3




/* We store all the location options in this structure to allow 
changing the range of bells and whistles without having to change
a long list of arguments. */
typedef struct Location_options {
	/* these are switches for different weight options */
	int atime_residual_weight;
	int atime_distance_weight;
	int slow_residual_weight;
	int slow_distance_weight;
	/* global scale factor for slowness data relative to arrival data */
	float slowness_weight_scale_factor;
	/* These are allowed range of scaled rms residuals in residual weighting*/
	float min_error_scale, max_error_scale;  

	int fix[4];    

	int generalized_inverse;  /* set to define values above */
	/* These are damped inverse parameters.  Damping parameter is 
	relative to largest singular value of matrix, and min and max
	specify the allowed range of damping paramter.  In Marquardt's 
	method the damping is repeatedly multiplied by damp_adjust_factor 
	up or down depending on whether or not rms increases or decreases.
	Two restictions are enforced.  damp_adjust_factor must be positive
	and > 1.0 and min_relative_damp must be larger than FLT_EPSILON */
	float min_relative_damp, max_relative_damp, damp_adjust_factor;
 
	/* pseudoinverse solution parameters.  Only parameter is relative
	truncation parameter.  We truncate singular values smaller than
	the largest singular value times this constant */
	float sv_relative_cutoff;

	/* Depth control variables.  Depth is not allowed over the ceiling
	or below the floor.  This is useful for stability. */
	float depth_ceiling, depth_floor; 
	/*Closely associated with the above are the step length adjustment
	 factor used to stabilize steps that would adjust the solution 
	outside the ceiling or floor.  Anytime this would happen, the step
	size is scaled down by step_length_scale_factor factors until the
	solution fits within the ceiling and floor bounds.  This can be
	dangerous, however, if allowed to proceed forever so we also 
	specify a min step size factor to prevent situations 
	that would never converge because the step keeps being made tiny.
	When the factor hits the minimum, the depth is fixed at the ceiling
	or floor.  Warning:  THESE CONSTANTS MUST ALL BE LESS THAN ONE
	OR THEY DO MORE HARM THAN GOOD*/
	float step_length_scale_factor, min_step_length_scale;

	/* Maximum number of times to adjust hypocenter location. 
	Only two options are conceivable:  some large number or 1. */
	int max_hypo_adjustments;  

	/* Relative RMS convergence criteria.  Convergence defined when
	delta rms/rms < this value (note weighed residual that is */
	float relative_rms_convergence;

	/* convergence criteria on distance of step (km) */
	float dx_convergence;
	 
} Location_options;

/* These are the statistical quantities we utilize for robust 
measures of scale and center */
typedef struct Robust_statistics{
	double median;
	double q1_4,q3_4;
} Robust_statistics;

/* This holds a generic predicted data and associated partial 
derivatives wrt to hypo variables */
typedef struct Hypo_partials
{
	double data;  /* Predicted data value */
	double dbdx, dbdy, dbdz, dbdt;  /* partials of data wrt x,y,z,
			and t respectively */
} Hypo_partials;
/* Switches for form_equations.  If RHS_VECTOR_ONLY, only the right
hand side vector of weighted residuals is computed by form_equations.
If set to ALL, partials are also computed.  Mode is passed to travel
time and slowness vector calculators as as switch in form_equations*/
#define RESIDUALS_ONLY 1
#define ALL 2

/* These define norm options switchs for grid search algorithm. */

#define RAW_RESIDUALS 0
#define RESWT_ON 1
#define DISTANCE_WT_ON 2
#define ALL_WEIGHTS 4

/* These define gridtype used */
#define POLAR 1
#define LAT_LON_GRID 2
#define CARTESIAN_GRID 3

/* This is the control structure for the grid search location code */
typedef struct Gridloc_options {
	int residual_norm;  /* switch for norm to minimize in grid */
	int gridtype; 
	double lat0, lon0, z0;
	double r1, r2;  
	int nr1, nr2, nz;
	double multiplier;  /* grid is shrunk by this factor each cycle */
	double ncycles;  /* number of cycles for grid scale reduction */
}Gridloc_options;
/* required for error ellipse definitions */
#define CHI_SQUARE 1
#define F_DIST 2
/* It is bad form to nest includes like this, but necessary to 
define function prototypes */
#include "db.h"
/* function prototypes */

float bisquare (float);
double normal_quantile(int, int);
float huber(float);
float thomson(float, float); 
Robust_statistics calc_statistics(float *, int);
float distance_weight(Distance_weight_function *,float);
float distance_weight_time(Arrival, Hypocenter);
float distance_weight_ux(Slowness_vector, Hypocenter);
float distance_weight_uy(Slowness_vector, Hypocenter);
int ggnloc (Hypocenter, Tbl *, Tbl *, Location_options, Tbl **, Tbl **, Tbl **);
Robust_statistics form_equations(int, Hypocenter, Tbl *, Tbl *, Location_options, 
	float **, float *, float *, float *, float *, int *);
void predicted_errors(Hypocenter, Tbl *, Tbl *, Location_options,
	double **, float *);
int save_emodel(int , float *, Dbptr );
int save_predarr(Dbptr, Tbl *, Tbl *, Hypocenter, int, char *);
int project_covariance(double **,int, double *, double, int,
		double *, double *, double *, double *, double *);
Hypocenter initial_locate(Tbl *, Tbl *, Location_options, Pf *);
Hypocenter gridloc(Tbl *, Tbl *, Point *, int, int, Location_options);
void copy_hypocenter(Hypocenter *,Hypocenter *);
double pfget_double_wdef(Pf *, char *, double);
int pfget_int_wdef(Pf *, char *, int);
int lat_lon_grid_setup(Pf *, Point **, int *);
int radial_grid_setup(Pf *, Point **, int *);
void initialize_hypocenter(Hypocenter *);

/* travel time function prototypes, start with the generic version */
Travel_Time_Function_Output calculate_travel_time(Arrival, Hypocenter, int);
Slowness_Function_Output calculate_slowness_vector(Slowness_vector, Hypocenter, int);
/* ttlvz functions */
int ttlvz_init(char *,Pf *);
Travel_Time_Function_Output ttlvz_time_exec(Ray_Endpoints, char *, int);
Slowness_Function_Output ttlvz_slow_exec (Ray_Endpoints, char *, int);
void ttlvz_destroy();

/* table interpolation function prototypes */
int uniform_table_interpolate_init(char *, Pf *);
Travel_Time_Function_Output uniform_time_table_interpolate(Ray_Endpoints, char *, int);
Slowness_Function_Output uniform_slowness_table_interpolate (Ray_Endpoints, char *, int);

/* Generic travel time interface routines */
int ttcalc_interface_init(char *, Pf *);
Travel_Time_Function_Output  ttcalc_interface_exec(Ray_Endpoints, char *, int);
Slowness_Function_Output  ttcalc_interface_slow_exec(Ray_Endpoints, char *, int);

/* Input function prototypes */
Location_options parse_options_pf (Pf *);
Arr *load_station_table(Pf *);
Arr *load_array_table(Pf *);
Arr *parse_phase_parameter_file(Pf *);
Tbl *read_arrivals(Pf *, Arr *, Arr *);
Tbl *read_slowness_vectors(Pf *, Arr *, Arr *);
void free_phase_handle(void *);

/* linear algebra functions - numerical recipe variants */
float **matrix(int, int, int, int);
double **dmatrix(int, int, int, int);
char **cmatrix(int, int, int, int);
void free_matrix(char **, int, int, int);
int svdcmp(float **,int,int,float *,float **);

/* db routines used when data are loaded directly from db */
Arr *dbload_station_table(Dbptr, int, int, Pf*);
Arr *dbload_array_table(Dbptr, int, int, Pf *);
Tbl *dbload_arrival_table(Dbptr, int, int, Arr *, Arr *);
Tbl *dbload_slowness_table(Dbptr, int, int, Arr *, Arr *);
int dbtable_invalid(Dbptr,char *);

/* S-P (or other "minus" phases) related functions*/
int db_badclock_definition(Dbptr, Pf *, Arr *);
void pfget_badclocks(Pf *,Arr *);
void Bad_Clock_free(Bad_Clock *);
int minus_phases_arrival_edit(Tbl *,Arr *,Arr *);
int clock_is_bad(Tbl *,double);

/* $Id$ */

