/*
Definitions of structures used in multiwavelet routines 
G Pavlis, July 1998
*/

/* Note multiwavelt code uses a series of datascope libraries with a nested
include here.  This is bad form if this were placed in a library, but the
current presumption is that this is an encapsulated set of C programs */
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "db.h"
#include "elog.h"
#include "pf.h"
#include "tt.h"
#include <sunperf.h>

/* This object defines a complex pair of multiwavelet functions.
They are implemented as two vectors rather than a complex function
because we always need to convolve with each vector seperately to 
produce a complex trace as a result */

typedef struct MWbasis_ {
	double f0, fw;  /* nondimensional center frequency and bandwidth*/
	int n;  /* number of points in function */
	float *r, *i;  /* Real and imaginary parts of multiwavelet pair */
} MWbasis;

/* This object defines one element of a multiwavelet transform.
The general transform is an array of these objects */
typedef struct MWtrace_ {
	double dt0, dt;  /* initial (t0) and actual sample interval (s) */
	int decimation_factor;  /* decimation factor for this trace */
	MWbasis *basis;  /* pointer back to basis function used to generate 
				this object */
	double f0, fw;  /* center frequency and bandwidth (in Hz) of 
				this object */
	int nz;  /* Number of samples in this trace */
	double starttime, endtime; /* epoch start and endtimes of trace.
			Defined by center of MWbasis function, not edges */
	complex *z;  /* Complex trace itself (length nz)*/
} MWtrace;

typedef struct FIR_decimation{
	int decfac;
	int ncoefs;
	float *coefs;
} FIR_decimation;
/* These objects contain station specific information used in multiwavelet
array processing.  They encapulate all the station dependent information
required by array processing functions in a single structure */
typedef struct MWstation_ {
	char *sta;  /* station name */
	double lat,lon,elev;  /* standard coordinates */
	double deast,dnorth;  /* css3.0 cartesian coordinates for arrays */
	char *refsta;  /* Name of reference station where deast=dnorth=0*/
	/* set to 1 for stations whose clocks are considered completely
	unreliable */
	int clock_is_bad;
	double *weights;  /* vector of weights for this station.  Length is
				number of bands */
	/* These two number together define current, dynamic weights used
	in a solution.  Current_weight_base is set to the appropriate element
	of the weights array as the solution progresses.  residual_weight
	is set by robust estimation routine used for estimating the 
	slowness vector.  Signal processing routines use the base weight
	only while the residual weight is used only for slowness vector
	and static calculations */
	double current_weight_base;  
	double residual_weight;  
	double vp0, vs0;  /* surface velocities (P and S) 
				to use to compute geometric static */
	double initial_static;  /* optional initial residual static */
	double elevation_static;  /* static due to elevation correction*/
	double plane_wave_static;  /* model based geometric static to account
			for spherical geometry and wavefront curvature*/
	double residual_static;    /* earth structure static value */
} MWstation;

/* This structure defines a slowness vector.  NOTE IMPORTANT:  mwap uses
a slowness vector that points in the direction of propagation.  Note that
css3.0 uses a backazimuth which is 180 degrees from the vector used
internally here.  */
typedef struct MWSlowness_vector_ {
	double ux,uy;	/* slowness vector in Cartesian components */
	char *refsta;  /* reference station name */
} MWSlowness_vector;

/* In the following in this program angles are stored in radians.
We use atan2 convention for phi from -Pi to Pi.  WARNING:
these aren't spherical coordinates as in the textbooks, but spherical
coordinates ala compass azimuth.  i.e. phi is an angle relative to 
north, which by convention is the x2 axis in this code.*/
typedef struct Spherical_Coordinate_{
	double radius;
	double theta;
	double phi;
} Spherical_Coordinate;

/* This structure defines particle motion major and minor ellipses
defined in cartesian coordinates.  Each vector is normalized to
unit length  */
typedef struct Particle_Motion_Ellipse_{
	double major[3];
	double minor[3];
	double rectilinearity;  
} Particle_Motion_Ellipse;
/* Error in particle motion ellipse computed by multiwavelets.  
Internally the angles all have units of radians, but they are converted
to degrees as an external representation in database tables  */
typedef struct Particle_Motion_Error_{
	double dtheta_major,dphi_major;  /* errors in spherical coordinate
					angles for major ellipse direction */
	double dtheta_minor,dphi_minor;  /* same for minor axis */
	double delta_rect;  /* error in rectilinearity */
	int ndgf_major, ndgf_minor, ndgf_rect;  /* Degrees of freedom */
} Particle_Motion_Error;
	
/* This object is used to define a gather of MWtrace objects that would
normally be from the same basis wavelet and same band.  By convention
in this program problem data are left in the gather, but flagged by
setting the x1[i],x2[i],x3[i] points NULL.  This must always be 
checked in working through a gather. */
typedef struct MWgather_ {
	int nsta,ncomponents; 	/* number of stations and components 
				(components = 1 or 3) */
	MWstation **sta; 		/* array of station objects of length nsta
				(i.e. MWstation *sta[nsta]) */
	
	MWtrace **x1,**x2,**x3;  /*Three components in cartesian coordinates.*/
} MWgather;

/* This defines the analysis time window in a given band. It is 
used for both signal and noise analysis definitions */
typedef struct Time_Window {
	int tstart, tend;  /* window start and end position in samples
			relative to an arrival time */
	int length, stepsize, increment;  /*  stepsize is the window 
		translation step size used in time scans and increment 
		is the step between samples used inside a window.  Length
		is number of samples in an analysis window so the total
		analysis window is increment*length).*/
	double tpad;  /* time pad required in seconds.  Time padding
			is required for multiwavelets for decimation
			fir filters and wavelet functions themselves.*/
	double si;  /* Sample interval for this window (in sec) */
} Time_Window;
/* This structure holds signal to noise statistics for a given 
station in a particular band.  the ratio_? values are averages
from multiwavelets in the band, and the min and max define the 
range.  This can allow some flexibility in how s/n cutoffs are set */
typedef struct Signal_to_Noise_ {
	char sta[8];
	double ratio_z,ratio_n,ratio_e,ratio_3c;
	double min_ratio_z,max_ratio_z;
	double min_ratio_n,max_ratio_n;
	double min_ratio_e,max_ratio_e;
	double min_ratio_3c,max_ratio_3c;
	double nstime, netime;
	double sstime, setime;
} Signal_to_Noise;
typedef struct MW_scalar_statistics_ {
	double mean;
        double median;
        double q1_4,q3_4;
	double low,high;
} MW_scalar_statistics;

/* This stucture encapsulates all the static information computable 
through the multiwavelet methods.  We don't keep the station
name here because this entity is always stored as an associative
array keyed by station in MW code. */
typedef struct MWstatic_ {
	double dt_final;  /* latest time adjustment */
	double t_raw;  /*current static uncorrected for moveout */
	double log10amp;   /* relative amplitude */
	double sigma_t, sigma_log10amp;  /*Uncertainties in time and amplitude
			computed by multiwavelet analysis.  Note amp is log10*/  
	int ndgf;
} MWstatic;



/* These are used in db manipulations to do grouping by evid and sta */
#define EVIDBUNDLE 4
#define STABUNDLE 3
#define EVIDBDLNAME "evidbdl"
#define STABDLNAME "stabdl"
/* These are channel codes assigned by rotate_to_standard for cardinal
direction */
#define EW "E"
#define NS "N"
#define VERTICAL "Z"

/* These are used to vary type of coherence measure used to 
find optimal lag */
#define USE_SEMBLANCE 1
#define USE_COHERENCE 2

/* normal distribution interquartile conversion to standard deviation */
#define NORMAL_IQSCALE 0.741
/*This defines the way interquartile minimum scale factors passed to
statistical functions are handled.  m-estimators need a minimum 
scale factor to prevent downward spiral that throws out all data.  
IQ_SCALE_RELATIVE means to scale the value passed by the initial 
scale estimate (e.g. 0.1 in this mode means never all the error
scale factor to fall below 10% of the initial value.)  
IQ_SCALE_ABSOLUTE says just use the value verbatim */
#define IQ_SCALE_RELATIVE 0
#define IQ_SCALE_ABSOLUTE 1
/* function prototypes in multiwavelet library */
Tbl **define_decimation(Pf *pf, int *nbands);
int decimate_trace(Tbl *dectbl,float *in, int nin, double dt0, double t0,
		float **out, int *nout, double *dt, double *t0out);
int sconv(float *in, int nin, float *filter, int nfilter,
                int ioff, int decfac,
                float *out, int *nout);
Tbl **build_decimation_objects(Tbl **filelists, int nbands, int *decfac);
void free_decimation(FIR_decimation *d);
MWbasis *load_multiwavelets_pf(Pf *pf,int *nwavelets);
MWtrace **MWmatrix(int nrl, int nrh, int ncl,int nch);
void free_MWtrace_matrix(MWtrace **t,int nrl, int nrh, int ncl,int nch);
MWtrace **MWtransform(float *trace, double dt, double starttime, int nsamples,
		 MWbasis *basis, int nbasis, Tbl **decimators, int nbands);
void initialize_MWstation(MWstation *,int );
void free_MWstation(MWstation *);
Arr *create_station_objects(Pf *, int );
int load_surface_velocity(Pf *, Arr *);
char *get_refsta(Arr *);
int load_station_geometry(Dbptr , Arr *, double );
int load_initial_statics(Pf *, Arr *);
Arr *build_station_objects(Dbptr , Pf *, double );

/* function prototypes for mwap */
void print_band_info(MWbasis *,int *, Pf *);
void print_window_data(int *,int,Time_Window *,Time_Window *,Pf *);
double compute_moveout(MWstation *, MWstation *, MWSlowness_vector *);
double array_aperture(Arr *);
int *get_decimation_factors(Tbl **, Pf *);
int *compute_tpad(Tbl **, MWbasis *, Arr *, Pf *);
Time_Window *get_signal_windows(int *d, int *, Pf *);
Time_Window *get_noise_windows(int *, int *, Pf *);
Time_Window compute_time_window(Time_Window *, int *, int );
int is_S(char *);
int compute_plane_wave_static(MWstation *, MWstation *, TTPoint , char *, Pf *);
int set_pwstatics(Arr *, char *, char *, Dbptr, Pf *);
double compute_elevation_static(MWstation *,MWSlowness_vector , double, char *);
void initialize_residual_statics(Arr *);
double compute_time_reference(Arr *, Arr *, char *, MWSlowness_vector );
int estimate_slowness_vector(MWSlowness_vector, Arr *, Arr *, 
  char *, double, double, char *, int, MWSlowness_vector *);
int pseudo_inv_solver(double *, double *, double *, int, int, double *, double, double *);
int null_project(double *,int, int , double *, double *);
int compute_slowness_covariance(Arr *,Arr *,double, double *);
void mwap_process(Dbptr ,char *,  Pf *);
double unwrap_delta_phase(complex , complex );
char *make_mw_key(char *, char *);
Arr *tr_mwtransform(Dbptr , Arr *, Time_Window *, int *, Tbl **, int , MWbasis *, int );
MWgather *MWgather_alloc(int );
void free_MWgather(MWgather *);
int snr_is_too_low(Signal_to_Noise *,int, Pf *);
MWgather *build_MWgather(int , int , Arr *, Arr *, Arr *, Pf *);
MWtrace *MWtrace_dup(MWtrace *);
MWgather *MWgather_transformation(MWgather *,double *);
void free_sn_ratios_arr(Arr **,int);
Arr **compute_signal_to_noise(Arr *,Arr *,Arr *,Arr *,
	Time_Window *,Time_Window *, int , int);
int computer_total_moveout(MWgather *,Arr *, char *, MWSlowness_vector , double, char *, double *);
int *compute_lag_in_samples(MWgather *,double *, double );
double phase_to_time(double ,double , double );
int compute_optimal_lag(MWgather **,int ,double ,
        double *,Spherical_Coordinate, Time_Window *,int, int);
void compute_mw_arrival_times(MWgather **,int ,double ,double *, int , 
	Spherical_Coordinate, Time_Window *, Arr **, Arr **,
	double *,double *,int *);
void compute_mw_particle_motion(MWgather **,int ,double ,
        double *, int , Time_Window *, double *up, 
	Particle_Motion_Ellipse *, Particle_Motion_Error *,
	Arr **, Arr **);
MW_scalar_statistics MW_calc_statistics_float(float *,int );
MW_scalar_statistics MW_calc_statistics_double(double *,int );
float M_estimator_float(float *,int,int, double);
complex M_estimator_complex(complex *,int );
Dbptr mwap_readdata(Dbptr , Arr *, Time_Window , Time_Window );
int free_noncardinal_traces(Dbptr );
void check_required_pf(Pf *);
Spherical_Coordinate estimate_initial_polarization(MWSlowness_vector ,
                        Arr *, char *, char *);
void ray_coordinate_trans(Spherical_Coordinate ,double *);
void copy_polarization(Spherical_Coordinate*, Spherical_Coordinate*);
Particle_Motion_Ellipse compute_particle_motion(complex, complex, complex,
				double *);
Spherical_Coordinate unit_vector_to_spherical(double *);
void MWgather_to_trace(MWgather *,Dbptr, int, int, int *);
void MWtrace_gather_reset_stime(MWgather *,Dbptr ,double );
int MWtrace_mark_window(MWgather *,Dbptr,Time_Window *,int);
void MWtrace_put_semblance(Dbptr,float *,int,double,int,double,int,char*);
void trplot_by_sta(Dbptr,char *);
void trplot_one_mwtrace(MWtrace *,char *);

int MWdb_save_slowness_vector(char *, MWSlowness_vector *, double,
	Time_Window *,char *, int, int, double, double *,
	int, int, Dbptr);
int MWdb_save_avgamp(char *, int, int, char *, double, double,
	Time_Window *, double, double, int, Dbptr);
int MWdb_save_statics(int, int, char *, double, double, Time_Window *,
	double, MWgather *, double *, Arr *, Arr *, Arr *, Dbptr);
int MWdb_save_pm(char *, int, int, char *, double, double, 
	Time_Window *, MWgather *, double *, Arr *, Arr *, 
	Particle_Motion_Ellipse *, Particle_Motion_Error *, Dbptr);
MWbasis *load_multiwavelets_db(Pf *,int *, int *);
double Window_stime(Time_Window);
double Window_etime(Time_Window);
