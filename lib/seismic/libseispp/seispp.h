#ifndef _SEISPP_H_
#define _SEISPP_H_
#include <vector>
#include <set>
#ifdef sun
#include <sunmath.h>
#endif
// antelope routines
#include "db.h"
#include "tr.h"
#include "pf.h"
// glp routines
#include "metadata.h"
#include "dbpp.h"
#include "pfstream.h"
#include "dmatrix.h"
namespace SEISPP 
{
extern bool SEISPP_verbose;
using namespace std;
//
// We use an abstract base class and derived classes to allow variations
// in errors thrown.  This uses methods described in books by Stroustrup
//
class seispp_error
{
public:
	string message;
	seispp_error(){message="seispp library error\n";};
	seispp_error(const string mess){message=mess;};
	virtual void log_error(){cerr << "seispp error: "<<message<<endl;};
};

// error class for datascope db interface
// adds antelope elog dump and a severity option identical
// to antelope elog
enum error_severity {fault, fatal, complain, notify, log, unknown};
class seispp_dberror : public seispp_error
{
public:
        Dbptr db;
	error_severity error_type;
        seispp_dberror(const string mess,Dbptr dbi);
	seispp_dberror(const string mess, 
		Dbptr dbi, error_severity et);
	void log_error();
};

class SAC_data_error : public seispp_error
{
public:
	SAC_data_error(const string mess){message=mess;};
	void log_error()
	{
		cerr<<"Error processing SAC format time series"<<endl;
		cerr<<"Error message = "<<message;
	}
};

// Warning the following names collide with location.h
class Slowness_vector
{
public:
	double ux,uy;   // base vector stored as components in s/km units
	double mag(){return(hypot(ux,uy));};
	double azimuth(){
		double phi;
		phi=M_PI_2-atan2(uy,ux);
		if(phi>M_PI)
			return(phi-2.0*M_PI);
		else
			return(phi);
	};
	double baz(){
		double phi;
		phi = M_PI_2-atan2(-uy,-ux);
		if(phi>M_PI)
			return(phi-2.0*M_PI);
		else
			return(phi);
	};
};
class Rectangular_Slowness_Grid
{
public:
	string name;
	double uxlow, uylow;
	double dux, duy;
	int nux, nuy;
	Rectangular_Slowness_Grid(Pf *);
	double ux(int i) {return(uxlow+i*dux);};
	double uy(int i) {return(uylow+i*duy);};
};

// This is used to define gaps and/or time variable weights
// The later is not implemented, but we 
//
class Time_Window
{
public:
	double start, end;  // time period of window (relative or abs)
	Time_Window(){start=0.0;end=1.0e99;};
	Time_Window(double ts,double te){start=ts;end=te;};
};
class Time_Variable_Weight : public Time_Window
{
public:
	bool gap; // alternate way to set a gap
	Time_Variable_Weight(){w0=1.0;wgrad=0.0;};
	// inline method applies ramp between start and end with 
	// flat floor and ceiling if outside range start:end
	// Generalization of commonly used mute 
	double weight(double t)
	{
		if(gap) return(0.0);
		if(t<start) return(w0);
		if(t>end) return(w0+(end-start)*wgrad);
		return(w0+(t-start)*wgrad);
	}
private:
	double w0;  // weight at t0
	double wgrad;  // dw/dt 
};

/* This strange looking function is a C++ function object.
// It is used in the STL container called a set used for gaps below.  
// This function is used as the comparison function for ordering
// the elements of the set.  It makes Time_Windows indexed by
// intervals similar to thw way Datascope uses time:endtime
// Be aware, however, that for the same reason as datascope overlapping 
// time windows will cause ambiguity in indexing times by this
// method.
*/
class Time_Window_Cmp
{
public:
	bool operator()(const Time_Window ti1,const Time_Window ti2) const
	{return(ti1.end<ti2.start);};
};


enum Time_Reference_Type {absolute,relative};

// base class
// includes essential parameters for any time series including gap processing
class Basic_Time_Series
{
public:
	bool live;
	double dt,t0;
	int ns;
	Time_Reference_Type tref;
	bool is_gap(int);  // query by sample number
	bool is_gap(double);  // query by time
	void add_gap(Time_Window tw){gaps.insert(tw);};
	virtual void zero_gaps()=0; // pure virtual function
	// inline function to return time of sample i
	double time(int i){return(t0+dt*((double)i));};
	// inverse of time
	int sample_number(double t){return(nint((t-t0)/dt));};
protected:
	set<Time_Window,Time_Window_Cmp> gaps;
};
// scalar time series definition
class Time_Series: public Basic_Time_Series , public Metadata
{
public:
	vector<double>s;
	Time_Series();
	Time_Series(int nsin);
	// This constructor only reads data when load_data is true.  When
	// false the Metadata object portion is constructed and reserve is called
	Time_Series(Metadata& md,bool load_data);
	// Antelope database constructor 
	Time_Series(Database_Handle& db, Metadata_list& mdl, Attribute_Map& am);
	Time_Series(const Time_Series&);
	Time_Series& operator=(const Time_Series&);
	void zero_gaps();
};

// 
// Spherical coordinate angles are convenient for trace rotation
// definitions
//
typedef struct Spherical_Coordinate_{
        double radius;
	// in radians 
        double theta;
        double phi;
} Spherical_Coordinate;
/* A Three_Component_Seismogram is viewed as a special collection of Time Series 
type data that is essentially a special version of a vector time series. 
It is "special" as the vector is 3D with real components.  One could produce a
similar inherited type for an n vector time series object.  

The structure of a vector time series allows the data to be stored in a matrix
*/
//typedef matrix<double,rectangle<>,dense<>,row_major>::type Vector_Data; 
class Three_Component_Seismogram : public Basic_Time_Series , public Metadata
{
public:
	bool components_are_orthogonal;  
	bool components_are_cardinal;  // true if x1=e, x2=n, x3=up
	// This is the transformation matrix applied relative to standard
	double tmatrix[3][3]; 
	dmatrix u;
	Three_Component_Seismogram();
	Three_Component_Seismogram(int nsamples);
	// The next two are similar to comparable functions for Time_Series
	Three_Component_Seismogram(Metadata& md,bool load_data);
	Three_Component_Seismogram(Database_Handle& db, 
		Metadata_list& mdl, Attribute_Map& am);
	Three_Component_Seismogram(const Three_Component_Seismogram&);
	Three_Component_Seismogram& operator 
		= (const Three_Component_Seismogram&);
	// Default destructor is acceptable
	//~Three_Component_Seismogram(); 
	void zero_gaps();
	void rotate_to_standard() throw(seispp_error);
	// This overloaded pair do the same thing for a vector
	// specified as a unit vector nu or as spherical coordinate angles
	void rotate(Spherical_Coordinate);
	void rotate(double nu[3]);
	// This applies a general transform with a 3x3 matrix.  
	// User should set components_are_orthogonal true if they
	// are so after this transformation.  The default assumes not
	// Note this is routine does NOT return to standard before
	// applying the transformation so this is accumulative.
	void apply_transformation_matrix(double a[3][3]);
	void free_surface_transformation(Slowness_vector u, double vp0, double vs0);
};
// Note for ensembles the lengths of each trace (3ctrace) and
// should be allowed to be variable.  The number of elements in
// the ensemble can be obtained using the size() function for the STL
// vector container.
class Time_Series_Ensemble : public Metadata
{
public:  
	vector <Time_Series> tse;

	Time_Series_Ensemble();
	Time_Series_Ensemble(int ntsin, int nsampin);
	Time_Series_Ensemble(int ntsin, int nsampin, Metadata_list& mdl);
	Time_Series_Ensemble(Pf_ensemble *pfe);
	friend void set_global_metadata_list(Time_Series_Ensemble& te,Metadata_list&);
private:
	// This list contains metadata copied as global to the ensemble
	Metadata_list mdlist;
};

class Three_Component_Ensemble : public Metadata
{
public:
	vector <Three_Component_Seismogram> tcse;

	Three_Component_Ensemble();
	Three_Component_Ensemble(int nsta, int nsamp);
	Three_Component_Ensemble(int nsta, int nsamp,
				Metadata_list& mdl);
	friend void set_global_metadata_list(Three_Component_Ensemble&,
			Metadata_list&);
private:
	Metadata_list mdlist;
};
//
//  Mute definitions
//
class Top_Mute
{
public:
	double t0e, t1;  // t0e is end of to 0 zone, t1 time when weight goes to 1
	 Time_Reference_Type reftype;   // from seispp is enum as absolute or relative
	Top_Mute(){t0e=1.0; t1=2.0; reftype=relative;};
	Top_Mute(Pf *pf);
};

// An assignment operator is not necessary for this object as it
//stands since it is constructed of simple types.
class Hypocenter
{
public:
	double lat,lon,z;
	double time;
	Hypocenter(){method=strdup("tttaup"); model=strdup("iasp91");};  // default
	~Hypocenter(){free(method); free(model);};
	double distance(double lat0, double lon0);
	double esaz(double lat0, double lon0);
	double seaz(double lat0, double lon0);
	double ptime(double lat0, double lon0, double elev)
		throw(seispp_error);
	Slowness_vector pslow(double lat0, double lon0, double elev)
		throw(seispp_error);
	double phasetime(double lat0, double lon0, double elev, string phase)
		throw(seispp_error);
	Slowness_vector phaseslow(double lat0, double lon0, double elev, 
			string phase) throw(seispp_error);
	void tt_setup(string meth, string mod); // change default method:model
private:
	char *method;
	char *model;
};

class Velocity_Model_1d_error
{
public:
	string message;
	virtual void log_error(){cerr<<"Velocity_Model_1d object error"<<endl;};
};

class Velocity_Model_1d_dberror : public Velocity_Model_1d_error
{
public:
	string name;
	Velocity_Model_1d_dberror(string modname,string mess){
		name=modname;  message=mess;};
	virtual void log_error(){
		cerr<<"Database error accessing velocity model mod1d table "<<name<<endl;
		cerr<<message;
	};
};
class Velocity_Model_1d_ioerror : public Velocity_Model_1d_error
{
public:
	string ioerr;
	Velocity_Model_1d_ioerror(string mess, string ioe){
		message = mess; ioerr = ioe;};
	virtual void log_error(){
		cerr<<"Velocity i/o error" << endl
			<< message << endl
			<< ioerr << endl;
	};
};

class Velocity_Model_1d
{
public:
	int nlayers;
	double *z,*v,*grad;
	Velocity_Model_1d(){z=NULL;v=NULL;grad=NULL;nlayers=0;};
	Velocity_Model_1d(int n){nlayers=n;
		z=new double[nlayers]; 
		v=new double[nlayers];
		grad=new double[nlayers];};
	Velocity_Model_1d(Dbptr db,string name, string property)
		throw(Velocity_Model_1d_dberror);
	Velocity_Model_1d(string fname, string form, string property)
		throw(Velocity_Model_1d_ioerror);
	~Velocity_Model_1d()
	{if(z!=NULL)delete[]z; if(v!=NULL)delete[]v; if(grad!=NULL)delete[]grad;};
	double getv(double zin);
};

//
//Helpers
//
void apply_top_mute(Time_Series &ts,Top_Mute& mute);
void apply_top_mute(Three_Component_Seismogram& ts,Top_Mute& mute);
void apply_top_mute(Time_Series_Ensemble& t, Top_Mute& mute);
void apply_top_mute(Three_Component_Ensemble &t3c, Top_Mute& mute);
void apply_geometric_static(Time_Series *ts, double vel, double elev);
void apply_geometric_static(Time_Series *ts);
// The set of functions are essentially constructors but considering their
// use and level of specialization I decided they were better made helpers.
Time_Series* get_next_time_series(Pfstream_handle *pfh);
Time_Series *Load_Time_Series_Using_Pf(Pf *pf);
Time_Series_Ensemble *get_next_ensemble(Pfstream_handle *pfh,
	 char *tag,Metadata_list& mdlist) throw(seispp_error);
Three_Component_Ensemble *get_next_3c_ensemble(Pfstream_handle *pfh,
	 char *tag,Metadata_list& mdlist) throw(seispp_error);
// Inverse of above
void pfstream_save_3cseis(Three_Component_Seismogram *seis,string tag,
	string dir, string dfile, Pfstream_handle *pfh) throw(seispp_error);
// Used by input routines to set gaps in time series objects
void set_gaps(Time_Series&,Trsample *,int, string)
		throw(seispp_error);
// Spherical coordinate routines
Spherical_Coordinate unit_vector_to_spherical(double nu[3]);
double *spherical_to_unit_vector(Spherical_Coordinate&);
// converters between types of time series objects
Time_Series *Extract_Component(Three_Component_Seismogram& tcs,int component,
        Metadata_list& mdl);
Time_Series *Extract_Component(Three_Component_Seismogram& tcs,int component);


// low level i/o routines
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(seispp_error);
long int vector_fwrite(double *x,int n, string fname) throw(seispp_error);
long int vector_fwrite(float *x,int n, string dir, string dfile) throw(seispp_error);
long int vector_fwrite(float *x,int n, string fname) throw(seispp_error);
// Antelope database output routine
void dbsave(Time_Series& ts,Dbptr db,string table, Metadata_list& md, Attribute_Map& am)
		throw(seispp_error);
void dbsave(Three_Component_Seismogram& ts,Dbptr db,string table, 
	Metadata_list& md, Attribute_Map& am);
void dbsave(Three_Component_Seismogram& ts,Dbptr db,
	string table, Metadata_list& md, 
	Attribute_Map& am, vector<string>chanmap,bool output_as_standard);
}
#endif
