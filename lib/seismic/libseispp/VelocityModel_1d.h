#ifndef _VELOCITYMODEL_1D_H_
#define _VELOCITYMODEL_1D_H_
#include <string>
#include <vector>
#include "db.h"
namespace SEISPP {
using namespace std;
/*! \brief Error object thrown by 1d velocity model objects.

\author Gary L. Pavlis
**/
class VelocityModel_1d_Error
{
public:
	//* Contains error message. */
	string message;
	//* Dump error message to stderr. */
	virtual void log_error(){cerr<<"VelocityModel_1d object error"<<endl;};
};

/*! \brief Error object thrown by 1d velocity model objects.

This version is thrown by database driven constructor.
**/
class VelocityModel_1d_Dberror : public VelocityModel_1d_Error
{
public:
	//* Holds name of velocity model that generated error.*/
	string name;
	//* Basic constructor.*/
	VelocityModel_1d_Dberror(string modname,string mess){
		name=modname;  message=mess;};
	//* Dump error message to stderr. */
	virtual void log_error(){
		cerr<<"Database error accessing velocity model mod1d table "<<name<<endl;
		cerr<<message;
	};
};
/*! \brief Error object thrown by 1d velocity model object for i/o error.

\author Gary L. Pavlis
**/
class VelocityModel_1d_IOerror : public VelocityModel_1d_Error
{
public:
	//* Holds i/o error message.*/
	string ioerr;
	//* Basic constructor */
	VelocityModel_1d_IOerror(string mess, string ioe){
		message = mess; ioerr = ioe;};
	//* Dump error message to stderr.*/
	virtual void log_error(){
		cerr<<"Velocity i/o error" << endl
			<< message << endl
			<< ioerr << endl;
	};
};

/*! \brief Object to encapsulate concept of a 1d (layered Earth) velocity model.

// Many seismology algorithms utilized layered earth velocity models. 
// This object provides methods to ask for the velocity at any given depth
// without concerns about anything about how the model is stored.  
// Thus it can hold constant velocity layers or continuous models 
// specified on irregular depth grids all through the same interface.
**/
class VelocityModel_1d
{
public:
	//* Number of points used to define the model.*/
	int nlayers;
	/*!
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the depths of each point that specifies the model.
	**/
	vector<double> z;
	/*!
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the velocity of each point that specifies the model.
	**/
	vector<double> v;
	/*!
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the gradient from point i to point i+1.
	// For last point it is the gradient to use to extrapolate downward
	// below the depth of the last point.
	**/
	vector<double> grad;
	//* Default constructor.  Initializes all to zero.*/
	VelocityModel_1d(){z.reserve(0);v.reserve(0);grad.reserve(0);nlayers=0;};
	/*!
	// Allocating constructor.  Sets aside space for n layers, but leaves
	// contents empty.  
	**/
	VelocityModel_1d(int n){nlayers=n;
                z.reserve(nlayers);
                v.reserve(nlayers);
                grad.reserve(nlayers);};
	/*!
	// Database driven constructor.
	// Builds a velocity model from database tables.
	// Depends on a models database as used in tt1dcvl travel time calculator.
	//
	//\exception SeisppError if there are problems constructing the object
	//    from the database.
	//
	//\param db Datascope database pointer.  
	//\param name unique name to identify requested Earth model.
	//\param property defines property attribute of models database table.  
	//    Normall Pvelocity or Svelocity.
	**/
	
	VelocityModel_1d(Dbptr db,string name, string property)
		throw(VelocityModel_1d_Dberror);
	/*!
	// Ascii file constructor.  
	// Reads a velocity model from file fname using a simple ascii format
	// file.  Data are assumed in free format lines of the form
	// (z,vp,vs) where z is depth, vp is P velocity at z, and vs is
	// S velocity at z.  
	//
	//\exception SeisppError if is an i/o problem of any kind.
	//
	//\param fname is file name to be read.
	//\param form is either rbh or plain.  Anything else will cause
	//   an exception to be thrown.  plain is just a set of lines as 
	//   described above.  rbh format is from Bob Herrmann's velocity
	//   model format.  The main difference is that his format has 7 
	//   header lines before the velocity model parameters begin.
	//\param property name of property to load for this model.
	//   Currently this is either "P" or "S" for P and S wave 
	//   velocities.  Anthing else will lead to an exception being
	//   thrown.
	**/
	VelocityModel_1d(string fname, string form, string property)
		throw(VelocityModel_1d_IOerror);
	/* Standard copy constructor. */
	VelocityModel_1d(const VelocityModel_1d& old);
	/* Standard assignment operator. */
	VelocityModel_1d& operator=(const VelocityModel_1d& old);
	/*!
	// Return interpolated velocity at depth zin.
	// If z is above the first point the first point velocity is returned.
	// If z is below the last point the value is computed from the last
	// point velocity and the last point gradient.
	**/
	double getv(double zin);
};

} // End SEISPP namespace declaration
#endif
