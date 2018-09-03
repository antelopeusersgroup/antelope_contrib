#ifndef _SEISPP_H_
#define _SEISPP_H_
// Seismic C++ Library (SEISPP).
// This is a collection of C++ objects used for processing seismic data.  
// Objects define some common seismological concepts including time series
// data, three component seismograms, ensembles of seismograms, velocity
// models, hypocenters, slowness vectors, and others.  
// The library has a strong dependence on Antelope in implementation, but
// the API design was intended to be more general.  
#ifdef sun
#include <sunmath.h>
#endif
#include <limits.h>
//
// These STL includes are needed for templates in this file
//
#include <vector>
#include <algorithm>
//
// Antelope includes required
//
#include "stock.h"
#ifndef NO_ANTELOPE
#include "db.h"
#include "tr.h"
#include "pf.h"
//
// These are glp stuff external to this library
// 
#include "pfstream.h"
#endif
//
// Library internal includes files required for the seispp.h  include
// 
#include "gclgrid.h"
#include "SeisppKeywords.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#ifndef NO_ANTELOPE
//#include "ComplexTimeSeries.h"
#include "seismicarray.h"
#endif
#include "ensemble.h"

namespace SEISPP 
{
using namespace SEISPP;
using namespace std;
/*!
Turns verbose mode on and off.  
**/
extern bool SEISPP_verbose;
/*! List of allowed attribute data types.

Binary data types are an essential component of all seismic data 
processing as binary data are used universally in external data formats.
This lists the allowed attributes that can be extracted from header
data.  It is also used in the interface to extract sample data from
raw external format inputs.*/
enum AttributeType {INT64, INT32, INT16, BYTE, REAL32, REAL64,
        STRING, BOOL, HDRINVALID};


/*! Apply a simple static time shift based on elevation.

Applies a time shift called a geometric static to BasicTimeSeries object ts 
using velocity vel and elevation elev.  This is a simple elev/vel correction.
Note this method can be applied to any data object that inherits 
BasicTimeSeries so this should be considered a generic method to do this
process.  It is very important to note, however, that it is the 
caller responsibility to do any bookeeping if it is important to 
keep track of the time shift applid by this procedure.
\param ts data to which the static should be applied.
\param vel surface velocity to use for static computation.
\param elev elevation of station.
**/
void ApplyGeometricStatic(BasicTimeSeries *ts, double vel, double elev);
/*!
// Applies a time shift called a geometric static to TimeSeries object ts 
// using velocity and elevation extracted from the metadata area of ts.  
// The function requires that attributes "elev" and "surface_velocity"
// be defined in the object.  If these attributes are not defined the 
// data are not altered but a diagnostic is issued to stderr.
**/
void ApplyGeometricStatic(TimeSeries *ts);
#ifndef NO_ANTELOPE
/*!
// Pfstream method for getting a time series object from an input stream.
// Useful only in a pfstream environment which is currently not well developed
// for this application.
**/
TimeSeries* GetNextTimeSeries(Pfstream_handle *pfh);
/*!
// Companion to GetNextTimeSeries.  
// Useful only in a pfstream environment which is currently not well developed
// for this application.
**/
TimeSeries *LoadTimeSeriesUsingPf(Pf *pf);
/*!
// Load an ensemble through a pfstream.
**/
TimeSeriesEnsemble *GetNextEnsemble(Pfstream_handle *pfh,
	 char *tag,MetadataList& mdlist) throw(SeisppError);
/*!
// Load a 3c ensemble through a pfstream.
**/
ThreeComponentEnsemble *GetNext3cEnsemble(Pfstream_handle *pfh,
	 char *tag,MetadataList& mdlist) throw(SeisppError);
/*!
// Save a 3c seismgram using a pfstream output.
**/
void PfstreamSave3cseis(ThreeComponentSeismogram *seis,string tag,
	string dir, string dfile, Pfstream_handle *pfh) throw(SeisppError);
/*!
//  Used by TimeSeries constructors to set data gaps using Antelope methods.
**/
void SetGaps(TimeSeries&,Trsample *,int, string)
		throw(SeisppError);
#endif
/*!
// Return direction of particle motion for a P wave with 
// slowness (ux,uy) at a surface with P velocity vp0 and 
// S velocity vs0.
**/
SphericalCoordinate PMHalfspaceModel(double vp0,double vs0,
	double ux,double uy);
/*!
// Returns a new seismogram in an arrival time reference.
// An arrival time reference means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// the object.  The key used to extract the arrival time used for the
// conversion is passed as a variable as this requires some flexibility.
// To preserve the absolute time standard in this conversion the 0 time
// computed from the arrival time field is used to compute the absolute
// time of the start of the output seismogram as atime+t0.  This result
// is stored in the metadata field keyed by the word "time".  This allows
// one to convert the data back to an absolute time standard if they so
// desire, but it is less flexible than the input key method.  
//
//\exception SeisppError for errors in extracting required information from metadata area.
//
//\param din  is input seismogram
//\param key is the metadata key used to find the arrival time to use as a reference.
//\param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
**/
shared_ptr<ThreeComponentSeismogram> ArrivalTimeReference(ThreeComponentSeismogram& din,
	string key, TimeWindow tw);
/*!
// Returns a gather of ThreeComponentSeismograms in an arrival time reference fram.
// An arrival time refernce means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// each member object.
//
//\exception SeisppError for errors in extracting required information from metadata area.
//
//\param din  is input gather
//\param key is the metadata key used to find the arrival time to use as a reference.
//\param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
**/
shared_ptr<ThreeComponentEnsemble> ArrivalTimeReference(ThreeComponentEnsemble& din,
	string key, TimeWindow tw);
/*!
// Returns a new TimeSeries seismogram in an arrival time reference.
// An arrival time reference means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// the object.  The key used to extract the arrival time used for the
// conversion is passed as a variable as this requires some flexibility.
// To preserve the absolute time standard in this conversion the 0 time
// computed from the arrival time field is used to compute the absolute
// time of the start of the output seismogram as atime+t0.  This result
// is stored in the metadata field keyed by the word "time".  This allows
// one to convert the data back to an absolute time standard if they so
// desire, but it is less flexible than the input key method.  
//
//\exception SeisppError for errors in extracting required information from metadata area.
//
//\param din  is input seismogram
//\param key is the metadata key used to find the arrival time to use as a reference.
//\param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
**/
shared_ptr<TimeSeries> ArrivalTimeReference(TimeSeries& din,
	string key, TimeWindow tw);
/*!
// Returns a gather of TimeSeries objects in an arrival time reference frame.
// An arrival time refernce means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// each member object.
//
//\exception SeisppError for errors in extracting required information from metadata area.
//
//\param din  is input gather
//\param key is the metadata key used to find the arrival time to use as a reference.
//\param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
**/
shared_ptr<TimeSeriesEnsemble> ArrivalTimeReference(TimeSeriesEnsemble& din,
	string key, TimeWindow tw);


/*!
// Bombproof low level write routine for a vector of doubles.  
// Uses fwrite to write vector x to the file dir+"/"+dfile.
//
//\exception SeisppError object if there are problems saving data to requested file.
//\param x vector of data to be saved.
//\param n length of vector x
//\param dir directory to place file.  If empty assumes current directory.
//\param dfile file name 
**/
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(SeisppError);
/*!
// Bombproof low level write routine for a vector of doubles.  
// Uses fwrite to write vector x to the file fname
//
//\exception SeisppError object if there are problems saving data to requested file.
//\param x vector of data to be saved.
//\param n length of vector x
//\param fname file name 
**/
long int vector_fwrite(double *x,int n, string fname) throw(SeisppError);
/*!
// Bombproof low level write routine for a vector of floats.  
// Uses fwrite to write vector x to the file dir+"/"+dfile.
//
//\exception SeisppError object if there are problems saving data to requested file.
//\param x vector of data to be saved.
//\param n length of vector x
//\param dir directory to place file.  If empty assumes current directory.
//\param dfile file name 
**/
long int vector_fwrite(float *x,int n, string dir, string dfile) throw(SeisppError);
/*!
// Bombproof low level write routine for a vector of floats.  
// Uses fwrite to write vector x to the file fname
//
//\exception SeisppError object if there are problems saving data to requested file.
//\param x vector of data to be saved.
//\param n length of vector x
//\param fname file name 
**/
long int vector_fwrite(float *x,int n, string fname) throw(SeisppError);
#ifndef NO_ANTELOPE
/*!
// Save the data in a TimeSeries object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A TimeSeries object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
// object am and pushed to an output record using the Datascope dbputv
// function one attribute at a time.  The data are saved to files
// whose name and location are driven by two (frozen) standard names
// extracted from the metadata area:  dir and dfile.  The filename
// for output is created as dir+"/"+dfile or simply dfile if dir
// is not defined (assumes current directory).  
//
// This function is dogmatic about four database output names.  
// It always translates it's internal t0 to be a "time" database
// attribute,  the ns variable is saved as "nsamp", the sample
// interval (dt) is always converted to 1/dt and called "samprate",
// and an endtime (computed as this->endtime()) is computed and
// saved as the database attribute "endtime".   These are the css3.0
// name conventions and I chose to leave them as frozen names.  
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
//\exception SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
//\return -1 if live is false, record number of added row otherwise
//
//\param ts is the TimeSeries object to be saved.
//\param db is a Datascope database pointer.  It need only point at a valid
//   open database.
//\param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
//\param md  is the list of metadata to be dumped to the database as described above.
//\param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
**/
long dbsave(TimeSeries& ts,Dbptr db,string table, MetadataList& md, AttributeMap& am)
		throw(SeisppError);
/*!
// Save the data in a ThreeComponentSeismogram object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A ThreeComponentSeismogram object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
// object am and pushed to an output record using the Datascope dbputv
// function one attribute at a time.  The data are saved to files
// whose name and location are driven by two (frozen) standard names
// extracted from the metadata area:  dir and dfile.  The filename
// for output is created as dir+"/"+dfile or simply dfile if dir
// is not defined (assumes current directory).  
//
// This function is dogmatic about four database output names.  
// It always translates it's internal t0 to be a "time" database
// attribute,  the ns variable is saved as "nsamp", the sample
// interval (dt) is always converted to 1/dt and called "samprate",
// and an endtime (computed as this->endtime()) is computed and
// saved as the database attribute "endtime".   These are the css3.0
// name conventions and I chose to leave them as frozen names.  
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
// This function differs in a significant way from an overloaded function
// with the same name.  The other has a "chanmap" argument to tell the 
// function how to split up the 3 components into channel codes.  This
// function takes a very different approach and saves data by dumping
// the internal 3xns matrix as the basic output data series. As a result
// this function will write ONE AND ONLY ONE DATABASE ROW PER OBJECT.
// This means somewhat by definition that the output table CANNOT be
// wfdisc if this function is called.  Consequently, this routine will
// throw an exception and do nothing if table!="wfprocess".
//
//\exception SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
//\return -1 if live is false, record number of added row otherwise
//
//\param ts is the TimeSeries object to be saved.
//\param db is a Datascope database pointer.  It need only point at a valid
//    open database.
//\param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
//\param md  is the list of metadata to be dumped to the database as described above.
//\param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
**/
long dbsave(ThreeComponentSeismogram& ts,Dbptr db,string table, 
	MetadataList& md, AttributeMap& am);
/*! \brief Save the data in a ThreeComponentSeismogram object 
  to a database with orientation data.

  This procedure is identical to the dbsave procedure with the 
  same argument signature.  The difference is that the data are
  save directly without an automatic reorientation to cardinal 
  directions used in that procedure.  This procedure requires
  an extension table called tmatrix to save the orientation data.
  This procedure actually calls the plain dbsave routine in
  a mode where the automatic reorientation is disabled.
  Details described there apply here too.

\exception SeisppError object if there are any problems saving the data or 
    writing attributes into the database.

\return -1 if live is false, record number of added row otherwise

\param ts is the ThreeComponentSeismogram object to be saved.
\param db is a Datascope database pointer.  It need only point at a valid
    open database.
\param table is the name of the table to index this time series data
   (e.g. "wfdisc").
 \param md  is the list of metadata to be dumped to the database as described above.
 \param am is a mapping operator that defines how internal names are to be mapped
    to database attribute names and tables.  
\param chanmap is a set of channel names to map each component to channel code (see above)
**/
long dbsave_oriented(ThreeComponentSeismogram& ts,Dbptr db,
	string table, MetadataList& md, 
	AttributeMap& am);
/*!
// Save the data in a ThreeComponentSeismogram object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A ThreeComponentSeismogram object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
// object am and pushed to an output record using the Datascope dbputv
// function one attribute at a time.  The data are saved to files
// whose name and location are driven by two (frozen) standard names
// extracted from the metadata area:  dir and dfile.  The filename
// for output is created as dir+"/"+dfile or simply dfile if dir
// is not defined (assumes current directory).  
//
// This function is dogmatic about four database output names.  
// It always translates it's internal t0 to be a "time" database
// attribute,  the ns variable is saved as "nsamp", the sample
// interval (dt) is always converted to 1/dt and called "samprate",
// and an endtime (computed as this->endtime()) is computed and
// saved as the database attribute "endtime".   These are the css3.0
// name conventions and I chose to leave them as frozen names.  
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
// The chanmap and output_to_standard variables control how the 
// data are saved externally.  If output_as_standard is set true
// (highly recommended in general)  the data are restored (if necessary)
// to standard 3c data geometry (ew,ns,z) before being written to 
// output.  In that case vang and hang are set accordingly in 
// case the output algorithm requires these to be stored.  
// The components are then extraced from the 3c object one by 
// one and written in three successive database rows with the
// channel code ("chan" attribute in css3.0) derived from the
// chanmap array (chanmap[0]=channel name for component 0,
// chanmap[1]=component 1, and chanmap[2]=component 2).
//
//\exception SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
//\return -1 if live is false, record number of added row otherwise
//
//\param ts is the TimeSeries object to be saved.
//\param db is a Datascope database pointer.  It need only point at a valid
//    open database.
//\param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
// \param md  is the list of metadata to be dumped to the database as described above.
// \param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
//\param chanmap is a set of channel names to map each component to channel code (see above)
//\param output_as_standard when true forces data to be converted to ew,ns, z system
**/
long dbsave(ThreeComponentSeismogram& ts,Dbptr db,
	string table, MetadataList& md, 
	AttributeMap& am, vector<string>chanmap,bool output_as_standard);
/*!
// Save the data in a ComplexTimeSeries object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema and database independdent.  
// It does this by assuming each object will generate one row in 
// some table.  What is written to the row that is associated with
// this object is assumed to be present in the Metadata area of the
// object.  The attributes to be written are controlled by the
// contents of the MetadataList passed as an argument.  
// The internal names in the MetadataList are translated 
// translated to the database namespace using the AttributeMap
// object am and pushed to an output record using the Datascope dbputv
// function one attribute at a time.  The data are saved to files
// whose name and location are driven by two (frozen) standard names
// extracted from the metadata area:  dir and dfile.  The filename
// for output is created as dir+"/"+dfile or simply dfile if dir
// is not defined (assumes current directory).  
//
// This function is dogmatic about four database output names.  
// It always translates it's internal t0 to be a "time" database
// attribute,  the ns variable is saved as "nsamp", the sample
// interval (dt) is always converted to 1/dt and called "samprate",
// and an endtime (computed as this->endtime()) is computed and
// saved as the database attribute "endtime".   These are the css3.0
// name conventions and I chose to leave them as frozen names.  
// The current implemenation ALWAYS saves the result as a 
// host-specific (i.e. not portable across platform) 
// vector of double complex values (real,imag).  i.e. real and 
// imaginary parts are multiplexed and stored externally as 
// doubles.  This will NOT work if data like this are passed 
// between different hosts with incompatible binary double
// formats (infamous endian problem).
// The "datatype" is ALWAYS saved and is ALWAYS set to cx.  
// Finally note that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
//\exception SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
//\return -1 if live is false, record number of added row otherwise
//
//\param ts is the ComplexTimeSeries object to be saved.
//\param db is a Datascope database pointer.  It need only point at a valid
//    open database.
//\param table is the name of the table to index this time series data
//   (generally wfprocess for this procedure)
//\param md  is the list of metadata to be dumped to the database as described above.
//\param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
**/
/*
long dbsave(ComplexTimeSeries& ts,Dbptr db,
	string table, MetadataList& md, 
	AttributeMap& am);
        */

#endif

/*!
// Extracts a requested time window of data from a parent TimeSeries object.
//
// It is common to need to extract a smaller segment of data from a larger
// time window of data.  This function accomplishes this in a nifty method that
// takes advantage of the methods contained in the BasicTimeSeries object for
// handling time and data gaps.
//
//\return new TimeSeries object derived from  parent but windowed by input
//      time window range.
//
//\exception SeisppError object if the requested time window does not overlap data
//
//\param parent is the larger TimeSeries object to be windowed
//\param tw defines the data range to be extracted from parent.
//\author Gary L. Pavlis
**/
TimeSeries WindowData(TimeSeries& parent, TimeWindow& tw);

/*!
// Extracts a requested time window of data from a parent ThreeComponentSeismogram object.
//
// It is common to need to extract a smaller segment of data from a larger
// time window of data.  This function accomplishes this in a nifty method that
// takes advantage of the methods contained in the BasicTimeSeries object for
// handling time and data gaps.
//
//\return new ThreeComponentSeismogram object derived from  parent but windowed by input
//      time window range.
//
//\exception SeisppError object if the requested time window does not overlap data
//
//\param parent is the larger ThreeComponentSeismogram object to be windowed
//\param tw defines the data range to be extracted from parent.
//\author Gary L. Pavlis
**/
ThreeComponentSeismogram WindowData(ThreeComponentSeismogram& parent, TimeWindow& tw);

/*! Extract a specified time window from an ensemble.
// The seispp library defines a fairly generic ensemble object that
// uses an STL vector container to hold an array of objects 
// (currently TimeSeries or ThreeComponentSeismogram objects) with the
// generic symbol "member".  This template applies a comparable 
// WindowData function to each member of the ensemble returning a
// new ensemble cut to the specified window.
//
//\exception SeisppError exception if TimeWindow is not consistent
// with input data.
//
//\param  parent input ensemble 
//\param tw TimeWindow to cut parent to produce output.
//
//\return new ensemble T as an shared_ptr cut to desired window.
**/
//template <class T> shared_ptr<T>WindowData(T& parent, TimeWindow& tw);

/*!
// Sorts an ensemble by station:channel.  
// In earthquake seismic data processing sorting data by station name
// and channel code is a very common operation.  This procedure implements
// this using the STL sort algorithm.  It depends upon the metadata
// fields keyed by "sta" and "chan" being defined in each member of the 
// input ensemble.
//
//\param ensemble is the input data to be sorted.  The STL algorithm
// sorts this in place so the result is altered.  Any indices using
// subscripts will no longer be valid on exit.
**/
void StaChanSort(TimeSeriesEnsemble& ensemble);
/*! \brief Builds a general station:channel subset ensemble.

 Builds a new ensemble of members that satisfy unix regular expression
 for sta and chan attributes passed as sta_expr and chan_expr.

 \param parent original ensemble to be subsetted
 \param sta_expr unix regular expression to apply to sta Metadata
    attribute
 \param chan_expr unix regular expression to apply to chan Metadata 
    attribute

\author Gary L. Pavlis
**/
shared_ptr<TimeSeriesEnsemble> StaChanRegExSubset(TimeSeriesEnsemble& parent,
        string sta_expr, string chan_expr);
#ifndef NO_ANTELOPE
/*! 
\brief Return a subset of an ensemble that match a specified array.

It is sometimes useful to reduce an ensemble of data to only those
matching a particular receiver geometry.  This is done here by comparing
station names in the ensemble with those found in a pattern SeismicArray
object.  Algorithm is a selective copy from parent to output.  
The Metadata for the output ensemble will be a duplicate of the parent.

Note the algorithm used is linear in the ensemble.  That is the station
name is extracted for each data member and compared to the contents of
the SeismicArray object.  Members not in the array are ignored and those
that match are copied to the output.

\param parent is the input data ensemble.
\param array contains the stations defining the array that will be used
	to form the output ensemble.  

\return shared_ptr<TimeSeriesEnsemble> containing data subset.
	Note be warned that the function may return an empty 
	ensemble if the parent does not have the sta attribute defined.
	It will not be silent about this, however, as it will blast 
	numerous messages to stderr if this happens.
**/
shared_ptr<TimeSeriesEnsemble> ArraySubset(TimeSeriesEnsemble& parent,
		SeismicArray& array);
#endif
/*! Sparse convolution routine.

  Sometimes a time series is made up initially of only a relatively 
  small number of impulses.  A case in point is some simple synthetic
  In that case, standard convolution methods are unnecessarily slow.
  This specialized function can sometimes be useful in such a context.

  \param wavelet is assumed to be the nonsparse wavelet that will
    be replicated with appropriate lags for each impulse in d
  \param d is the sparse ThreeComponentSeismogram object that to which
    the wavelet function is to be convolved.  The contents of this 
    object are assumed to be mostly zeros or the algorithm is not 
    very efficient. It will work for data that is not sparse, but it will
    be slow compared to convolution by Fourier transforms.
  \return ThreeComponentSeismogram object that is the convolution of
    wavelet with d.  Result will have more samples than d by 2 times
    the length of wavelet 
*/

ThreeComponentSeismogram sparse_convolve(TimeSeries& wavelet,
        ThreeComponentSeismogram& d);

/*! Generic routine to compute a median.
// This template can be used to compute the median of a vector
// of objects for any class which has the default comparison
// operator needed by STL sort.  It is most likely to be used
// for standard numerical types (int, float, double) where it
// is guaranteed to work.  If T is more exotic, you need to understand
// the rules of what sort expects.  
//
//\param x - input STL vector of data to be compute median.
//   x is not altered.  We make a copy of this input and sort it.
//   Not the best algorithm if the sorted output is desired
**/
template <class T> T median(vector<T>& x)
{
	int count=x.size();
	if(count<=1)return(x[0]);
	int medposition=count/2;
	T result;
	vector<T> copyx(x);
	sort(copyx.begin(),copyx.end());
	if(count%2)
		result=copyx[medposition];
	else
		result=(copyx[medposition]+copyx[medposition-1])/2.0;
	return (result);
}
/*!
// Aligns an ensemble of data by moveout.  
//
// A common theme in multichannel processing is alignment of data
// by a static shift.  That is, the entire trace is shifted by a fixed
// time amount.  This generic method does this for ensembles with a
// vector of objects held in a variable called "member".  It uses
// a special keyword to extract a "moveout" value from the data objects
// metadata and applies this to forever shift the 0 time reference for
// that data object.  For this reason it always returns a copy of the
// original data.
//
//\param d - input data ensemble.
//
//\return copy of ensemble but with t0 values modified by moveout.
**/
template <class Tensemble> Tensemble MoveoutTimeShift(Tensemble& d)
{
	Tensemble dshift(d);
	try {
		for(int i=0;i<d.member.size();++i)
		{
			double tshift;
			if(d.member[i].live)
			{
				tshift=d.member[i].get_double(moveout_keyword);
				dshift.member[i].t0-=tshift;
			}
		}
	} catch (MetadataGetError mde)
	{
		throw SeisppError(string("MoveoutTimeShift:  moveout keyword")
			+ moveout_keyword
			+string(" not defined for one or members of input ensemble") );
	}
	return (dshift);
}
/*!
\brief Align an ensemble by a set of lag estimates.

We often want to shift the time base for a set of data in relative
time coordinates.  An example is application of statics of any kind.
Another is display of aligned traces after cross-correlation.  
This function handles this for any of the family of time series
ensemble objects.  It requires the input be in a relative time
reference frame to allow the application of the BasicTimeSeries
methods to deal with data gaps.  The basic algorithm is to 
get the original t0 time stamp (could be anything really, but 
the trefkeyword Metadata field must be set or an error will
follow), restore the data to an absolute time frame, shift
the original t0 shift value by lag extracted from the object
with the lagkey field, and then return the data to relative
time scale using the modified t0.  This algorithm retains the
integrity of the original absolute time stamp so the data are
altered in place.  This is in contrast to a similar template
function MoveoutTimeShift which is destructive.

On the flip side, the attribute defined by trefkeyword is incremented
by the extracted lag value.  This keeps this key attribute with the
0 time reference.  This is a design choice which causes the trefkeyword
value to be retained as a true time reference.

\param d input data ensemble of generic time series objects.
\param lagkey keyword used to extract the lag values from the 
	Metadata (generalized header) field of the ensemble members.
\param trefkeyword keyword used to extract the absolute time 0 stamp
	for each member of the ensemble.  This field is not altered 
	in the Metadata but is required to redefine t0 with an 
	applied lag value.
*/

template <class Tensemble> void LagShift(Tensemble& d,
	const string lagkey,
		const string trefkeyword)
{
	string error1("LagShift:  Input ensemble contains data with UTC time.  Must be relative.");
	string error2("LagShift:  Metadata error.  See previous error  message.");
	try {
		for(int i=0;i<d.member.size();++i)
		{
			double tshift,lag;
			if(d.member[i].live)
			{
				if(d.member[i].tref==absolute)
					throw SeisppError(error1);
				tshift=d.member[i].get_double(trefkeyword);
				lag=d.member[i].get_double(lagkey);
				// We do nothing to traces with moveout 
				// marked as bad by this test
				if(lag<MoveoutBadTest)
				{
					d.member[i].rtoa(tshift);
					tshift+=lag;
					d.member[i].ator(tshift);
					d.member[i].put(trefkeyword,tshift);
				}
			}
		}
	} catch (MetadataGetError mde)
	{
		mde.log_error();
		throw SeisppError(error2);
	}
}
template <class Tensemble> Tensemble remove_dead(Tensemble& d)
{
    Tensemble dedit(dynamic_cast<Metadata&>(d),1);
    int nd=d.member.size();
    int i;
    for(i=0;i<nd;++i)
    {
        if(d.member[i].live)
            dedit.member.push_back(d.member[i]);
    }
    return dedit;
}
/*! \brief Test a generic time series object for sample rate match with standard.

With real data there is often an issue about the actual sample rate of data
versus the nominal sample rate.  e.g. in the resample procedures in the SEISPP
library it is necessary to ask this question as a trigger to know if data need
to be resample or not. The question is not just common, but prone to variations
because what is considered good enough is context dependent.  This template
regularizes this test for any time series object (child of BasicTimeSeries).
A default is used to allow for a fairly common standard so most applications
can drop the third argument for this litle procedure.  

The actual test is a simple fractional difference formula like percentage
error but not converted to percent (e.g. 10% here is 0.1).

\param seis generic time series whose sample interval is to be tested against standard.
\param target_dt standard sample interval for test
\param tolerance relative error tolerance (default 0.001)
*/
template <class T> bool SampleIntervalsMatch(T& seis, double target_dt,
		double tolerance=0.001)
{
	double normalized_error=fabs( (seis.dt-target_dt)/target_dt);
	if(normalized_error<tolerance)
		return true;
	else
		return false;
}

/*!
// Convert a velocity model in flattened earth geometry to true geometry.
//
// Multiple programs exist in seismology that use a flattening transformation
// to convert spherical geometry into a regular grid.  The main reason
// for this in all examples I know of is to allow standard finite difference
// algorithms to used.  This is essential since standard finite differences
// demand regular grid geometries and the flattening transformation provides
// a convenient way to turn a spherical shell problem into a standard 
// finite different grid.  A GCLgrid3d object is aimed at getting the
// true geometry correct, however, so using velocity models defined on 
// a flattened coordinate system requires a conversion.  This function
// does this for a complete grid of points.  This algorithm assumes 
// a 3D model has been loaded into a GCLgrid3d object, BUT the coordinates
// and velocities stored in the grid have not been corrected by the
// inverse flattening transformation.  That is, the assumption is that
// the original model was defined in on flattened coordinate system but
// the points were converted to lat,lon, and (flattened) depth in building
// the input GCLscalarfield3d object.  The algorithm used here then just
// passes through the grid doing two things:  (1) the depth() method is 
// called for the parent grid and the uflatz (see gclgrid.h) function is
// called to convert this to true depth; and (2) the velocity value at each
// point is altered by in inverse flattening transformation.  Thus the 
// net effect is a depth dependent change in velocity values and a nonlinear
// distortion of the depth of each point in the grid.  
//
//\param vmodel is the parent grid.  It is altered in place to convert
//  flattened to true geometry as described above.
//
//\author Gary L. Pavlis
**/
void ConvertFlatModelToSpherical(GCLscalarfield3d& vmodel);
		
// for linux only
int nint(double);
/*! \brief Test for little endian condition.

To handle mixed processors it is essential to know if the
word structure of this machine you are on is little or big 
endian.  Intel processors are dominate today and are little
endian while Sun machines, which are commonly used in geophysics,
are big endian.  Because it is common today to mix these platforms
on the same network a way to detect which byte order the current
machine is, is necessary.

\return true if this processor is little endian (Intel byte order).
	Conversely returns false if the processor is big endian.
\author  Gary L. Pavlis with the core algorithm stolen from the
	University of Texas supercomputer web site.
*/
bool IntelByteOrder();
/*! \brief Architecture indedependent procedure 
to byte swap a vector of doubles.

In the seispp library most data are stored internally as doubles.
External data representations, however, are subject to byte order
issues.  This routine will take a vector of doubles and automatically
swap bytes using a method appropriate for the parent architecture.
It should always be preceded by logic to decide if byte swapping
is necessary as this will always swap bytes one way or the other.

\param x pointer to array of doubles to be byte swapped.
\param nx number of elements in x.  This is quietly assumed
	to be correct and not bounds checking is done by this procedure.
*/

void swapdvec(double *x,int nx);
}
#endif
