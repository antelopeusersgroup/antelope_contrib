#ifndef _ENSEMBLE_H_
#define _ENSEMBLE_H_
/* This include file violates the normal rule of good style recommended
 * for files in OOP languages where each object type should have it's own 
 * include.  I intentionally violate this here because there are multiple
 * types of useful data seismic data "ensembles".  The code is so parallel
 * between them that I felt it preferable to emphasize this by keeping 
 * it together. At this writing the ensembles are either TimeSeries or
 * ThreeComponentSeismogram object, but the concept is general enough 
 * that a member vector container could hold a lot of other things like
 * complex valued time series, multiwavelet transformed data, or 
 * spectra.  
 */

#include <memory>
#include <vector>
#include "coords.h"
#include "perf.h"

#ifndef NO_ANTELOPE
#include "pfstream.h"
#endif
#include "TimeWindow.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#ifndef NO_ANTELOPE
#include "ComplexTimeSeries.h"
#endif
#include "Hypocenter.h"
#include "SeisppKeywords.h"
#ifdef NO_ANTELOPE
using namespace PWMIG;
#endif
namespace SEISPP {
/*! \brief Object to contain a group (ensemble) of time series objects (seismograms).

// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// time series data using an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//\author Gary L. Pavlis
**/
class TimeSeriesEnsemble : public Metadata
{
public:  
/*!
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
**/
	vector <TimeSeries> member;

/*!
// Default constructor.  Does little, but is not defaulted.  
**/
	TimeSeriesEnsemble();
/*!
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
**/
	TimeSeriesEnsemble(int ntsin, int nsampin);
#ifndef NO_ANTELOPE
/*!
// Database-driven constructor.  
// Seismic data are often stored today using a database to index the raw
// waveforms.  The database contains attributes loaded here as metadata.
// Attributes that are global to the ensemble are loaded driven by the
// ensemble_mdl list while single 3c seismogram metadata is controlled
// by station_mdl.  Note in both cases the list of names is the internal
// name not external database names.  The AttributeMap object 
// defines the mapping from the internal name space to database
// names.
//
// This constructor assumes the ensemble is defined by a grouping.
// The database interface must provide a mechanism equivalent to 
// the "group by" clause in SQL.  The concept of this constructor 
// is that the ensemble is defined by whatever that group by definition
// defines.  This constructor ONLY works with segmented data.  
// Use the TimeWindow driven constructor on continuous data.
//
//\exception SeisppError object if one of several unrecoverable errors
//   occur.  These are of two main type.  First, if any of the 
//   require booleans are true and not satisfied (e.g. site table
//   is missing when require_coords is true) the constructor will
//   abort by throwing an error.  Second, if any of the Antelope
//   routines that underly this constructor fail the Antelope elog
//   error dump routine is called and the constructor will throw an
//   exception.
//
//\param dbhi is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//   into each seismogram's metadata area.  
//\param data_mdl is a list of metadata attributes to be loaded 
//\param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensemble.
//\param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
**/
	TimeSeriesEnsemble(DatabaseHandle& dbhi,
	        MetadataList& data_mdl,
	        	MetadataList& ensemble_mdl,
	                        AttributeMap& am);
/*!
// Time-window based database-driven constructor.  
// Seismic data today are often stored as continous data indexed by
// a CSS3.0 database.  These data are most rationally accessed by
// the concept of a station-channel view over a specified time window.
// That is, we commonly as for a subset (sometimes all) channels that
// were on between time start and time end.  This constructor builds
// a TimeSeriesEnsemble object containing all stations and all channels
// that were recording in a specified time.  Optionally one can specify
// a station or channel subset expression.  
//
// The current implementation is completely locked to the CSS3.0
// schema AND the Antelope Trace4.0 library.  This constructor
// should be thought of as more-or-less of an interface routine
// to use the Antelope trload_css procedure.  The namespace of
// Metadata loaded into the individual traces is completely frozen
// as the list of attributes found in the trace table used in
// Trace4.0.  The segmented data constructor found in this object
// is more appropriate if more general Metadata are needed in
// a processing system.  This method should be thought of as a
// handle into a raw data archive or raw data tied to an 
// Antelope orb.  
//
// The attributes loaded with each trace and the ensemble are totally
// frozen in this constructor.  If other attributes are required they
// MUST be loaded some other way.  The attributes loaded also depend
// on the settings of the three require boolean arguments.  
// Base attributes loaded are:  sta, chan, time, endtime, nsamp,
// samprate, calib, datatype, and segtype.  If require_coords is
// true the following are loaded:  sta_lat, sta_lon, sta_elev, dnorth,
// deast, and refsta.  If require_orientation is true the hang and
// vang attributes are loaded.  Note all of these names are shorthand
// internal names defined in internal namespace for the SEISPP library.
// The relation to database names is defined in the seispp_attributes_map.pf
// parameter file.
//
// Note no ensemble attributes are loaded by this constructor.
// That is, an ensemble can have attributes loaded that are global for 
// the entire ensemble.  This constructor loads none in that area.
//
//\exception SeisppError object if one of several unrecoverable errors
//   occur.  These are of three main type.  First, if any of the 
//   require booleans are true and not satisfied (e.g. site table
//   is missing when require_coords is true) the constructor will
//   abort by throwing an error.  Second, if any of the Antelope
//   routines that underly this constructor fail the Antelope elog
//   error dump routine is called and the constructor will throw an
//   exception.  Finally, SeisppError objects can be propagated from 
//   seispp routines called internally by this constructor.
//
//\param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//\param twin defines the time range of data to load.
//\param sta_expression regular expression to sift station codes 
//   (e.g. A.* for all stations starting with A or ..2 for all 3 station
//   codes ending in 2.)
//\param chan_expression regular expression to sift channel codes 
//   (e.g. BH. for all BH channels or .* for all).
//\param require_coords require that a site table be present.  (default true)
//\param require_orientation require that sitechan orientation information
//   (hang, vang) be present. (default true)
//\param require_response require the set of css3.0 response tables be 
//   present.   (Note that response data is currently ignored.  This is
//   a placeholder.)  default is false
**/
	TimeSeriesEnsemble(DatabaseHandle& db,
		TimeWindow twin,
			const string sta_expression="none",
	                	const string chan_expression="none",
	        bool require_coords=true,
	                bool require_orientation=true,
	                        bool require_response=false);
/*!
// Construct an ensemble from a parameter file description.  This
// constructor is useful in combination with the pfstream library.
// In that context blocks of data can be parsed to produce the Pf_ensemble
// regular C data object.  See man pfstream(3).
**/
	TimeSeriesEnsemble(Pf_ensemble *pfe);
#endif
/*!
// Standard copy constructor. 
**/
	TimeSeriesEnsemble(const TimeSeriesEnsemble& tseold);
/*!
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
//\param md is the metadata object to copy to the metadata area for the ensemble.
//\param nmembers is the number of slots to reserve in the new ensemble vector.
**/
	TimeSeriesEnsemble(Metadata& md,int nmembers);
/*!
// Standard assignment operator.
**/
	TimeSeriesEnsemble& operator=(const TimeSeriesEnsemble& tseold);
};

/*!
\brief Object to contain a group (ensemble) of three component seismogram objects. 

// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// three component seismogram data stored as as ThreeComponentSeismogram
// objects.  This object uses an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
**/
class ThreeComponentEnsemble : public Metadata
{
public:
/*!
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
**/
	vector <ThreeComponentSeismogram> member;
/*!
// Default constructor.  Does little, but is not defaulted.  
**/
	ThreeComponentEnsemble();
/*!
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
**/
	ThreeComponentEnsemble(int nsta, int nsamp);
/*!
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
**/
	ThreeComponentEnsemble(int nsta, int nsamp,
				MetadataList& mdl);
#ifndef NO_ANTELOPE
/*!
// Database-driven constructor.  
// Seismic data are often stored today using a database to index the raw
// waveforms.  The database contains attributes loaded here as metadata.
// Attributes that are global to the ensemble are loaded driven by the
// ensemble_mdl list while single 3c seismogram metadata is controlled
// by station_mdl.  Note in both cases the list of names is the internal
// name not external database names.  The AttributeMap object 
// defines the mapping from the internal name space to database
// names.

//\param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//\param station_mdl is a list of metadata attributes to be loaded 
//   into each 3c seismogram's metadata area.  
//\param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensemble.
//\param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
**/
	ThreeComponentEnsemble(DatabaseHandle& db,
		MetadataList& station_mdl,
		MetadataList& ensemble_mdl,
		 AttributeMap& am);
/*!
// Construct a ThreeComponentEnsemble from 
// database-indexed storage returning data defined by a fixed time window.
// In processing continuous seismic data it is commonly necessary 
// to read data for an array of stations within a fixed time window.
// Many algorithms start this way and some assume data are already 
// on a common time base.  This constructor returns a vector of 
// ThreeComponentSeismogram objects spanning a constant time 
// defined by the TimeWindow argument.  i.e. all objects in
// the ensemble will have the same start time (t0).
//
//\exception SeisppError object with an explanatory message if process fails.
//\param dbhi is a handle to the database that indexes these data.
//\param twin defines the window of time to be retrieved
//\param ensemble_mdl defines the list of attributes to be extracted
//   from the database and loaded into the global (ensemble) metadata
//   area.  i.e. this is the list of parameters that are global to the
//   ensemble.
//\param data_mdl defines the list of attributes to be extracted from
//   the database for each TimeSeries object that forms this ensemble.
//\param am is a schema-specific attribute map that defines the mapping
//   from database attribute name space to the seispp internal namespace.
//\param chanmap defines mapping operator for how channel codes map
//   into components.  
**/ 
	ThreeComponentEnsemble(DatabaseHandle& dbhi,
	        TimeWindow twin,
	                MetadataList& ensemble_mdl,
	                        MetadataList& data_mdl,
	                                AttributeMap& am,
						vector<string>chanmap);
/*!
// Standard copy constructor.
**/
#endif
	ThreeComponentEnsemble(const ThreeComponentEnsemble& tseold);
/*!
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
//\param md is the metadata object to copy to the metadata area for the ensemble.
//\param nmembers is the number of slots to reserve in the new ensemble vector.
**/
	ThreeComponentEnsemble(Metadata& md,int nmembers);
/*!
// Standard assignment operator.
**/
	ThreeComponentEnsemble& operator=(const ThreeComponentEnsemble& tseold);
};
/*!
\brief Remove a member of the ensemble using an index.

// Sometimes one needs to edit an ensemble to remove one or more 
// traces.  This is particularly true in using an algorithm that is
// interactive where the user would pick one or more traces to be deleted.
// This method removes one member from the ensemble.
// Note ensemble here is generic and this should work the same on 
// any of the ensemble types in libseispp.
//
//\param ensemble reference to ensemble to be edited.
//\param no trace member number to be deleted.
**/
template <class Tmember>
	void remove_trace(Tmember& ensemble, int no)
{
    if(no >= ensemble.member.size()) return;
    int i;
    typedef typename std::vector<Tmember> vector_type;
    typename vector_type::iterator it;

    it=ensemble.member.begin();
    for(i=0;i<no;++i) ++it;
    ensemble.member.erase(it);
}
/* Current implementations of PeakAmplitude */

/*! Measures peak amplitude for a TimeSeries. */
double PeakAmplitude(TimeSeries *p);
/*! Measures peak amplitude (L2 norm of 3 components) for a ThreeComponent
Seismogram. */
double PeakAmplitude(ThreeComponentSeismogram *p);
/*! Measures peak amplitude for a ComplexTimeSeries. */
#ifndef NO_ANTELOPE
double PeakAmplitude(ComplexTimeSeries *p);
#endif

/*! Generic algorithm to measure and set peak amplitude as a scaling attribute.

Scaling by peak amplitudes is a standard mechanism to scale data for plotting.
This is a generic algorithm for an ensemble that requires a typed function
called PeakAmplitude(Tmember) to be written that actually finds the largest
amplitude of each member.  The result is set in the Metadata for each 
live member with the key defined by scale_attribute.  i.e. the data are not
altered by this algorithm but the measured amplitude is computed and
set as an attribute for each member of the ensemble.  Normal procedure
would then be to call a scaling function to actually alter the data.

Note that traces marked dead are silently skipped.  Beware if an editing
scheme is used that marks traces live/dead interactively.  This algorithm
assumes once dead forever dead.

\param d ensemble to be scan to find peak amplitudes.
\param scale_attribute is the keyword used to define the peak amplitude
	as an attribute for each member of the ensemble.
*/
template <class Tensemble,class Tmember> 
	void MeasureEnsemblePeakAmplitudes(Tensemble& d,
		string scale_attribute)
{
	int i;
	double amplitude;
	for(i=0;i<d.member.size();++i)
	{
		Tmember *dp=&(d.member[i]);
		if(dp->live)
		{
			amplitude=PeakAmplitude(dp);
			dp->put(scale_attribute,amplitude);
		}
	}
}

/*! Generic algorithm to scale an ensemble by using a specified scale factor.

In plotting data it is frequently desirable to scale data by some 
amplitude attribute to maximize the range of what can be seen in the data.
This is a generic algorithm to apply such an algorithm to an 
emsemble of data.  The approach is to assume the amplitude factor was
computed independently by some other method and set as the 
an attribute in the Metadata area of each member.  Thus this generic
algorithm assumes each member has inherited the Metadata object as 
a component of each member AND that each member of the ensemble is 
held in an STL vector container of Tmember objects.  

This algorithm silently skips data marked dead.  If a member does 
not have the amplitude attribute define this algorithm spits out 
an error to stderr and does nothing to the associated member.
This can cause downstream problems, but seems preferable to aborting.

\param d input data ensemble with members of type Tmember.
\param scale_attribute keyword used to access desired amplitude
	attribute.  Each object is multiplied by the value extracted
	as this (real-valued) attribute.
\param invert can be used to scale by 1/scale_attribute.  This can
	be useful to either undo a previous transformation or 
	scale to a fixed gain using a computed amplitude metric.
*/
template <class Tensemble,class Tmember> 
	void ScaleEnsemble(Tensemble& d,
		string scale_attribute,
			bool invert=false)
{
	int i;
	double amplitude;
	for(i=0;i<d.member.size();++i)
	{
		Tmember *dp=&(d.member[i]);
		if(dp->live)
		{
			try {
				amplitude=dp->get_double(scale_attribute);
				// invalid amplitudes are signaled as negative
				if(amplitude>0)
				{
					if(invert)
						ScaleMember(dp,1.0/amplitude);
					else
						ScaleMember(dp,amplitude);
				}
			}
			catch (MetadataGetError mde)
			{
				cerr << "Warning (ScaleEnsemble):  "
					<<"scaling of data failed"<<endl;
				mde.log_error();
				cerr << "Continuing with data left unscaled"
					<<endl;
			}
		}
	}
}
/* Current implementations of ScaleMember. */

/*! Scales a TimeSeries object by scale. */
void ScaleMember(TimeSeries *p,double scale);
/*! Scales a ThreeComponentSeismogram object by scale. */
void ScaleMember(ThreeComponentSeismogram *p,double scale);
#ifndef NO_ANTELOPE
/*! Scales a ComplexTimeSeries object by scale. */
void ScaleMember(ComplexTimeSeries *p,double scale);
#endif

/* Companion to above */

/*! Resets a calibration constant when amplitude scaling is applied.

When the ScaleEnsemble generic algorithm is used the data sample
values are permanently altered.  It is usually essential to preserve
a correct amplitude scale factor so the data can be returned to some
physical units.  This generic algorithm handles that by a simple
manipulation of Metadata attributes.  The user must tell the algorithm
the calibration attribute and the scale factor attribute.

\param d ensemble of data to be changed.  Assumed to be a vector
	of data objects that are or inherit Metadata.
\param calib_attribute calibration attribute name.  Note if this attribute is
	not found in a member data object it is silently assumed to
	be one.  It perhaps could spit something to stderr, but a
	presumption is this is a potentially common problem that 
	should just be quietly handled.
\param scale_attribute attribute containing scaling constant.  This
	must be a real valued attribute.  If it is not defined it
	is set to one and ignored.
\param invert boolean to control how factor is handled.  If true the 
	reciprocal of the scale_attribute is used.  Multiplied if
	true (the default).
*/
template <class Tensemble> void ScaleCalib(Tensemble d,
	string calib_attribute, 
		string scale_attribute, 
			bool invert=false)
{
	double calib;
	double scale;
	for(int i=0;i<d.member.size();++i)
	{
		if(d.member[i].live)
		{
			try {
				calib=d.member[i].get_double(calib_attribute);
			} catch (MetadataGetError mde) 
			{
				calib=1.0;
			}
				
			try {
				scale=d.member[i].get_double(scale_attribute);
			} catch (MetadataGetError mde) 
			{
				scale=1.0;
			}
			if(invert) scale=1.0/scale;
			d.member[i].put(calib_attribute,scale*calib);
		}
	}
}
/*! Set an attribute to a constant to initialize a gather to some default. 

The type of the attribute to be set is passed through the second template
argument.  

\param d ensemble of data to initialize.
\param name is the name of the attribute to initialize
\param val Ta type value with which to initialize ensemble members.
*/
template <class Te, class Ta> 
	void InitializeEnsembleAttribute(Te& d, string name,
		Ta val)
{
	try {
		int i;
		for(i=0;i<d.member.size();++i)
			d.member[i].put(name,val);
	} catch(...){throw;};
}
#ifndef NO_ANTELOPE
/*! \brief Finds arrival time picks for a particular phase spanned by a seismogram.

In passive array processing a common thing one needs to do is associate a seismogram
with a measured arrival time for a particular phase.  This template does this 
for an ensemble of generic seismic data objects (ensemble) defined in this library.
The algorithm finds matching arrivals for a given phase in a database.  To make
the match it uses a time window driven by predicted arrivals.  The procedure finds
the time span defined by predicted arrivals in the data set given.  It does this by
requesting this attribute from each trace.  If the predicted arrival attribute is 
not define on any object in the ensemble and exception is thrown.  Otherwise the 
time interval used to find arrivals is the range of predicted arrivals extended
by tpad on both ends.  The procedure then initializes the arrival time attribute
field to a null value for all data members (live or dead).  It subsets the
database and then loads arrivals into each data member that match the phase
and time interval condition. 

\param d data ensemble to which this procedure is to be applied.
\param dbi generic database handle.  In this implementation this is immediately
	cast to a DatascopePointer.  Future work could make this more generic.
\param phase is the seismic phase for which an arrival time is to be loaded.
\param predarrkeyword is the predicted arrival time keyword used to fetch this
	attribute from each data member of the ensemble passed through d.
\param atkeyword is the arrival time keyworded used to load the arrival time
	into the Metadata of each member of the ensemble.  
\param tpad time padding around around time window computed from predicted arrival
	times stored with these data.
\param nullvalue is the value loaded as the arrival time when there is no entry 
	in the database for a data member.  This assures this attribute is set
	for all members.  This is useful, for example, to sort data.  

\exception SeisppError is thrown in a few conditions that represent serious
	problems.  A process should probably die if this procedure throws this
	exception as the intent is this be pretty bombproof.
\author Gary L. Pavlis
*/


template <class Tensemble> void LoadEventArrivals(Tensemble& d, 
		DatabaseHandle& dbi,
			string phase,
				string predarrkeyword,
					string atkeyword,
						double tpad,
							double nullvalue)
{
	DatascopeHandle dbh=dynamic_cast<DatascopeHandle&> (dbi);
	DatascopeHandle dbhss(dbh);   /* working copy */
	/* First scan the ensemble for the range of predicted arrival times.
	Initialize with negative values to provide error check below */
	TimeWindow atrange(-2.0,-1.0);
	double testval;
	bool firstpass(true);
	int i;
	for(i=0;i<d.member.size();++i)
	{
		if(d.member[i].is_attribute_set(predarrkeyword))
		{
			testval=d.member[i].get_double(predarrkeyword);
			if(firstpass)
			{
				atrange.start=testval;
				atrange.end=testval;
				firstpass=false;
			}
			else
			{
				if(atrange.start>testval)
					atrange.start=testval;
				if(atrange.end<testval)
					atrange.end=testval;
			}
		}
	}
	if(atrange.start<0.0)
		throw SeisppError(string("LoadEventArrivals:  no predicted arrivals are defined")
			+ string(" in ensemble passed to this procedure\n")
			+ string("Coding error or problem in the way metadata were loaded.") );
	atrange.start -= tpad;
	atrange.end += tpad;
	char ssexpression[256];
	sprintf(ssexpression,"(time >= %lf && time<=%lf && iphase=~/%s/)",
		atrange.start,atrange.end,phase.c_str());
	dbhss.subset(string(ssexpression));
	/* We always initialize the full ensemble to null values.  This is needed
	in case an exception is thrown by one of the get routines */
	for(i=0;i<d.member.size();++i)
			d.member[i].put(atkeyword,nullvalue);
	if(dbhss.number_tuples()>0)
	{
		try {
			map<string,double> atimes;
			map<string,double>::iterator atptr;
			double t;
			string sta;
			dbhss.rewind();
			for(i=0;i<dbhss.number_tuples();++i,++dbhss)
			{
				sta=dbhss.get_string("sta");
				t=dbhss.get_double("arrival.time");
				atimes[sta]=t;
			}
			for(i=0;i<d.member.size();++i)
			{
				sta=d.member[i].get_string("sta");
				atptr=atimes.find(sta);
				if(atptr!=atimes.end())
				{
					d.member[i].put(atkeyword,atptr->second);
				}
			}
		}
		catch (...) 
		{
			throw SeisppError(string("LoadEventArrivals:  ")
			+ string("Problems arrivals.  Not all arrivals were loaded") );
		}
	}
	/*WARNING:  the handle really should automatically do this, but in the 
	current implementation it does not.  This will produce a memory leak if
	we don't do this.  If changed this next line must be removed */
	//dbfree(dbhss.db);
}
#endif
/*! \brief Load predicted times for a general ensemble.

This is a generic algorithm to implement posting predicted arrival times
to every member of a generic ensemble.  The method uses is not appropriate
for event (common source) gathers because it loads the source information
for each member of the ensemble individually.  For this reason the 
following seven attributes must be loaded into the Metadata area of
every member of the ensemble:  source_lat, source_lon, source_depth,
source_time, sta_lat, sta_lon, and sta_elev.  Note this algorithm
assumes the seispp convention that all angles in Metadata are stored
as degrees, NOT radians.  

Traces marked dead are handled gracefully, but of course will not
have valid results.  Specifically the predicted time result is posted
as 0 since a 0 epoch time is nonsense.

\param d ensemble object to be processed
\param phase phase name for which arrival time is to be computed
\param predarr_keyword is the key used to store the predicted time in
	the Metadata area for each ensemble member.  (default is
	the global value for seispp defined in SeisppKeywords.h.
	Currently predarr_time.)
\param ttmethod travel time method passed to ttcalc interface 
	(default tttaup).
\param ttmodel travel time model passed to ttcalc interface
	(default iasp91).
\param verbose if true every error will cause an error messae
	to be be written to stderr.  Otherwise be silent and 
	just return the count or errors (default=false)

\return Normal return is 0.  Nonzero values indicate number of
	failures in computing times.  Failed data members have
	predicted time set to 0.0.

*/
template <class Tensemble> int LoadPredictedArrivalTimes(Tensemble& d,
	string phase,
		string predarr_keyword=predicted_time_key,
			string ttmethod="tttaup",
				string ttmodel="iasp91",
					bool verbose=false)
{
	int i,nmembers;
	double slat,slon,sz,stime;
	double rlat,rlon,relev;
	double phasetime;
	int nfailures(0);

	nmembers=d.member.size();
	for(i=0;i<nmembers;++i)
	{
		if(d.member[i].live)
		{
			try{
				slat=d.member[i].get_double("source_lat");
				slon=d.member[i].get_double("source_lon");
				sz=d.member[i].get_double("source_depth");
				stime=d.member[i].get_double("source_time");
				rlat=d.member[i].get_double("sta_lat");
				rlon=d.member[i].get_double("sta_lon");
				relev=d.member[i].get_double("sta_elev");
				slat=rad(slat); 
				slon=rad(slon);
				rlat=rad(rlat);
				rlon=rad(rlon);
				Hypocenter h(slat,slon,sz,stime,
					ttmethod,ttmodel);
				phasetime=h.phasetime(rlat,rlon,relev,phase);
				phasetime+=stime;
			}
			catch (MetadataGetError mderr)
			{
				phasetime=0.0;
				++nfailures;
				if(verbose)
				{
					cerr << "LoadPredictedArrivalTimes:  "
					 << "get failed on attribute name="
					<< mderr.name
					<<" for ensemble member number "
					<< i <<endl
					<< "Arrival time set to 0.0"<<endl;
				}
			}
			catch (SeisppError serr)
			{
				phasetime=0.0;
				++nfailures;
				if(verbose)
				{
					cerr << "LoadPredictedArrivalTimes:  "
					 << "travel time calculator error "
					 << "for member="<<i <<endl
					 << "SeisppError message:"<<endl;
					serr.log_error();
					cerr << "Arrival time set to zero"<<endl;
				}
			}

		}
		else
		{
			phasetime=0.0;
		}
		d.member[i].put(predarr_keyword,phasetime);
	}
	return(nfailures);
}

/*! Extract a componnt from a ThreeComponentEnsemble to yield a TimeSeriesEnsemble.

An ensemble of three component data can be conceptualized as a three-dimensional
array (3 by number of samples by number of ensemble members) while a TimeSeriesEnsemble
is effectively 2D (although implemented in a more general way in SEISPP).  
It is often useful to extract one of the components of a ThreeComponent data set
to yield a scalar (TimeSeriesEnsemble) result.  This procedure does this.

\param tcs input ensemble
\param component is data component to extract.  Must be 0,1, or 2 or the procedure
	will throw an exception.
\return auto_ptr to ensemble containing requested component

\exception SeisppError is throw if result is empty of component number is illegal.
*/
auto_ptr<TimeSeriesEnsemble> ExtractComponent(ThreeComponentEnsemble& tcs,int component);
} // End SEISPP namespace declaration
#endif
