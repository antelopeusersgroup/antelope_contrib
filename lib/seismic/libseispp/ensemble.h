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

#include <vector>
#include "pfstream.h"
#include "TimeWindow.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
namespace SEISPP {
//@{
// Object to contain a group (ensemble) of time series objects (seismograms).
// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// time series data using an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//@author Gary L. Pavlis
//@}
class TimeSeriesEnsemble : public Metadata
{
public:  
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <TimeSeries> member;

//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	TimeSeriesEnsemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	TimeSeriesEnsemble(int ntsin, int nsampin);
//@{
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
//@throw SeisppError object if one of several unrecoverable errors
//   occur.  These are of two main type.  First, if any of the 
//   require booleans are true and not satisfied (e.g. site table
//   is missing when require_coords is true) the constructor will
//   abort by throwing an error.  Second, if any of the Antelope
//   routines that underly this constructor fail the Antelope elog
//   error dump routine is called and the constructor will throw an
//   exception.
//
//@param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//@param station_mdl is a list of metadata attributes to be loaded 
//   into each seismogram's metadata area.  
//@param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensemble.
//@param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
//@}
	TimeSeriesEnsemble(DatabaseHandle& dbhi,
	        MetadataList& ensemble_mdl,
	                MetadataList& data_mdl,
	                        AttributeMap& am);
//@{
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
//@throw SeisppError object if one of several unrecoverable errors
//   occur.  These are of three main type.  First, if any of the 
//   require booleans are true and not satisfied (e.g. site table
//   is missing when require_coords is true) the constructor will
//   abort by throwing an error.  Second, if any of the Antelope
//   routines that underly this constructor fail the Antelope elog
//   error dump routine is called and the constructor will throw an
//   exception.  Finally, SeisppError objects can be propagated from 
//   seispp routines called internally by this constructor.
//
//@param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//@param sta_expression regular expression to sift station codes 
//   (e.g. A.* for all stations starting with A or ..2 for all 3 station
//   codes ending in 2.)
//@param chan_expression regular expression to sift channel codes 
//   (e.g. BH. for all BH channels or .* for all).
//@param require_coords require that a site table be present.  (default true)
//@param require_orientation require that sitechan orientation information
//   (hang, vang) be present. (default true)
//@param require_response require the set of css3.0 response tables be 
//   present.   (Note that response data is currently ignored.  This is
//   a placeholder.)  default is false
//@}
	TimeSeriesEnsemble(DatabaseHandle& db,
		TimeWindow twin,
			const string sta_expression="none",
	                	const string chan_expression="none",
	        bool require_coords=true,
	                bool require_orientation=true,
	                        bool require_response=false);
//@{
// Construct an ensemble from a parameter file description.  This
// constructor is useful in combination with the pfstream library.
// In that context blocks of data can be parsed to produce the Pf_ensemble
// regular C data object.  See man pfstream(3).
//@}
	TimeSeriesEnsemble(Pf_ensemble *pfe);
//@{
// Standard copy constructor. 
//@}
	TimeSeriesEnsemble(const TimeSeriesEnsemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
//@param md is the metadata object to copy to the metadata area for the ensemble.
//@param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	TimeSeriesEnsemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	TimeSeriesEnsemble& operator=(const TimeSeriesEnsemble& tseold);
//@{
// Remove a member of the ensemble using an index.
// Sometimes one needs to edit an ensemble to remove one or more 
// traces.  This is particularly true in using an algorithm that is
// interactive where the user would pick one or more traces to be deleted.
// This method removes one member from the ensemble.
//
//@param no trace member number to be deleted.
//@}
	void remove_trace(int no);
};

//@{
// Object to contain a group (ensemble) of three component seismogram objects. 
// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// three component seismogram data stored as as ThreeComponentSeismogram
// objects.  This object uses an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//@}
class ThreeComponentEnsemble : public Metadata
{
public:
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <ThreeComponentSeismogram> member;
//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	ThreeComponentEnsemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	ThreeComponentEnsemble(int nsta, int nsamp);
//@{
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
//@}
	ThreeComponentEnsemble(int nsta, int nsamp,
				MetadataList& mdl);
//@{
// Database-driven constructor.  
// Seismic data are often stored today using a database to index the raw
// waveforms.  The database contains attributes loaded here as metadata.
// Attributes that are global to the ensemble are loaded driven by the
// ensemble_mdl list while single 3c seismogram metadata is controlled
// by station_mdl.  Note in both cases the list of names is the internal
// name not external database names.  The AttributeMap object 
// defines the mapping from the internal name space to database
// names.

//@param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
//@param station_mdl is a list of metadata attributes to be loaded 
//   into each 3c seismogram's metadata area.  
//@param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensemble.
//@param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
//@}
	ThreeComponentEnsemble(DatabaseHandle& db,
		MetadataList& station_mdl,
		MetadataList& ensemble_mdl,
		 AttributeMap& am);
//@{
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
//@throws SeisppError object with an explanatory message if process fails.
//@param dbhi is a handle to the database that indexes these data.
//@param twin defines the window of time to be retrieved
//@param ensemble_mdl defines the list of attributes to be extracted
//   from the database and loaded into the global (ensemble) metadata
//   area.  i.e. this is the list of parameters that are global to the
//   ensemble.
//@param data_mdl defines the list of attributes to be extracted from
//   the database for each TimeSeries object that forms this ensemble.
//@param am is a schema-specific attribute map that defines the mapping
//   from database attribute name space to the seispp internal namespace.
//@param chanmap defines mapping operator for how channel codes map
//   into components.  
//@} 
	ThreeComponentEnsemble(DatabaseHandle& dbhi,
	        TimeWindow twin,
	                MetadataList& ensemble_mdl,
	                        MetadataList& data_mdl,
	                                AttributeMap& am,
						vector<string>chanmap);
//@{
// Standard copy constructor.
//@}
	ThreeComponentEnsemble(const ThreeComponentEnsemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
//@param md is the metadata object to copy to the metadata area for the ensemble.
//@param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	ThreeComponentEnsemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	ThreeComponentEnsemble& operator=(const ThreeComponentEnsemble& tseold);
//@{
// Remove a member of the ensemble using an index.
// Sometimes one needs to edit an ensemble to remove one or more 
// traces.  This is particularly true in using an algorithm that is
// interactive where the user would pick one or more traces to be deleted.
// This method removes one member from the ensemble.
//
//@param no trace member number to be deleted.
//@}
	void remove_trace(int no);
};
template <class Tmember>
	void remove_traces(vector<int> tobe_removed)
{
    int i;
    vector<Tmember>::iterator it;
    list< <vector<Tmember>::iterator > dellist;

    it=member.begin();
    i=0;
    while (it != member.end()) {
        if (find(tobe_removed.begin(),tobe_removed.end(),i)
		!=tobe_removed.end()) 
	{
            dellist.push_back(it);
        } 
	else 
	{
		it++;
	}
        i++;
    }
    list< <vector<Tmember>::iterator >::iterator delptr;
    for(delptr=dellist.begin();delptr!=dellist.end();++delptr)
	member.erase(delptr);
}
template <class Tmember>
	void remove_trace(int no)
{
    int i;
    vector<Tmember>::iterator it;
    list< <vector<Tmember>::iterator > dellist;

    it=member.begin();
    i=0;
    while (it != member.end()) {
        if (i==no) {
            member.erase(it);
	    break;
        } else it++;
        i++;
    }
}

} // End SEISPP namespace declaration
#endif
