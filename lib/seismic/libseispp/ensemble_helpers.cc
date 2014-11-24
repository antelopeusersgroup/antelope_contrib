#include <vector>
#include <string>
#include "seispp.h"
#include "stock.h"
using namespace SEISPP;
using namespace std;
namespace SEISPP{
#ifndef NO_ANTELOPE
/* This file contains tools that operate on ensemble objects
// to subset or sort ensembles in common ways.  May eventually
// generate a generalized sort or subset method, but for now
// it will be built up in stages with the most common uses
// built first.  All the functions in this file should return
// an auto_ptr to a new Ensemble object (scalar or 3c time
// series) derived from a parent.  
*/

//@{
// Builds a new ensemble of members that satisfy unix regular expression
// for sta and chan attributes passed as sta_expr and chan_expr.
//
// @param parent original ensemble to be subsetted
// @param sta_expr unix regular expression to apply to sta Metadata
//    attribute
// @param chan_expr unix regular expression to apply to chan Metadata 
//    attribute
//
//@author Gary L. Pavlis
//@}
auto_ptr<TimeSeriesEnsemble> StaChanRegExSubset(TimeSeriesEnsemble& parent,
	string sta_expr, string chan_expr)
{
	int i;
	string sta;
	string chan;
	Hook **stahook=NULL;
	Hook **chanhook=NULL;
	// This clones metadata for the ensemble, but allocs no space
	// for data members.  Since we have no way of knowing the
	// output size this is a good use of the automatic resizing
	// ability of the stl vector
	auto_ptr<TimeSeriesEnsemble> result( new TimeSeriesEnsemble(
				dynamic_cast<Metadata&>(parent),0));

	for(i=0;i<parent.member.size();++i)
	{
		try{
			sta=parent.member[i].get_string("sta");
			chan=parent.member[i].get_string("chan");
			if( strmatches(const_cast<char *>(sta.c_str()),
				const_cast<char *>(sta_expr.c_str()),stahook)
			 && strmatches(const_cast<char *>(chan.c_str()),
				const_cast<char *>(chan_expr.c_str()),chanhook) )
			{
				result->member.push_back(parent.member[i]);
		
			}

		} catch (MetadataGetError& mde) {
			cerr << "StaChanRegExSubset (Warning):  sta/chan metadata error in building ensemble subset. "<<endl;
			mde.log_error();
		}
	}
	return(result);
}
auto_ptr<TimeSeriesEnsemble> ArraySubset(TimeSeriesEnsemble& parent,
                			SeismicArray& sa)
{
	int expected_size=sa.array.size();
	auto_ptr<TimeSeriesEnsemble> result(new TimeSeriesEnsemble(dynamic_cast<Metadata&>(parent),
						expected_size));
	string sta;
	map<string,SeismicStationLocation>::iterator aptr,aptr_end;
	aptr_end=sa.array.end();
	for(int i=0;i<parent.member.size();++i)
	{
		try{
			sta=parent.member[i].get_string("sta");
			aptr=sa.array.find(sta);
			if(aptr!=aptr_end) result->member.push_back(parent.member[i]);
		} catch (MetadataGetError& mde) {
			cerr << "ArraySubset (Warning):  "
				<< "Missing sta attribute in parent ensemble member number "
				<< i << endl;
			mde.log_error();
		}
	}
	return(result);
}
#endif
/*  Extract a single component from an ensemble to produce a scalar ensemble. */
auto_ptr<TimeSeriesEnsemble> ExtractComponent(ThreeComponentEnsemble& tcs,int component)
{
	vector<ThreeComponentSeismogram>::iterator tcsp;
	TimeSeries *x;
	auto_ptr<TimeSeriesEnsemble> result(new 
		TimeSeriesEnsemble(dynamic_cast<Metadata&>(tcs),tcs.member.size()));
	for(tcsp=tcs.member.begin();tcsp!=tcs.member.end();++tcsp)
	{
		// silently skip anything that throws an exception.  
		// sanity test at end to throw an exception if the result is empty
		try {
			
			x=ExtractComponent(*tcsp,component);
			result->member.push_back(*x);
			delete x;
		} catch(...){};
	}
	if(result->member.size()<=0)
		throw SeisppError(
			string("SEISPP::ExtractComponent ThreComponentEnsemble procedure: ")
			+ string("Output TimeSeriesEnsemble is empty."));
	return(result);
}

} // End SEISPP namespace
