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
		try {
			
			x=ExtractComponent(*tcsp,component);
			result->member.push_back(*x);
			delete x;
		} catch(SeisppError& serr)
                {
                    if(SEISPP_verbose)
                        serr.log_error();
                }
                catch(...){
                    cerr << "ExtractComponent:  "
                        <<"Something threw an undefined exception"<<endl;
                }
	}
	if(result->member.size()<=0)
		throw SeisppError(
			string("SEISPP::ExtractComponent ThreeComponentEnsemble procedure: ")
			+ string("Output TimeSeriesEnsemble is empty."));
	return(result);
}
int component_count(vector<int> comps)
{
    bool hits[3];
    int i;
    for(i=0;i<3;++i) hits[i]=false;
    vector<int>::iterator cptr;
    for(cptr=comps.begin();cptr!=comps.end();++cptr)
    {
        // quietly ignore illegal values 
        if((*cptr)<0) continue;
        if((*cptr)>2) continue;
        hits[*cptr]=true;
    }
    int result;
    for(result=0,i=0;i<3;++i)
        if(hits[i]) ++result;
    return(result);
}
#ifndef NO_ANTELOPE
/* This procedure does more or less the inverse of ExtractComponent.
That process is far more complicated, however, becasue of the complexity 
of multiple channel and loc codes.  We handle that through the 
StationChannelMap object. 
 
Note most of this code is borrowed from array_get_data_3c.cc*/
void BundleChannels(TimeSeriesEnsemble& rawdata, ThreeComponentEnsemble& dtcs,
        StationChannelMap scmap)
{
    string current_sta;
    vector<int> comp,prec,member_index;
    vector<TimeSeries> components;
    string chan;
    int i,j;
    int rawsize=rawdata.member.size();
    try{
        if(SEISPP_verbose)
            cout << "BundleChannels procedure:  attempting to assemble "
                << rawdata.member.size()<<" channels into 3C bundles"<<endl;
        /* Sort the data by sta chan for the algorithm here to work */
        StaChanSort(rawdata);
        //initialize before loop - assumes rawdata not empty 
        current_sta=rawdata.member[0].get_string("sta");;
        ThreeComponentChannelMap tccm=scmap.channels(current_sta);
        for(i=0;i<rawsize;++i)
        {
            string nextsta=rawdata.member[i].get_string("sta");
            if(nextsta==current_sta)
            {
                try{
                    chan=rawdata.member[i].get_string("chan");
                    comp.push_back(tccm.component(chan));
                    prec.push_back(tccm.precedence(chan));
                    member_index.push_back(i);
                }catch (SeisppError& serr)
                {
                  cerr << "BundleChannels (Warning):   Problem constructing "
                    << "ensemble member = "<<i<<endl
                    << "This seismogram links to station "
                    << current_sta<<endl
                    << "SeisppError message thrown follow"<<endl;
                  serr.log_error();
                }
            }
            if( (nextsta!=current_sta) || (i>=rawsize-1) )
            {
                try {
                /*Always dangerous to decement a loop counter but
                  otherwise we would drop the last channel in this
                  bundle with this logic borrowed form array_get_data_3c.cc
                  tccm is loaded for next sta as part of this odd logic. 
                  */
                if(i<(rawsize-1)) --i;
                tccm=scmap.channels(nextsta);
                int ncomponents=comp.size();
                /* Note this check for valid component is new and not
                   part of code in array_get_data_3c.cc.  It also 
                   alters the if logic a bit  */
                int n_bundle_components;
                n_bundle_components=component_count(comp);
                if(n_bundle_components != 3)
                {
                    cerr << "BundleComponents:  Incomplete channels."<<endl
                        << "Bundle with "<< ncomponents 
                        << " has only "<<n_bundle_components 
                        << " of required 3 independent components"
                        << endl
                        << "Data for station "<<nextsta<<" dropped"<<endl;

                }
                else if(ncomponents==3)
                {
                    /* Make sure we do not have any parallel components
                       with different channel codes and this was simply
                       an accident we got 3 as the count */
                    for(j=0;j<ncomponents;++j)
                        components.push_back(rawdata.member[member_index[j]]);
                    ThreeComponentSeismogram new3c(components);
                    dtcs.member.push_back(new3c);
                    comp.clear();
                    member_index.clear();
                    components.clear();
                }
                else
                {
                    /* Land here only when ncomponents > 3 because the 
                       number components test will handle less than 3 
                       case */
			vector<int>::iterator minprec,maxprec;
			minprec=min_element(prec.begin(),prec.end());
			int minval=*minprec;
			maxprec=max_element(prec.begin(),prec.end());
			int maxval=*maxprec;
			int iprec;
			for(iprec=minval;iprec<=maxval;++iprec)
			{
			    for(j=0;j<comp.size();++j)
			    {
				if(prec[j]==iprec)
				{
                               	    components.push_back(rawdata.member[member_index[j]]);
				}
			    }
			    if(components.size()==3)
			    {
				ThreeComponentSeismogram new3c(components);
                                dtcs.member.push_back(new3c);
				break;
			    }
			    else
			    {
				components.clear();
			    }
			}
		        if(components.size()!=3)
				cerr << "BundleChannels:  Irregular data from station "
					<< nextsta
					<< ".  Data from this station dropped."
					<<endl;
			comp.clear();
			prec.clear();
			member_index.clear();
			components.clear();
                }
                }catch(SeisppError& serr)
                {
                    cerr << "BundleChannels:  Problems assembling data "
                        << "for station "<<current_sta<<endl
                        << "The following SeisppError message was posted:"
                        <<endl;
                    serr.log_error();
                    cerr << "Data from this station will be dropped"
                        <<endl;
                }
                current_sta=nextsta;
            }
        }
        if(SEISPP_verbose)
            cout << "BundleChannels procedure:  "
                << "ensemble number of 3C stations assembled="
                << dtcs.member.size()<<endl;
    }catch(...){throw;};
}
#endif
} // End SEISPP namespace
