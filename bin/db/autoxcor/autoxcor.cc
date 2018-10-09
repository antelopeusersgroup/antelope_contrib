#include <vector>
#include <list>
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <time.h>
#include <cmath>
#include <complex>

#include "SeisppKeywords.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
#include "Metadata.h"
#include "Hypocenter.h"
#include "dbpp.h"
#include "filter++.h"
#include "resample.h"
#include "seispp.h"
#include "stack.h"
#include "SignalToNoise.h"
#include "MultichannelCorrelator.h"
#include "PfStyleMetadata.h"

/*
This the program that aligns all the traces in a event gather and computes
the stack of all vertical components. Input and output
are all based on antelope database. 
All parameters are defined in the autoxcor.pf.
*/

using namespace std;
using namespace SEISPP;

bool SEISPP::SEISPP_verbose(true);
/*! \brief Generic algorithm to load arrival times from a database.

In passive array processing a very common need is to extract time windows
around marked phase pick or a theoretical arrival time.  For measured
arrival times the css database has an awkward link to waveforms that 
causes problems when dealing with continuous data.  This is one element of
a set of functions aimed at dealing with this problem.  

This particular procedure aims to take an input ensemble of data and 
find matches for arrival times in an external database.  For each member
of the ensemble with a matching arrival it posts the arrival time to the
generalized header (Metadata) of the parent object.  To avoid testing 
for valid arrivals in the ensemble after this procedure is run a index
of data with valid arrivals is returned as an stl list container of ints.

\param dat is the input ensemble passed as a reference to the vector 
	container of data objects.  The type of the objects in the container
	is generic except class T MUST be an object that inherits Metadata.
	At time of this writing T could be TimeSeries, ThreeComponentSeismogram,
	or ComplexTimeSeries. 
\param dbh match handle to a Datascope database containing arrival. 
	Normally this should reference the standard catalog view.  That
	is:  event->origin->assoc->arrival subsetted to orid==prefor.
	The algorithm actually only cares that a find using the Metadata
	area of the vector data components will provide a unique match
	into the table the handle references (normally arrival).  This 
	tacitly assumes the handle has been properly constructed and the
	proper attributes have been loaded with the data to provide a unique
	match into arrival.  With css this REQUIRES that each component of
	dat MUST have had evid and/or orid posted before calling this function.  There
	is no other unambiguous way to match waveforms in this context to
	a unique arrival. 
\param keyword is the attribute name used to posted the arrival time data.

\return list of members of input vector with valid arrival times posted.
	The contents of the list can be traversed and used with operator[]
	to extract valid data from the ensemble.  
*/
list<long> LoadArrivalTimes(vector<ThreeComponentSeismogram>& dat,
                DatascopeMatchHandle& dbh,
		    const string keyword)
{
        std::vector<ThreeComponentSeismogram>::iterator d;
	long i;
	list<long> data_with_arrivals;
	const string base_error("Warning (LoadArrivalTimes): ");
	for(d=dat.begin(),i=0;d!=dat.end();++d,++i)
	{
		double atime;  //  arrival time.  
		long aid;
		if(d->live)
		{
		// First see if there is an arrival for this
		// station.  If not, skip it. 
			list<long> records
				=dbh.find(dynamic_cast<Metadata&>(*d),false);
			// if no arrival silently skip data for this station
			if(records.size()<=0) continue;
			if(records.size()>1)
			{
				string sta=d->get_string("sta");
				cerr << base_error 
					<< "found "
					<< records.size()
					<< " arrivals for station "
					<< sta <<endl
					<< "Using first found "
					<< "in database view"<<endl;
			}
			Dbptr db=dbh.db;
			// tricky usage here.  begin() returns
			// an iterator so the * operator gets the
			// value = record number of the match
			db.record=*(records.begin());
			char csta[10];
			if(dbgetv(db,0,"arrival.time",&atime,"sta",csta,0)
				== dbINVALID) 
			{
				string sta=d->get_string("sta");
				cerr << base_error
					<< "dbgetv failed"
					<< " in attempt to obtain"
					<< " arrival time for station"
					<< sta << endl
					<< "Data from this station"
					<< " will be dropped"<<endl;	
			}
			dbgetv(db,0,"arrival.arid",&aid,"sta",csta,0);
			d->put(keyword,atime);
			d->put(keyword+".arid",aid);
			data_with_arrivals.push_back(i);
		}
	}
	return(data_with_arrivals);
} 
void PostEvid(ThreeComponentEnsemble *d,int evid)
{
	vector<ThreeComponentSeismogram>::iterator dptr;
	for(dptr=d->member.begin();dptr!=d->member.end();++dptr)
		dptr->put("evid",evid);
}
/*! \brief Builds a standard catalog view from a CSS3.0 database.

Much passive array processing is built around the css3.0 schema.
The css3.0 schema has a standard view that defines the definitive
catalog for a network.  This is formed by the join of:
	event->origin->assoc->arrival
and is usually (as here) subsetted to only keep rows with
orid equal to the "preferred origin" (prefor).  This procedure
builds a handle to this database view.

\param dbh is a handle to the parent Datascope database 
	from which the view is to be constructed.  It need
	only reference the correct database. 
\return new handle that refers to the "standard" catalog view
	described above.
*/
DatascopeHandle StandardCatalogView(DatascopeHandle& dbh)
{
	DatascopeHandle result(dbh);
	dbh.lookup("event");
	dbh.natural_join("origin");
	string ss_to_prefor("orid==prefor");
	dbh.subset(ss_to_prefor);
	dbh.natural_join("assoc");
	dbh.natural_join("arrival");
	return(dbh);
}
/*! \brief Multiple stage TimeInvariantFilter operator.

Sometimes it is necessary to apply a series of stages of
filtering to equalize different data sets or just to 
simply the process of defining a multiple filter chain.
This object simplifies that process by allowing the definition
of a chain of filters and a set of supplied methods to apply
these filters to data.
*/
class MultiStageFilter
{
public:
	/*! \brief Default constructors.  

	Loads a null filter definition.  That is the default is
	a do nothing operator. */
	MultiStageFilter();
	/*! \brief Construct the filter definitions from a string.

	This is currently the prime constructor.  A string is parsed
	into tokens that describe a series of filters that will be
	chained together.  The strings that define each individual
	filter type are parsed into blocks determined by the separator
	argument.  The parsed strings are currently used to construct
	a set of TimeInvariantFilter processing objects.
	An example helps explain how this would be used.  If we
	passed "DEMEAN; BW 0.5 5 2 5" and define ";" as the separator
	this would yield a two stage filter:  demean followed by a 
	0.5 to 2 Hz bandpass filter.

	\param filterlist contains the set of filter recipes to use.
	\param separator is the string used as a separator between
		the recipes for each filter description.
	*/
	MultiStageFilter(string filterlist,string separator);
	template <class T> void  apply(T& d);
private:
	list<TimeInvariantFilter> stages;
};
MultiStageFilter::MultiStageFilter()
{
	TimeInvariantFilter f(string("none"));
	stages.push_back(f);
}
MultiStageFilter::MultiStageFilter(string filterlist,string separator)
{
    try {
	const string white(" \t\n");
	int current=0,end_current;
	string stmp;
	string filterparam;
	// Strip any leading white space
	stmp=filterlist;
	if((current=stmp.find_first_not_of(white,0)) != 0)
	{
		stmp.erase(0,current);
		current=0;
	}
	int endstmp=stmp.size();
	do {
		end_current=stmp.find_first_of(separator,current);
		if(end_current<0)
		{
			filterparam.assign(stmp,current,endstmp);
			stages.push_back(TimeInvariantFilter(filterparam));
			break;
		}			
		filterparam.assign(stmp,current,end_current-current);
		stages.push_back(TimeInvariantFilter(filterparam));
		current=stmp.find_first_not_of(separator,end_current);
		current=stmp.find_first_not_of(white,current);
	}
	while(current<endstmp && current>=0);
    } catch (...) {throw;};
}
		
	
	
/* For now this only works on ensembles.  */
template <class T> void MultiStageFilter::apply(T& d)
{
    try {
	list<TimeInvariantFilter>::iterator filt;
	for(filt=stages.begin();filt!=stages.end();++filt)
		FilterEnsemble(d,*filt);
	/* Note this template could be made to work with TimeSeries
	 or ThreeComponentSeismogram components individually if 
	we used a typeid check on T and then used this line
	for that type of beast:  filt->apply(d);
	*/
    } catch (...) {throw;};
}
void ApplyFST(ThreeComponentEnsemble& e,Hypocenter& hypo)
{
	double vp0(6.0),vs0(3.5);
	for(int i=0;i<e.member.size();++i)
	{
		double lat,lon,elev;
		lat=e.member[i].get_double("lat");
		lon=e.member[i].get_double("lon");
		elev=e.member[i].get_double("elev");
		SlownessVector u=hypo.pslow(lat,lon,elev);
		e.member[i].free_surface_transformation(u,vp0,vs0);
	}
}
void ApplyFST(ThreeComponentSeismogram& e,Hypocenter& hypo)
{
	double vp0(6.0),vs0(3.5);
	double lat,lon,elev;
	lat=e.get_double("lat");
	lon=e.get_double("lon");
	elev=e.get_double("elev");
	SlownessVector u=hypo.pslow(lat,lon,elev);
	e.free_surface_transformation(u,vp0,vs0);
	
}

template <class T> T VectorMedian(vector<T> v)
{
	sort(v.begin(),v.end());
	int size=v.size();
	return (size % 2 ? v[size/2] : (v[size/2-1]+v[size/2])/2);
}
template <class T> T VectorMean(vector<T> v)
{
	T sum=0.0;
	for(int i=0;i<v.size();i++)
		sum+=v[i];
	return (sum/v.size());
}

TimeWindow SemblanceSearch(TimeSeriesEnsemble &t, Metadata &md)
{
	/*wind=1000;
	da=x3(:,:);
	sembl=zeros(floor(size(da,1)/wind)*2-1,1);
	k=0;
	for i=wind/2:wind/2:floor(size(da,1)/wind)*wind-wind/2
	    k=k+1;
	    stack=median(da(i-(wind/2-1):i+wind/2,:),2)*size(da,2);
	    danorm=zeros(size(da,2),1);
	    for j=1:size(da,2)
		danorm(j)=norm(da(i-(wind/2-1):i+wind/2,j));
	    end
	    sembl(k)=norm(stack)/(median(danorm)*size(da,2));
	end*/
	double dt=md.get_double("target_sample_interval");
	int wind=(int)floor(md.get_double("semblance_window")/dt);
	double sembllimit=md.get_double("semblance_lower_limit");
	double tstart=md.get_double("semblance_window_start");
	
	int num_gather=t.member.size();
	
	int maxns=0;
	int maxtrace=0;
	for(int i=0;i<num_gather;i++)
		if(t.member[i].ns>maxns)
		{
			maxns=t.member[i].ns;
			maxtrace=i;
		}
	
	vector<double> sembl;
	sembl.reserve(floor(maxns/wind)*2-1);
	for(int i=0;i<floor(maxns/wind)*2-1;i++)
	{
		//int index=(i+1)*wind/2;
		//vector<double> stack;
		//stack.reserve(wind);
		double normstack=0;
		for(int j=i*wind/2;j<(i+2)*wind/2;j++)
		{
			vector<double> temp;
			temp.reserve(num_gather);
			int count=0;
			for(int k=0;k<num_gather;k++)
				if(!t.member[k].is_gap(j))
				{
					temp.push_back(t.member[k].s[j]);
					count++;
				}
			double median = VectorMedian(temp);
			double s=median*count;
			normstack+=s*s;
		}
		normstack=sqrt(normstack);
		
		vector<double> tnorm;
		tnorm.reserve(num_gather);
		for(int j=0;j<num_gather;j++)
		{
			double sqsum=0;
			for(int k=i*wind/2;k<(i+2)*wind/2;k++)
			{
				if(!t.member[j].is_gap(k))
				{
					double v=t.member[j].s[k];
					sqsum+=v*v;
				}
			}
			tnorm.push_back(sqrt(sqsum));
		}
		
		sembl.push_back(normstack/(VectorMedian(tnorm)*num_gather));
	}
	int asampl=t.member[maxtrace].sample_number(0.0);
	int samplend=sembl.size()-1;
	for(int i=ceil(2.0*asampl/wind)-1;i<sembl.size();i++)
		if(sembl[i]<sembllimit)
		{
			samplend=i;
			break;
		}
	double tend=t.member[maxtrace].time((samplend+1)*wind/2);
	TimeWindow result(tstart,tend);
	return(result); 
}

void usage()
{
	cerr << "autoxcor db [-o outputdb -e evid -pf pfname]" << endl;
	exit(-1);
}

int main(int argc, char **argv)
{
	// This is a C++ thing.  Not required on all platforms, but
	// recommended by all the books.  
	ios::sync_with_stdio();
	// This is a standard C construct to crack the command line
	if(argc<2) usage();
	string pfin(argv[0]);
	string dbin(argv[1]);
	string dbout=dbin;
	string evidst("NULL");
	for(int i=2;i<argc;++i)
	{
		if(!strcmp(argv[i],"-V"))
			usage();
		else if(!strcmp(argv[i],"-pf"))
		{
			++i;
			if(i>=argc) usage();
			pfin = string(argv[i]);
		}
		else if(!strcmp(argv[i],"-o"))
		{
			++i;
			if(i>=argc) usage();
			dbout=string(argv[i]);
		}
		else if(!strcmp(argv[i],"-e"))
		{
			++i;
			if(i>=argc) usage();
			evidst=string(argv[i]);
		}
		else
			usage();
	}
	// Standard Antelope C construct to read a parameter file
	// Well almost standard.  const_cast is a C++ idiom required
	// due to a collision of constantness with Antelope libraries.
	Pf *pf;
        if(pfread(const_cast<char *>(pfin.c_str()),&pf)) 
	{
		cerr << "pfread error for pf file="<<pfin<<".pf"<<endl;
		exit(-1);
	}
	Metadata control(pf);
	PfStyleMetadata controlpf=pfread(pfin);
	MetadataList mdens=pfget_mdlist(pf,"Ensemble_mdlist");
	MetadataList mdtrace=pfget_mdlist(pf,"station_mdlist");
	MetadataList mdlo=pfget_mdlist(pf,"output_mdlist");
	MetadataList mdbeam=pfget_mdlist(pf,"Beam_mdlist");
	try {
		// Get set of control parameters
		string netname=control.get_string("netname");
		string phase=control.get_string("phase");
		double ts,te;
		ts=control.get_double("data_window_start");
		te=control.get_double("data_window_end");
		TimeWindow datatwin(ts,te);
		double tpad=control.get_double("data_time_pad");
		ts=control.get_double("noise_window_start");
		te=control.get_double("noise_window_end");
		TimeWindow noise_twin(ts,te);
		ts=control.get_double("signal_window_start");
		te=control.get_double("signal_window_end");
		TimeWindow signal_twin(ts,te);
		ts=control.get_double("beam_window_start");
		te=control.get_double("beam_window_end");
		TimeWindow beamstk_twin_init(ts,te);
		ts=control.get_double("beam_window_start");
		te=control.get_double("data_window_end");
		TimeWindow beam_twin(ts,te+1);
		//ts=control.get_double("robust_beam_window_start");
		//te=control.get_double("robust_beam_window_end");
		//TimeWindow beamrbst_twin(ts,te);
		double lag_cutoff=control.get_double("lag_cutoff");
		double laglimit=control.get_double("lag_limit");
		StationChannelMap stachanmap(pf);
		string schemain=control.get_string("InputAttributeMap");
		string schemaout=control.get_string("OutputAttributeMap");
		double target_dt=control.get_double("target_sample_interval");
		ResamplingDefinitions rdef(pf);
		
		string output_dir=control.get_string("output_dir");
		
		// First get all the database components assembled.
		// input data, output data
		DatascopeHandle dbh(dbin,false);
		/* Build a match handle for arrivals using the standard catalog
		view of the join of event->origin->assoc->arrival */
		AttributeMap am(schemain);  
		AttributeMap amo(schemaout);  
		DatascopeHandle dbcatalog=StandardCatalogView(dbh);
		list<string> matchkeys;
		matchkeys.push_back(string("sta"));
		matchkeys.push_back(string("evid"));
		DatascopeMatchHandle dbhm(dbcatalog,string(""),matchkeys,am);
		
		/* Build a event view to drive the process*/
		DatascopeHandle dbhv(dbh);
		dbhv.lookup("event");
		dbhv.natural_join("origin");
		dbhv.subset(string("orid==prefor"));
		
		/* Define output database*/
		DatascopeHandle dbho(dbh);
		if(dbout!=dbin)
			dbho=DatascopeHandle(dbout,false);
		dbhv.rewind();
		ThreeComponentEnsemble *rawdata=NULL;
		SeismicArray *stations=NULL;
		/* Assorted variables needed in the loop below */
		double lat,lon,depth,otime;
		long evid;
		long record;
		list<string> filter_param;
		// Define the filter
		try {
			filter_param=controlpf.get_tbl("filter");
		} catch (PfStyleMetadataError mdge) {
			cerr	<<"filter attribute not defined"
				<<" using default of DEMEAN"<<endl;
			filter_param.push_back(string("DEMEAN"));
		}
		vector<MultiStageFilter> filtlist;
		for (list<string>::iterator it=filter_param.begin(); it!=filter_param.end(); ++it)
		{
			MultiStageFilter filttemp(*it,string(";"));
			filtlist.push_back(filttemp);
		}
		
		//for now, test only!!
		//for(int i=0;i<2756;i++) ++dbhv;
		if(evidst.compare("NULL")!=0)
		{
			long idst=atol(evidst.c_str()); 
			for(int i=0;i<dbhv.number_tuples();++i,++dbhv)
			{
				if(dbhv.get_long(string("evid"))==idst)
				{
					++dbhv;
					break;
				}
			}
		}
		long currecord=dbhv.current_record();
		for(record=0;record<dbhv.number_tuples()-currecord;++record,++dbhv)
		{
			int num_gather;
			lat=dbhv.get_double(string("origin.lat"));
			lon=dbhv.get_double(string("origin.lon"));
			depth=dbhv.get_double(string("origin.depth"));
			otime=dbhv.get_double(string("origin.time"));
			evid=dbhv.get_long(string("evid"));
			// origin coordinates are degrees in the db,
			// but must be radians for hypocenter object
			lat=rad(lat);
			lon=rad(lon);
			
			Hypocenter hypo(lat,lon,depth,otime,
				string("tttaup"),string("iasp91"));
			// On the first record we need to load the station
			// geometry object
			if(record==0)
			{
				stations = new SeismicArray(
					dynamic_cast<DatabaseHandle&>(dbh),
					hypo.time,netname);
			}
			else
			{
				TimeWindow test(hypo.time,hypo.time+2000.0);
				if(!stations->GeometryIsValid(test))
				{
					delete stations;
					stations = new SeismicArray(
					dynamic_cast<DatabaseHandle&>(dbh),
					hypo.time,netname);
				}
			}
			// Read the raw data using the time window based constructor
			try{
			rawdata=array_get_data(*stations,hypo,
				phase,datatwin,tpad,dynamic_cast<DatabaseHandle&>(dbh),
				stachanmap,mdens,mdtrace,am);
			}catch (SeisppError& serr)
			{
				cout<<"bad event"<<endl;
				serr.log_error();
				continue;
			}
			if(rawdata->member.size()==0)
			{
				cout<<"bad event with problem wf data"<<endl;
				delete rawdata;
				continue;
			}
			PostEvid(rawdata,evid);
			
			num_gather=rawdata->member.size();
			for(int i=0;i<num_gather;i++)
			{
				double lat=stations->array[rawdata->member[i].get_string("sta")].lat;
				double lon=stations->array[rawdata->member[i].get_string("sta")].lon;
				double elev=stations->array[rawdata->member[i].get_string("sta")].elev;
				rawdata->member[i].put("lat",lat);
				rawdata->member[i].put("lon",lon);
				rawdata->member[i].put("elev",elev);
			}
			
			vector<double> beam_vector;
			const string arrival_keyword("arrival.time");
			list<long> data_with_arrivals;
			data_with_arrivals=LoadArrivalTimes(rawdata->member,dbhm,arrival_keyword);
			list<long>::iterator index;
			TimeSeriesEnsemble beam_ensemble;
			for(index=data_with_arrivals.begin();index!=data_with_arrivals.end();++index)
			{
				ThreeComponentSeismogram d=rawdata->member[*index];
				if(d.live)
				{
					try{
						d.rotate_to_standard();
						if(control.get_bool("apply_free_surface_transform"))
						    ApplyFST(d,hypo);
					}catch (SeisppError& serr){
						cout<<"Error when rotating record from event: "
							<<evid<<" station: "<<d.get_string("sta")<<endl;
						serr.log_error();
						continue;
					}catch(...){
						cout<<"Unexpected error when rotating record from event: "
							<<evid<<" station: "<<d.get_string("sta")<<endl;
						continue;
					}
					ThreeComponentSeismogram d3c(d);
					shared_ptr<TimeSeries> beam;
					shared_ptr<TimeSeries> beampre;
					beam=shared_ptr<TimeSeries>(ExtractComponent(d,2));
					if(beam->ns<=0)
					{
						cout<<"Number of samples is less than 1 for event: "
							<<evid<<" station: "<<d.get_string("sta")<<endl;
						//beam.release();
						continue;
					}
					if( (abs( (beam->dt)-target_dt)/target_dt) > 0.01)
					{
					    *beam=ResampleTimeSeries(*beam,rdef,target_dt,false);
					}
					double atime=beam->get_double(arrival_keyword);
					//cout.precision(14);
					//cout<<"**"<<endl<<d.get_string("sta")<<"	"<<atime<<"**"<<endl;
					beampre=ArrivalTimeReference(*beam,arrival_keyword,beam_twin);
                    
					if(beampre->live)
					{
						beampre->put(string("reftime"),atime);
						beampre->put(moveout_keyword,0.0);
						//beam->put(snr_keyword,SNR_rms(*beam,signal_twin,noise_twin));
						beam_ensemble.member.push_back(*beampre);
					}
					//beam.release();
                    //beampre.release();
				}
			}
			delete rawdata;
			int beamensemblesize=beam_ensemble.member.size();
			if(beamensemblesize<10)
			{
				cout<<"skipped event_id: "<<evid<<endl;
				continue;
			}
			
			MultichannelCorrelator xcor;
			vector<MultichannelCorrelator> xcor_v;
			int reference_member;
			TimeWindow beamrbst_twin, beamstk_twin;
			for(vector<MultiStageFilter>::iterator it=filtlist.begin(); it!=filtlist.end(); ++it)
			{
				TimeSeriesEnsemble be=beam_ensemble;
				// Filter the raw data.
				it->apply(be);
				
				for(int i=0;i<beamensemblesize;i++)
					be.member[i].put(snr_keyword,SNR_rms(be.member[i],signal_twin,noise_twin));
			
				beamrbst_twin=SemblanceSearch(be,control);
				beamstk_twin=beamstk_twin_init;
				beamstk_twin.end+=beamrbst_twin.end;
				if(beamstk_twin.end>beam_twin.end)
					beamstk_twin.end=beam_twin.end;
				reference_member=0;
				double maxsnr=be.member[0].get_double(snr_keyword);
				for(int i=0;i<beamensemblesize;i++)
					if(be.member[i].get_double(snr_keyword)>maxsnr)
					{
						maxsnr=be.member[i].get_double(snr_keyword);
						reference_member=i;
					}
                try{
				MultichannelCorrelator xcortemp(be, RobustStack, beamstk_twin, beamrbst_twin, lag_cutoff,
					 RobustSNR, NULL, reference_member);
				xcor_v.push_back(xcortemp);
                }catch(...){
                    cout<<"Unexpected error doing MultichannelCorrelator from event: "
                    <<evid<<endl;
                    continue;
                }
			}
			int bestindex=0;;
			double bestlag=VectorMean(xcor_v[0].lag);
			for(int i=0;i<xcor_v.size();i++)
			{
				double templag=VectorMean(xcor_v[i].lag);
				if(bestlag>templag)
				{
					bestlag=templag;
					bestindex=i;
				}
			}
			if(bestlag>laglimit)
				cout<<"Warning: the average lag for event "<<evid<<" is "<<bestlag<<endl;
				
			filtlist[bestindex].apply(beam_ensemble);
			for(int i=0;i<beamensemblesize;i++)
				beam_ensemble.member[i].put(snr_keyword,SNR_rms(beam_ensemble.member[i],signal_twin,noise_twin));
			beamrbst_twin=SemblanceSearch(beam_ensemble,control);
			beamstk_twin=beamstk_twin_init;
			beamstk_twin.end+=beamrbst_twin.end;
			if(beamstk_twin.end>beam_twin.end)
				beamstk_twin.end=beam_twin.end;
			reference_member=0;
			double maxsnr=beam_ensemble.member[0].get_double(snr_keyword);
			for(int i=0;i<beamensemblesize;i++)
				if(beam_ensemble.member[i].get_double(snr_keyword)>maxsnr)
				{
					maxsnr=beam_ensemble.member[i].get_double(snr_keyword);
					reference_member=i;
				}
			xcor=xcor_v[bestindex];
			
			for(int i=0;i<beamensemblesize;i++)
			{
				double atime=beam_ensemble.member[i].get_double("reftime");
				beam_ensemble.member[i].rtoa(atime);
				beam_ensemble.member[i].put(arrival_keyword,atime+xcor.lag[i]);
				//beam_ensemble.member[i].ator(beam_ensemble.member[i].get_double(arrival_keyword));
			}
			
			TimeSeries beam=xcor.ArrayBeam();
			
			long xcorlive=0;
			for(int i=0;i<beamensemblesize;i++)
			{
				if(xcor.xcor.member[i].live)
					xcorlive++;
			}
			if(xcorlive==0)
				continue;
			
			long fold=xcorlive;
			string filter_beam=beam.get_string("filter_spec");
			stringstream ss;
			ss << evid;
			string beam_dfile = ss.str();
			beam.put("wfprocess.dir",output_dir);
			beam.put("dir",output_dir);
			beam.put("wfprocess.dfile","beam"+beam_dfile);
			beam.put("wfprocess.endtime",beam.endtime());
			
			DatascopeHandle dbxcorbeam(dbho);
			dbxcorbeam.lookup(string("xcorbeam"));
			DatascopeHandle dbevlink(dbho);
			dbevlink.lookup(string("evlink"));
			DatascopeHandle dbwfprocess(dbho);
			dbwfprocess.lookup(string("wfprocess"));
			DatascopeHandle dbarrival(dbho);
			dbarrival.lookup(string("arrival"));
			DatascopeHandle dbxcorarrival(dbho);
			dbxcorarrival.lookup(string("xcorarrival"));
			DatascopeHandle dbwfdisc(dbho);
			dbwfdisc.lookup(string("wfdisc"));
			
			long recordo=dbsave(beam,dbwfprocess.db,string("wfprocess"),mdbeam,amo);
			
			beam.put("sta",beam_ensemble.member[reference_member].get_string("sta"));
			//beam.put("chan",beam_ensemble.member[reference_member].get_string("chan"));
			beam.put(string("chan"),string("Z"));
			beam.put("dfile","beam_wfdisc"+beam_dfile);
			dbsave(beam,dbwfdisc.db,string("wfdisc"),mdlo,amo);
			
			dbwfprocess.db.record=recordo;
			long pwfid;
			dbgetv(dbwfprocess.db,0,"pwfid",&pwfid,NULL);
			double beam_amplitude=beam.get_double(beam_rms_key);
			recordo=dbaddnull(dbxcorbeam.db);
            if(recordo==dbINVALID) throw SeisppError(string("dbsave:  dbaddnull failed"));
            dbxcorbeam.db.record=recordo;
            dbputv(dbxcorbeam.db,0,"netname",netname.c_str(),
				"chan","BHZ",
				"pchan","BZ",
				"phase",phase.c_str(),
				"pwfid",pwfid,
				"filter",filter_beam.c_str(),
				"robustt0",beamrbst_twin.start,
				"robusttwin",beamrbst_twin.length(),
				"fold",fold,
				"amp",beam_amplitude,
					NULL);
			//if(recordo<0)
			//	cerr << "save_results(Warning):  problems adding to xcorbeam table"<<endl;
			recordo=dbaddnull(dbevlink.db);
            if(recordo==dbINVALID) throw SeisppError(string("dbsave:  dbaddnull failed"));
            dbevlink.db.record=recordo;
            dbputv(dbevlink.db,0,"evid",evid,"pwfid",pwfid,NULL);
			
			int counti=0;
			for(vector<TimeSeries>::iterator d=beam_ensemble.member.begin();d!=beam_ensemble.member.end();++d,++counti)
			{
				if(!xcor.xcor.member[counti].live)
					continue;
                recordo=dbaddnull(dbarrival.db);
                if(recordo==dbINVALID) throw SeisppError(string("dbsave:  dbaddnull failed"));
                dbarrival.db.record=recordo;
				dbputv(dbarrival.db,0,"sta",d->get_string("sta").c_str(),
					"time",d->get_double(arrival_keyword),
					"arid",d->get_long(arrival_keyword+".arid"),
					"chan","BHZ",
					"iphase",phase.c_str(),
					"snr",d->get_double(snr_keyword),
					"auth","autoxcor",
                       NULL);
                recordo=dbaddnull(dbxcorarrival.db);
                if(recordo==dbINVALID) throw SeisppError(string("dbsave:  dbaddnull failed"));
                dbxcorarrival.db.record=recordo;
				dbputv(dbxcorarrival.db,0,"sta",d->get_string("sta").c_str(),
					"chan","BHZ",
					"phase",phase.c_str(),
					"pwfid",pwfid,
					"filter",filter_beam.c_str(),
					"algorithm","autoxcor",
					"pchan","BZ",
					"time",d->get_double(arrival_keyword),
					"twin",beamstk_twin.length(),
					"samprate",1.0/d->dt,
					"stackwgt",xcor.weight[counti],
					"xcorpeak",xcor.peakxcor[counti],
					"coherence",xcor.amplitude_static[counti],
					NULL);
				d->put(string("chan"),string("BHZ"));
				d->put("dir",output_dir);
				d->put("dfile","wf"+beam_dfile);
				//int rnum=dbsave(*d,dbwfdisc.db,string("wfdisc"),mdlo,amo);
			}
		}
		delete stations;
	}
	catch (SeisppError& serr)
	{
		serr.log_error();
	}
	catch (...)
	{
		cerr << "Something threw an unexpected exception"<<endl;
	}
}

