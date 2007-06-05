#include "XcorProcessingEngine.h"
using namespace std;
using namespace SEISPP;
void usage()
{
	cerr << "rtxcor dbtmp [-pf pffile]"<<endl;
	exit(-1);
}

/*
design issues:
1.  will read from a temp db from new orbevproc.  this db
has data for one and only one event, norminally produced by
dbgenloc.

2.  program will need a wfdb in the parameter file used for
waveform database.

3.  Program will read origin from the tmp db as starting hypocenter

4.  Computed arrivals will be posted to tmp db with a new orid.

5.  will get time shift for picks to correct for origin errors 
by matching arrival times from detectors and computing average
difference from lags computed by cross correlation.
Decided against this.  Are driving this with output of dbgenloc
so do not need to worry about the bias issue.
*/

/* Finds the reference trace as the index position by a
simple linear search for maximum signal to noise ratio. 
May eventually want a more complex recipe.*/
int choose_reference(DatascopeHandle& dbh,
		TimeSeriesEnsemble *d)
{
	string maxsta,sta;
	double maxsnr,snr;
	int i;
	try {
	dbh.rewind();
	for(i=0;i<dbh.number_tuples();++i,++dbh)
	{
		if(i==0)
		{
			maxsnr=dbh.get_double("snr");
			maxsta=dbh.get_string("sta");
		}
		else
		{
			snr=dbh.get_double("snr");
			sta=dbh.get_string("sta");
			if(snr>maxsnr)
			{
				maxsta=sta;
				maxsnr=snr;
			}
		}
	}
	} catch (SeisppDberror se)
	{
		se.log_error();
		cerr << "rtxcor defaulting to first trace as reference"<<endl;
		return(0);
	}
	// Now search for this station in the ensemble
	for(i=0;i<d->member.size();++i)
	{
		if(d->member[i].live)
		{
			sta=d->member[i].get_string("sta");
			if(sta==maxsta)  return(i);
		}
	}
	cerr << "rtxcor data mismatch in search for reference trace"<<endl
		<<"arrival maximum snr station="<<sta<<" has no trace data  "
		<<"Default to 0"<< endl;
	return(0);
}
	

int main(int argc, char **argv)
{
	int i;
	if(argc<2) usage();
	string pfname("rtxcor");
	string dbtmp(argv[1]);
	for(i=2;i<argc;++i)
	{
		string argtest(argv[i]);
		if(argtest==string("-pf"))
		{
			++i;
			pfname=string(argv[i]);
		}
		else
			usage();
	}
	Pf *pf;
	if(pfread(const_cast<char *>(pfname.c_str()),&pf))
	{
		cerr << "Error reading pf file = "<<pfname<<endl;
		exit(-1);
	}
	try {
		AttributeMap("css3.0");
		Metadata global_md(pf);
		AnalysisSetting asetting(global_md);
		// Window parameters
		double ts,te;
		ts=global_md.get_double("beam_window_start_time");
		te=global_md.get_double("beam_window_end_time");
cout << "Beam window="<<ts<<"-"<<te<<endl;
		TimeWindow beamtw(ts,te);
		asetting.set_beam_tw(beamtw);
		ts=global_md.get_double("robust_window_start_time");
		te=global_md.get_double("robust_window_end_time");
		TimeWindow robusttw(ts,te);
cout << "Robust window="<<ts<<"-"<<te<<endl;
		asetting.set_robust_tw(robusttw);
		string wfdname(global_md.get_string("waveform_database"));
		// This should eventually become dbtmp, but xcor1.2 is
		// nonstandard at this point
		string resultdb(global_md.get_string("output_database"));
		string method(global_md.get_string("method"));
		string model(global_md.get_string("model"));
		XcorProcessingEngine xpe(pf,asetting,wfdname,resultdb);
		DatascopeHandle dbh(dbtmp,false);
		// This assumes there is one and only one origin
		// in the tmp database that will drive this
		dbh.natural_join(string("event"),string("origin"));
		dbh.rewind();
		int orid,evid;
		double lat,lon,otime,depth;
		orid=dbh.get_int("orid");
		evid=dbh.get_int("evid");
		Hypocenter h(dbh.get_double("lat"),
				dbh.get_double("lon"),
				dbh.get_double("depth"),
				dbh.get_double("time"),
				method,
				model);
		// The current method has a default filter applied
		// immediately after reading.  Here that is the filter.
		// To extend to multiple filters will require 
		// a different approach
		xpe.load_data(h);
		// Now we need to select a reference trace.  We use
		// snr from the tmp database. xpe wants the index
		// position of this trace that we have to search for.
		TimeSeriesEnsemble *ens=xpe.get_waveforms_gui();
cout << "data loaded.  Number of members in this ensemble="
	<<ens->member.size()<<endl;
		dbh.natural_join(string("assoc"));
		dbh.natural_join(string("arrival"));
		int reftrace=choose_reference(dbh,ens);
		asetting.set_ref_trace(reftrace);
cout << "Using reference trace ="<<reftrace<<endl;
		xpe.change_analysis_setting(asetting);
cout << "Running correlator"<<endl;
		MultichannelCorrelator *mce=xpe.analyze();
		orid=dbnextid(dbh.db,"orid");
cout << "Saving results tagged with orid="<<orid<<endl;
		xpe.save_results(evid,orid);
	}
	catch(MetadataError mde)
	{
		mde.log_error();
	}
	catch(SeisppError spe)
	{
		spe.log_error();
	}
}
