#include <fstream>
#include <iostream>
#include "dbpp.h"
#include "VectorStatistics.h"
using namespace std;
using namespace SEISPP;
void usage()
{
cerr << "xcoravg dbarr [-v -pf pffile -u]"<<endl
	<<" dbarr - db with arrival and dbxcor xsaa table"<<endl
	<<"    -u will replace update arrivals in dbarr (default is a dry run)"
	<<endl;
exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
	if(argc<2) usage();
	string dbarr(argv[1]);
	string pffile("xcoravg");
	bool update_enabled(false);
	int i;
	for(i=2;i<argc;++i)
	{
		string sarg(argv[i]);
		if(sarg=="-v")
			SEISPP_verbose=true;
                else if(sarg=="-pf")
                {
                        ++i;
                        if(i>=argc)usage();
                        pffile=string(argv[i]);
                }
		else if(sarg=="-u")
			update_enabled=true;
		else
			usage();
	}
        Pf *pf;
        if(pfread((char *)(pffile.c_str()),&pf))
        {
                cerr << "pfread failed for file = "<<pffile<<endl;
                usage();
        }
	try{
		Metadata control(pf);
		bool set_deltim=control.get_bool("set_deltim_in_arrival");
		double xcordeltim;
		if(set_deltim)
			xcordeltim=control.get_double("arrival_deltim");
		/* This is posted to arrival.  Get it over with right away as the
		effort is trivial */
		char username[20];
		my_username(username);
		string auth=string("xcoravg:")+username;
		DatascopeHandle dbh(dbarr,false);
		DatascopeHandle dbhxcat(dbh);
		dbhxcat.lookup("xsaa");
		list<string> xcatskeys,xcatgrpkeys;
		xcatskeys.push_back("phase");
		xcatskeys.push_back("sta");
		xcatskeys.push_back("evid");
		xcatgrpkeys=xcatskeys;
		xcatskeys.push_back("gridid");
		dbhxcat.sort(xcatskeys);
		if(SEISPP_verbose) 
			cout << "Working view built from xsaa number of rows="
				<< dbhxcat.number_tuples()<<endl;
		dbhxcat.group(xcatgrpkeys);
		if(SEISPP_verbose) 
			cout << "Number of phase:sta:evid groups="
				<< dbhxcat.number_tuples()<<endl;
		/* Build a match handle against dbh2. */
		AttributeMap am("css3.0");
		/* Now build match handle on arrival for db1*/
		list<string> matchkeys;
		matchkeys.push_back("sta");
		/* This is a bit problematic.  arrival uses iphase and we have
		put phase as an attribute in xsaa */
		matchkeys.push_back("iphase");
		matchkeys.push_back("evid");
		/* We need to use a join of arrival and assoc because 
		arid is not in the xsaa table.  This would be simpler
		if that were the case, but elected to do this to avoid
		maintaining arid as a unique key. This is too often 
		abused by me and others and is subject to errors. 
		Dark side is it complicates this code. */
		dbh.lookup("event");
		dbh.natural_join("origin");
		dbh.subset(string("orid==prefor"));
		dbh.natural_join("assoc");
		dbh.natural_join("arrival");
		if(dbh.number_tuples()<=0) 
		{
			cerr << "event->origin->assoc->origin view is empty."
				<<"  Verify database = "<< dbarr<<endl;
			usage();
		}
		/* Null string is a signal to this constructor to use
		dbh.db as the table to match against. */
		DatascopeMatchHandle arrivalhandle(dbh,string(""),
			matchkeys,am);
		/* Now loop through the group view computing statistics for
		each arrival.  The median of the group is used to set the
		new arrival time estimate */
		dbhxcat.rewind();
		int i;
		int ierr;
		/* This holds the list of matching record numbers used to find
		the right row in arrival */
		list<long> matchlist;
		vector<double> arrival_times;
		cout << "evid sta phase db_arrival_time median_dt mean_dt interquartile mad range ndgf nskipped" <<endl;
		/* Hold keys extracted with dbgetv in loop below for matching to db2 */
		int gridid, evid;
		char sta[20],phase[10];
		double oldatime,newatime;
		for(i=0;i<dbhxcat.number_tuples();++i,++dbhxcat)
		{
			Dbptr dbarrival;  // holds Datascope Dbptr fetched from view below
			Dbptr dbview;  // used for looping through group view 
			DBBundle grp=dbhxcat.get_range();
			/* correct because end is +1 as per dbget_range */
			int narr=grp.end_record-grp.start_record;
			dbview=grp.parent;
			arrival_times.clear();
			int nskipped;
			for(dbview.record=grp.start_record,nskipped=0;
				dbview.record<grp.end_record;++dbview.record)
			{
				double atime;
				ierr=dbgetv(dbview,0,"time",&atime,
					"sta",sta,"phase",phase,"evid",&evid,"gridid",&gridid,0);
				if(ierr==dbINVALID)
				{
					cerr << "Warning:  dbgetv error.  Skipping record number "
						<< dbview.record<<endl;
				}
				else
				{
					arrival_times.push_back(atime);
				}
			}
			narr=arrival_times.size();
			if(narr<=0)
			{
				cerr << "Warning:  no data survived for sta="<<sta
					<< " phase="<<phase
					<< " and event="<<evid<<endl;
			}
			else
			{
				string stastr=dbhxcat.get_string("sta");
				string phasestr=dbhxcat.get_string("phase");
				evid=dbhxcat.get_int("evid");
				Metadata finder;
				finder.put("sta",stastr);
				finder.put("evid",evid);
				finder.put("iphase",phasestr);
				matchlist=arrivalhandle.find(finder,false);
				if(matchlist.size()<=0)
					cerr << "dbmatches failed for evid:sta:phase="
						<<evid<<" : "
						<<stastr<<" : "
						<<phasestr<<endl;
				else
				{
					if(matchlist.size()>1)
						cerr << "Warning:  evid:sta:phase="
                                                <<evid<<" : "
                                                <<sta<<" : "
                                                <<phase<<endl
						<<"dbmatches found "<<matchlist.size()
						<< " matching rows.  Using first row of list."
						<<endl;
					dbview=dbh.db;
					dbview.record=*(matchlist.begin());
					ierr=dbgetv(dbview,0,"arrival",&dbarrival,0);
					if(ierr==dbINVALID)
						cerr << "Warning:  evid:sta:phase="
                                                <<evid<<" : "
                                                <<sta<<" : "
                                                <<phase<<endl
						<< "Error fetching Dbptr for arrival table.  "
						<< "Results for this arrival will not be in output database"
						<<endl;
					else
					{
					    dbgetv(dbarrival,0,"time",&oldatime,0);
					    if(narr>1)
					    {
						VectorStatistics<double> atstat(arrival_times);
						newatime=atstat.median();
						
						cout << evid <<" "
							<< stastr<<" "
							<< phasestr <<" "
							<< strtime(oldatime) << " "
							<< oldatime-newatime << " "
							<< oldatime-atstat.mean() <<" "
							<< atstat.interquartile() <<" "
							<< atstat.mad(newatime) <<" "
							<< atstat.range() <<" "
							<< narr-1 <<" "
							<< nskipped<<endl;
					    }
					    else
					    {
						newatime=arrival_times[0];
						cout << evid <<" "
							<< stastr <<" "
							<< phasestr <<" "
							<< strtime(oldatime) << " "
							<< oldatime-arrival_times[0] << " "
							<< oldatime-arrival_times[0] << " "
							<< "0.0 "
							<< "0.0 "
							<< "0.0 "
							<< "0 " 
							<< nskipped <<endl;
					    }
					    if(update_enabled)
					    {
						dbputv(dbarrival,0,
							"time",newatime,
							"auth",auth.c_str(),0);
						if(set_deltim)
							dbputv(dbarrival,0,"deltim",xcordeltim,0);
					    }
					}
				}
			}
		}
	}
	catch(SeisppError serr)
	{
		serr.log_error();
	}
}

