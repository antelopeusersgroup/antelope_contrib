/* This is an template file that can be used to allow processing of
data through matlab using the seispp library as a front end to handle
data issues.  Not all the concepts used in the seispp library can 
translate to matlab, but the idea should be to do all data prep 
possible using seispp and then pass simple matrices to matlab.
The reader should consult the documentation on SEISPP found under
http://seismo.geology.indiana.edu/~pavlis/software.html

This template file is for a ThreeComponentEnsemble.  The cryptic
name tcee is short for Three Component Ensemble Example.  
*/

#include "Metadata.h"
#include "dbpp.h"
#include "MatlabProcessor.h"
using namespace std;
using namespace SEISPP;
// This template assumes a database as the only required argument.
// pf is a parameter file following the usual antelope convention.
void usage()
{
	cerr << "tcee db [-pf pfname]" << endl;
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
	// These define attributes to be extracted from the 
	// database for each trace and for the overall ensemble.
	// Different application WILL likely want to change this list.
	MetadataList mdlin=pfget_mdlist(pf,"Station_mdlist");
	MetadataList mdens=pfget_mdlist(pf,"Ensemble_mdlist");
	try {
		// This illustrates the C++ API to Datascope.  
		// This could be done with the C API, but a generic
		// handle is required below so we use this construction
		DatascopeHandle dbh(string(argv[1]),true);
		// The database view you desire can be customized by
		//editing the dbprocess list in the parameter file.
		// Be aware, however, that the result has to be grouped
		// properly to define an ensemble.  See sample
		DatascopeHandle dbhv(dbh.db,pf,"dbprocess_commands");
		dbhv.rewind();
		dbh.rewind();
		int record;
		// For this example we use the pwmig1.1 schema.  
		// Others may want to choose css3.0, but there are issues
		// with how to handle 3C objects in css3.0
		const string schema("pwmig1.1");
		AttributeMap am(schema);
		// This launches matlab and sets up the communication channels
		MatlabProcessor mp(stdout);
		// A 3C ensemble is loaded as three matrices with these names
		string chans[3]={"x1","x2","x3"};
		ThreeComponentEnsemble *pwdata;
		// This demo loops over multiple ensembles.
		for(record=0;record<dbhv.number_tuples();++record,++dbh)
		{
			// this reads the data for one ensemble
			pwdata=new ThreeComponentEnsemble(dynamic_cast<DatabaseHandle&>(dbhv),
				mdlin,mdens,am);
			// load the data into matlab
			mp.load(*pwdata,chans);
			// process these data.  Substitute alternative commands
			// here to do something else.
			mp.process(string("imagesc(x1);"));
			mp.process(string("figure; imagesc(x2);"));
			mp.process(string("figure; imagesc(x3);"));
			// In this demo we pause with this mechanism.
			// Obviously undesirable for data processing that is
			// not interactive.
			mp.run_interactive();
		}
	}
	catch (SeisppError serr)
	{
		serr.log_error();
	}
	catch (...)
	{
		cerr << "Something threw an unexpected exception"<<endl;
	}
}
