/* This is an template file that can be used to allow processing of
data through matlab using the seispp library as a front end to handle
data issues.  Not all the concepts used in the seispp library can 
translate to matlab, but the idea should be to do all data prep 
possible using seispp and then pass simple matrices to matlab.
The reader should consult the documentation on SEISPP found under
http://seismo.geology.indiana.edu/~pavlis/software.html

This template file is for a ThreeComponentEnsemble.
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
	cerr << "SingleTrace3C db [-pf pfname]" << endl;
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
	// this defines what attributes are to be loaded for each 
	// ThreeComponentSeismogram  
	MetadataList mdlin=pfget_mdlist(pf,"Station_mdlist");
	try {

		// This example uses the C++ Datascope API.
		// This is the cleanest path for a 3c object because
		// Antelope does not treat 3c data as a different beast
		// in the same way SEISPP does
		DatascopeHandle dbh(string(argv[1]),true);
		dbh.lookup("wfprocess");
		dbh.rewind();
		int record;
		// Here we are using a nonstandard schema.  The reason
		// is that this allows a simple scheme for reading
		// 3C data not available in Antelope.  This is the 
		// approach used in the plane wave migration algorithm
		// and this is the one used here.
		const string schema("pwmig1.1");
		AttributeMap am(schema);
		MatlabProcessor mp(stdout);
		// Loop through the waveform table plotting each seismogram
		// This creates a movie with a 1s delay between plot displays.
		for(record=0;record<dbh.number_tuples();++record,++dbh)
		{
			ThreeComponentSeismogram d(dbh,mdlin,am);
			mp.load(d,string("d"));
			// Replace this with your own commands to do something
			// else
			mp.process(string("subplot(3,1,1),plot(d(1,:));"));
			mp.process(string("subplot(3,1,2),plot(d(2,:));"));
			mp.process(string("subplot(3,1,3),plot(d(3,:));"));
			mp.process(string("pause(1);"));
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
