#include<string>
#include "stock.h"
#include "elog.h"
#include "db.h"
#include "pf.h"
#include "glputil.h"
#include "seispp.h"
using namespace SEISPP;
#include "resample.h"
void usage()
{
	cerr<<"dbresample dbin dbout [-pf pffile -notrim]"<<endl;
	exit(-1);
}

int main(int argc, char **argv)
{
	Dbptr db;
	Pf *pf;
	string pffile("dbresample");
	bool trim=true;
	string chan;
	string dfile_name;

	ios::sync_with_stdio();

	// Standard argument parsing in C++ form

	elog_init(argc,argv);
	if(argc<3)
		usage();
	try
		{
	
		string dbname(argv[1]);
		string dboname(argv[2]);
		string tag("dbprocess_commands");
		for(int i=3;i<argc;++i)
		{
			string test;
			test=string(argv[i]);
			if(test=="-pf")
			{
				++i;
				pffile=string(argv[i]);
			}
			else if(test=="-notrim")
				trim = false;
			else
			{
				cerr<<"Illegal argument = "<<argv[i]<<endl;
				usage();
			}
			if(i>=argc) usage();
		}	
		if(pfread(const_cast<char *>(pffile.c_str()),&pf))
			die(0,"Failure reading parameter file %s\n",pffile.c_str());
		
		// local variables used below
		double dtout;
		dtout = 1.0/pfget_double(pf,"output_sample_rate");
		char *chan_code;
		chan_code=pfget_string(pf,"output_channel_code"); 
		cout << "Output channels will be coded as "<<chan_code<<endl;
		if(strlen(chan_code)!=1)
		{
			cerr << "Warning:  input channel code is more than one character long"
				<< "Only the first character "
				<< chan_code[0] << " will be used"<<endl;
		}
		// This object defines mapping from external to internal namespace
		Attribute_Map am(pf,string("Attribute_Map"));
		// This defines the list of internal names actually extracted from db
		Metadata_list md_to_input=pfget_mdlist(pf,
			"input_metadata_list");
		// This is the list saved
		Metadata_list md_to_output=pfget_mdlist(pf,
			"output_metadata_list");
		if(dbopen(const_cast<char *>(dbname.c_str()),"r",&db))
			die(0,"dbopen failed on database %s",dbname.c_str());

		// Input and output database handles
		Datascope_Handle dbhi(db,pf,tag);
		Datascope_Handle dbho(dboname,false);
		// Builds the object that defines how decimation is
		// and resampling is to be done.
		Resampling_Definitions rsampdef(pf);
		dbhi.rewind();
		for(int i=0;i<dbhi.number_tuples();++i,++dbhi)
		{
			Time_Series *tin;
			Time_Series traceout;
			string table("wfdisc");
			tin = new Time_Series(dynamic_cast<Database_Handle&>(dbhi),
				md_to_input,am);   
			traceout = Resample_Time_Series(*tin,rsampdef,dtout,trim);
			// Simple method to change channel code
			// only does right thing for SEED chan codes
			chan=traceout.get_string("chan");
			chan[0]=chan_code[0];
			traceout.put_metadata("chan",chan);
			// a crude way to alter files to preserve original structure
			// append the string ".resampled"
			dfile_name = traceout.get_string("file");
			dfile_name = dfile_name + string(".resampled");
			traceout.put_metadata("file",dfile_name);
			dbsave(traceout,dbho.db,table,md_to_output,am);
			delete tin;
		}
	}
	// for now we exit on any exception.  Some errors may 
	// need to be caught and handled without exits
	catch (Metadata_error spe)
	{
		spe.log_error();
		exit(-1);
	}
	catch (seispp_error se)
	{
		se.log_error();
		exit(-1);
	}
}
