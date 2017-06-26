#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include "stock.h"
#include "seispp.h"
#include "ensemble.h"
#include "AttributeCrossReference.h"
#include "dbpp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "db2seispp db [-pf pffile --help -binary -v] > outfile"
        <<endl
        << "Converts a database of segmented waveforms produced "
        << "by extract_events to a serial file of ThreeComponentSeismogram objects"<<endl
        << " -pf - use alternative parameter file instead of default db2seispp"
        << endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"<<endl
        << " -v - be more verbose"
        <<endl;
    exit(-1);
}
AttributeCrossReference pfload_ACR(Pf *pf,string tag)
{
  try{
    string acrstr=pftbl2string(pf,tag.c_str());
    return (AttributeCrossReference(acrstr));
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc<2) usage();
    string dbname(argv[1]);
    if(dbname=="--help") usage();
    string pffile("db2seispp");
    bool binary_mode(false);
    string otype("ThreeComponentSeismogram");
    bool write_logfile(false);
    string logfile("");
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-binary")
        {
            binary_mode=true;
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    Pf *pf;
    if(pfread(const_cast<char*>(pffile.c_str()),&pf))
    {
      cerr << "pfread failed for parameter file="<<pffile<<endl;
      usage();
    }
    try{
      const string dbprocesstag("dbprocess_commands");
      const string mdltag("attributes_to_load");
      /* The AttributeCrossReference was designed for a somewhat different purpose
      but it will work well here.   It allows us to map the css3.0 attribute
      names to anything we want in this context.  In a more coherent system
      this should be globally available.   Worth considering as a future
      development*/
      MetadataList attributes_to_copy=pfget_mdlist(pf,mdltag);
      AttributeMap am(string("css3.0"));
      DatascopeHandle dbh0(dbname,true);
      DatascopeHandle dbh(dbh0,pf,dbprocesstag);
      /* This could be put into the dbprocess commands, but since it is
      essential to guarantee the success of this algorithm best to
      make sure it appears.  This will be redundant if the dbprocess commands
      contain the same subset condition. */
      if(SEISPP_verbose)
        cerr << "db2seispp:  number of tuples in working view created by dbprocess="
          << dbh.number_tuples()<<endl;
      string fixed_wfprocess_subset("(wfprocess.datatype=~/3c/) || (wfprocess.datatype=~/c3/)");
      dbh.subset(fixed_wfprocess_subset);
      if(SEISPP_verbose)
        cerr << "db2seispp:  number of tuples in working view after wfprocess 3c subset="
          << dbh.number_tuples()<<endl;
      /* We will write serialized 3c seismograms to stdout.  */
      shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>> out;
      if(binary_mode)
      {
        out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>('b'));
      }
      else
      {
        out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>);
      }
      /* Now we work through the entire view. All the hard work here is done
      in the 3c seismogram constructor.*/
      long irec,nrec,nseis(0);
      nrec=dbh.number_tuples();
      dbh.rewind();   // Probably unnecessary but useful for clarity
      for(irec=0;irec<nrec;++irec,++dbh)
      {
        try{
          ThreeComponentSeismogram d(dbh,attributes_to_copy,am);
          out->write(d);
          ++nseis;
        }catch(SeisppError& serr)
        {
          cerr << "Error in processing database row="<<irec<<endl;
          serr.log_error();
          cerr << "Data for that seismogram may not have been written to output - blundering on"
            << endl;
        }
      }
    }
    catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
