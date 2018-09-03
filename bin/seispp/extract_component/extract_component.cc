#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;
using namespace SEISPP;

void usage()
{
    cerr << "extract_component n [-text --help]<< infile > outfile"<<endl
        << "Extract component n (must be 0, 1, or 2) from input 3C ensemble"
        <<endl
        << "Output is boost serialized TimeSeriesEnsemble object"<<endl
        << " -text - switch to text input and output (default is binary)"<<end;
    exit(-1);
}


bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{

    int i;
    if(argc!=2) usage();
    int outchan=atoi(argv[1]);
    if( (outchan<0) || (outchan>2) )
    {
      cerr << "extract_component:  illegal output channel of "
        <<outchan<<" specified"<<endl;
      usage();
    }
    bool binary_data(true);
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
            usage();
        else if(sarg=="-text")
            binary_data=false;
        else
            usage();
    }
    try{
        if(SEISPP_verbose)
        {
          cerr << "extract_component:  extracting component number "
            << outchan<<endl;
        }
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> ia;
        if(binary_data)
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<TimeSeriesEnsemble>> oa;
        if(binary_data)
        {
          oa=shared_ptr<StreamObjectWriter<TimeSeriesEnsemble>>
             (new StreamObjectWriter<TimeSeriesEnsemble>('b'));
        }
        else
        {
          oa=shared_ptr<StreamObjectWriter<TimeSeriesEnsemble>>
             (new StreamObjectWriter<TimeSeriesEnsemble>);
        }
        ThreeComponentEnsemble d;
        while(!ia->eof())
        {
            d=ia->read();
            shared_ptr<TimeSeriesEnsemble> dscalar=ExtractComponent(d,outchan);
            oa->write(*dscalar);
        }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
