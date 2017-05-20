#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "filter++.h"
using namespace std;   
using namespace SEISPP; 
void usage()
{
    cerr << "filter filter_specification < in > out [-v --help -binary]"
        <<endl
        << "Apply BRTT filter defined by filter_specification to all data"<<endl
        << " -v - be verbose - here that means echo the filter definition"<<endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    string filter_spec(argv[1]);
    if(filter_spec=="--help") usage();
    bool binary_data(false);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-binary")
        {
            binary_data=true;
        }
        else if(sarg=="-v")
            SEISPP_verbose=true;
        else
            usage();
    }
    try{
        if(SEISPP_verbose) cerr<<"filter program: filtering data with "
            << filter_spec<<endl;
        TimeInvariantFilter filt(filter_spec);
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentEnsemble d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            FilterEnsemble(d,filt);
            out->write(d);
            ++n;
        }
        if(SEISPP_verbose)
            cerr << "filter: Total number of ensembles processed ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

