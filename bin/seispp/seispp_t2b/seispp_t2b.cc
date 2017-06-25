#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP; 
void usage()
{
    cerr << "seispp_t2b [--help] < in > out"
        <<endl
        << "Simple filter to convert text format ThreeComponentEnsemble files to binary format"<<endl
        << "Warning:  binary files should not be moved between machines"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-v")
            SEISPP_verbose=true;
        else
            usage();
    }
    try{
        StreamObjectReader<ThreeComponentEnsemble> inp('t');
        StreamObjectWriter<ThreeComponentEnsemble> out('b');
        ThreeComponentEnsemble d;
        int n(0);
        while(!inp.eof())
        {
            d=inp.read();
            out.write(d);
            ++n;
        }
        if(SEISPP_verbose) 
            cerr << "seispp_t2b:  converted "<<n<<" 3C ensembles"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

