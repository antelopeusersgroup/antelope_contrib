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
    cerr << "cut tmin tmax  < in > out [-v --help -binary]"
        <<endl
        << "Window the data in time between tmin and tmax"<<endl
        << "Will exit with an error message if interval tmin:tmax is outside bounds of data"
        <<endl
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
    const int narg_required(2);
    double tmin,tmax;
    if(string(argv[1]) == "--help") usage();
    tmin=atof(argv[1]);
    tmax=atof(argv[2]);
    if(tmin>tmax)
    {
        cerr << "Illegal time window:  tmin>tmax"<<endl;
        usage();
    }
    TimeWindow twin(tmin,tmax);
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
        if(SEISPP_verbose) cerr << "cut:  windowing data between "
            << tmin<<" and "<<tmax<<" seconds"<<endl;
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
        vector<ThreeComponentSeismogram>::iterator dptr;
        while(inp->good())
        {
            d=inp->read();
            ThreeComponentEnsemble dcut(dynamic_cast<Metadata&>(d),
                    d.member.size());
            ThreeComponentSeismogram d3ccut;
            for(dptr=d.member.begin(),i=0;dptr!=d.member.end();++dptr,++i)
            {
                try{
                    d3ccut=WindowData(*dptr,twin);
                    dcut.member.push_back(d3ccut);
                }catch(SeisppError& serr)
                {
                    cerr << "cut: problem with member "<<i<<" of ensemble "
                        <<n<<endl<<"Error message thrown follows"<<endl;
                    serr.log_error();
                }
            }
            out->write(dcut);
            ++n;
        }
        if(SEISPP_verbose) cerr << "cu:  Total number of ensembles processed ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

