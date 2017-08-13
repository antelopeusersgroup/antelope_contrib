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
    cerr << "template < in > out [-v --help -text]"
        <<endl
        << "Example, do nothing filter using seismic unix style pipeline"<<endl
        << "Reads serialized ThreeComponentEnsemble objects from stdin"<<endl
        << "and writes a copy to stdout"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    double example_real(0.0);
    bool example_boolean(false);
    bool binary_data(true);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-x")
        {
            ++i;
            if(i>=argc)usage();
            example_real=atof(argv[i]);
        }
        else if(sarg=="-flag")
        {
            example_boolean=true;
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
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
        cerr << "Template seispp unix filter:  copying "
            <<"ThreeComponentEnsemble object from stdin to stdout"
            <<endl;
        if(binary_data)
            cerr << "Assuming binary format data"<<endl;
        else
            cerr << "Assuming ascii formatted data"<<endl;
        while(inp->good())
        {
            d=inp->read();
            out->write(d);
            ++n;
        }
        cerr << "Total number of ensembles copied ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

