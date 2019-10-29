#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "PfStyleMetadata.h"
#include "mute.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "topmute < in > out [-v --help -pf pffile -text]"
        <<endl
        << "Apply top mute to ThreeComponentEnsemble data"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << "-pf - use pffile instead of default topmute.pf"<<endl
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
    bool binary_data(true);
    string pffile("topmute");
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
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
        string tag("TopMute");
        PfStyleMetadata control(pffile);
        TopMute mute(control,tag);
        ThreeComponentEnsemble d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            ApplyTopMute(d,mute);
            out->write(d);
            ++n;
        }
        if(SEISPP_verbose)
           cerr << "topmute:  Total number of ensembles copied ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
