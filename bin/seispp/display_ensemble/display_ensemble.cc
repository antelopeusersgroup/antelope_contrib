#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
#include "SeismicPlot.h"
#include "ThreeCEnsembleTimePicker.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "display_ensemble [-t objt -text -pf pffile] < infile"
        <<endl
        << "Display an ensemble object (typically end of a pipeline)"<<endl
        << " -t set object type for ensemble (ThreeComponentEnsemble (default) or TimeSeriesEnsemble)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "Use -pf to specify alternative parameter file to default display_ensemble.pf"
        <<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE,  TS, TSE, PMTS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* I tnink this declaration is necessary */
template <typename Tdata> void display_ensemble(Metadata& c, bool b);
/* We spcialize each ensemble because I want to use a different plot gizmo for
scalar and 3c data */
template <> void display_ensemble<ThreeComponentEnsemble>(Metadata& control,bool binary_data)
{
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
    ThreeComponentEnsemble d;
    SeismicPlot win(control);
    int i(0);
    while(inp->good())
    {
      d=inp->read();
      cerr << "Plotting ensemble number "<<i<<endl
        << "Hit x key in the display window to show next ensemble"<<endl;
      win.plot(d);
      ++i;
    }
  }catch(...){throw;};
}

template <> void display_ensemble<TimeSeriesEnsemble>(Metadata& control,bool binary_data)
{
  try{
    shared_ptr<StreamObjectReader<TimeSeriesEnsemble>> inp;
    if(binary_data)
    {
      inp=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
         (new StreamObjectReader<TimeSeriesEnsemble>('b'));
    }
    else
    {
      inp=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
         (new StreamObjectReader<TimeSeriesEnsemble>);
    }
    TimeSeriesEnsemble d;
    SeismicPlot win(control);
    int i(0);
    while(inp->good())
    {
      d=inp->read();
      cerr << "Plotting ensemble number "<<i<<endl
        << "Hit x key in the display window to show next ensemble"<<endl;
      win.plot(d);
      ++i;
    }
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    if(argc>3) usage();
    char *pffile=argv[0];
    bool binary_data(true);
    string otype("ThreeComponentEnsemble");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=argv[i];
        }
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else
            usage();
    }
    Pf *pf;
    if(pfread(pffile,&pf))
    {
      cerr << "pfread failed on file="<<pffile<<endl;
      usage();
    }
    try{
        Metadata control(pf);
        AllowedObjects dtype=get_object_type(otype);
        switch (dtype)
        {
            case TCE:
                display_ensemble<ThreeComponentEnsemble>(control,binary_data);
                break;
            case TSE:
                display_ensemble<TimeSeriesEnsemble>(control,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };

    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
