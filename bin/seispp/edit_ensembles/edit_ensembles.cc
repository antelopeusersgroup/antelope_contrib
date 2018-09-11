#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "TraceEditPlot.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "edit_ensembles < in > out [-t object_type -v --help -text]"
        <<endl
        << "Interactive editor for ensembles.   Each ensemble in a file"<<endl
        << "is displayed for kill picking (use MB2 to mark traces to kill)"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default) or TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
enum AllowedObjects {TCE,  TSE};
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
template <typename DataType> int edit_ensembles(TraceEditPlot& win,bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            win.plot(d);
            if(SEISPP_verbose)
            {
              cerr << "Edit completed for ensemble number "<<count<<endl;
              set<int> kills;
              kills=win.report_kills();
              if(kills.size()<=0)
                cerr << "No seismograms were killed"<<endl;
              else
              {
                cerr << "Members killed: ";
                set<int>::iterator kptr;
                for(kptr=kills.begin();kptr!=kills.end();++kptr)
                {
                  cerr << *kptr<<", ";
                }
                cerr << endl;
              }
            }
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string otype("ThreeComponentEnsemble");
    string pffile("edit_ensembles.pf");

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
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else
            usage();
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        PfStyleMetadata control=pfread(pffile);
        TraceEditPlot win(control);
        int count;
        switch (dtype)
        {
            case TCE:
                count=edit_enembles<ThreeComponentEnsemble>(win,binary_data);
                break;
            case TSE:
                count=edit_ensembles<TimeSeriesEnsemble>(win,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "template:  copied "<<count<<" objects from stdin to stdout"
            <<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
