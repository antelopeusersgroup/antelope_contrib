#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
#include "ThreeCEnsembleTimePicker.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std; 
using namespace SEISPP; 
void usage()
{
    cerr << "display_ensemble [-pf pffile] < infile"
        <<endl
        << "Display a ThreeComponentEnsemble object (typically end of a pipeline)"<<endl
        << "(Accepts only text file input.)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    if(argc>3) usage();
    char *pffile=argv[0];
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=argv[i];
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
        StreamObjectReader<ThreeComponentEnsemble> ia;
        ThreeComponentEnsemble d;
        d=ia.read();
        ThreeCEnsembleTimePicker win(control);
        int nseis;
        nseis=win.plot(d,true);
        cout << "Plot completed of "<<nseis<<" seismogram"<<endl
          << "Use the menu or type x in any active window to quit"
          <<endl;
        /* A not so good feature of this gizmo is we have to put it
         * in pick mode.*/
        win.pick();
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

