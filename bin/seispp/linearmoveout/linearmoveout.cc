#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "perf.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
/* This program applies a linear moveout computed from the 
   offset Metadata attribute stored with the data.  This 
   program is a unix filter that assumes input is a boost
   serialized ThreeComponentSeismogram object.   Output is 
   an altered version of the input.  

   This algorithm is less general than it should be because of
   a current limitation in the boost library.   I'm unable to
   get the boost library to save gap data.  Hence, here I 
   use the classic seismic processing approach of silently 
   zeroing gap data.  This will happen a lot with this algorithm.
   Whenever a negative t0 value is specified a zero pad will 
   always be created when the input is shot data.   If the input 
   is derived from continuous data that is a user error but it is 
   unavoidable with shot data.   

Author:  Gary L Pavlis
Written:  Nov 29, 2015
*/
void usage()
{
    cerr << "linearmoveout [-vr v_reduce -t0 timeshift --help -text] < infile > outfile"
        <<endl
        << " Applies a constant wavespeed time shift to data based on offset"
        << " header value"<<endl
        << " Lag applid is posted to header with key linearmoveout"<<endl
        << " Input must be ThreeComponentEnsemble objects" <<endl
        << " -vr sets reducing velocity (default 6000)"<<endl
        << " -t0 applies a time shift to all seismograms (default 0)"<<endl;;
    exit(-1);
}
const string lagkey("linearmoveout");
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    double vreduce(6000.0);   // unit so m/s for this default
    double t0(0.0);
    bool binary_data(true);
    int i;
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-t0")
        {
            ++i;
            if(i>=argc)usage();
            t0=atof(argv[i]);
        }
        else if(sarg=="-vr")
        {
            ++i;
            if(i>=argc)usage();
            vreduce=atof(argv[i]);
        }
        else if(sarg=="--help")
            usage();
        else if(sarg=="-text")
            binary_data=false;
        else
            usage();
    }
    try{
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
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> oa;
        if(binary_data)
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentEnsemble d;
        while(!ia->eof())
        {
          d=ia->read();
          int nm=d.member.size();
          for(i=0;i<nm;++i)
          {
            if(d.member[i].live)
            {
               double tshift;
               double offset=d.member[i].get_double("offset");
               offset=fabs(offset);
               tshift = (offset/vreduce);
               tshift += t0;
               d.member[i].put(lagkey,tshift);
               d.member[i].t0 -= tshift;
            }
          }
          oa->write(d);
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

