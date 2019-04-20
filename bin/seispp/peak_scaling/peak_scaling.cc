/* This small program scales each 3c seismogram by peak amplitude.  */
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "peak_scaling [-help -v -text] < infile > outfile"
        <<endl
        << "Reads serialized 3c ensemble file and scales each "
        << "seismogram by peak 3C amplitude"<<endl
        << " -v - be more verbose (echos scaling numbers)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}

/* This obnoxious external variable is a necessary evil to deal with
   error logging in the SEISPP library. Your code will probably not link
   without it.*/
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int iarg;
    bool binary_data(true);
    for(iarg=1;iarg<argc;++iarg)
    {
      string sarg(argv[iarg]);
      if(sarg=="-text")
        binary_data=false;
      else if(sarg=="--help")
        usage();
      else if(sarg=="-v")
        SEISPP_verbose=true;
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
        vector<ThreeComponentSeismogram>::iterator dptr;
        int i;
        for(dptr=d.member.begin(),i=0;dptr!=d.member.end();++dptr,++i)
        {
          if(dptr->live)
          {
            double amp;
            ThreeComponentSeismogram *ptr;
            ptr=&(d.member[i]);
            amp=PeakAmplitude(ptr);
            double gain(1.0);
            try {
                gain=dptr->get_double("gain");
            }catch(MetadataGetError &mde)
            {
                if(SEISPP_verbose)
                    cerr << "Warning:   gain attribute was not set, using default 1"
                        <<endl;
            }
            double scaling=1.0/amp;
            gain = gain*scaling;
            dptr->put("gain",gain);
            dptr->u=scaling*dptr->u;
            if(SEISPP_verbose)
                cerr << "Computed gain scaling="<<scaling<<endl;
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
