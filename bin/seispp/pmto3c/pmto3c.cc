#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "pmto3c < in > out [-minor -v --help -text]"
        <<endl
        << "Extracts major axis of particle motion ellipses defined in "
        << "input PMTimeSeries objects"<<endl
        << "Output is major axis vectors written as ThreeComponentSeismogram"
        << " objects"<<endl
        << " -minor - to switch to output minor axis vectors "
        << "(default is major axis vectors)"<<endl
        << " -unit - output unit vectors (default scale by axis amplitude)"
        <<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i,k;
    bool use_minor(false);
    bool unit_out(false);;
    bool binary_data(true);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-minor")
        {
            use_minor=true;
        }
        else if(sarg=="-unit")
        {
            unit_out=true;
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
        shared_ptr<StreamObjectReader<PMTimeSeries>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<PMTimeSeries>>
             (new StreamObjectReader<PMTimeSeries>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<PMTimeSeries>>
             (new StreamObjectReader<PMTimeSeries>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>);
        }
        PMTimeSeries d;
        while(inp->good())
        {
            d=inp->read();
            ThreeComponentSeismogram dout(dynamic_cast<Metadata&>(d),false);
            dout.ns=d.ns;
            dout.t0=d.t0;
            dout.live=true;
            dout.tref=d.tref;
            dout.u=dmatrix(3,d.ns);
            dout.u.zero();
            for(i=0;i<dout.ns;++i)
            {
              /* leave gap data as zeros for this output */
              if(!d.is_gap(i))
              {
                ParticleMotionEllipse pme=d.ellipse(i);
                if(use_minor)
                {
                  if(unit_out)
                    for(k=0;k<3;++k) dout.u(k,i)=pme.minor[k];
                  else
                    for(k=0;k<3;++k) dout.u(k,i)=pme.minornrm*pme.minor[k];
                }
                else
                {
                  if(unit_out)
                    for(k=0;k<3;++k) dout.u(k,i)=pme.major[k];
                  else
                    for(k=0;k<3;++k) dout.u(k,i)=pme.majornrm*pme.major[k];
                }
              }
            }
            out->write(dout);
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

