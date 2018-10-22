/* This set of includes will fail using g++ for reasons I haven't figured out */
/*
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PMTimeSeries.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
*/
/* Includes in this order work with g++.   */
#include <string>
#include <iostream>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP; 
void usage()
{
    cerr << "mwpm < in > out [-pf pffile --help -text]"
        <<endl
        << "Processes input data set of ThreeComponentSeismogram objects"<<endl
        << "Output is data set of PMTimeSeries objects"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i,j;
    const int narg_required(0);
    bool binary_data(true);
    string pffile("mwpm.pf");

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc) usage();
          pffile=string(argv[i]);
        }
        else
            usage();
    }
    try{
        shared_ptr<StreamObjectReader<ThreeComponentSeismogram>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>);
        }
        shared_ptr<StreamObjectWriter<PMTimeSeries>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<PMTimeSeries>>
             (new StreamObjectWriter<PMTimeSeries>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<PMTimeSeries>>
             (new StreamObjectWriter<PMTimeSeries>);
        }
        PfStyleMetadata control=pfread(pffile);
        /* These would normally use the defaults */
        double confidence=control.get<double>("error_estimate_confidence_interval");
        double bsmultiplier=control.get<double>("bootstrap_multiplier");
        int nsvd=control.get<int>("number_singular_values_to_use");
        /* The rest of these would usually be altered by the user */
        int avlen=control.get_int("particle_motion_time_average_length");
        int pmdt(1);
        if(avlen>1)
          pmdt=control.get_int("particle_motion_sampling_decimation_factor");
        MWTransform mwt(pffile);
        int nbands=mwt.number_frequencies();
        if(SEISPP_verbose)
        {
          cerr << "mwpm - processing seismogram with "<<mwt.number_frequencies()
            <<" frequencies and "
            <<mwt.number_wavelet_pairs()<<" basis function pairs"
            <<endl;
        }
        ThreeComponentSeismogram d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            /* The multiwavelet transform has no way currently to handle
             * data in relative time.   This is a kludge fix to handle 
             * this situation.   */
            bool restore_data_to_relative=false;
            double t0shift; // need this below to restore to absolute if needed
            if(d.tref == relative) 
            {
                restore_data_to_relative=true;
                t0shift=d.time_reference();
                d.rtoa(t0shift);
            }
            /* We make this a nonfatal error and drop seismgrams 
             * for which the transform fails.  Without this a large
             * data set can be aborted inappropriately. */
            shared_ptr<MWTBundle> dmwt;
            try{
              dmwt=shared_ptr<MWTBundle>(new MWTBundle(d,mwt));
            }catch(SeisppError& serr)
            {
              cerr << "Error encountered in MWTransform of object number "<<n
                  <<endl<<"Error message from processor follows:"<<endl;
              serr.log_error();
              cerr << "Attempting to continue"<<endl;
            };
            //DEBUG
            /*
            for(j=0;j<dmwt.number_wavelets();++j)
            {
                cerr << "t0,dt,ns - each component of mwtransform trace"<<endl;
                for(int ic=0;ic<3;++ic)
                {
                    MWTwaveform mwtmp=dmwt(0,j,ic);
                    cerr << strtime(mwtmp.t0)<<" "
                        << mwtmp.dt<<" "
                        << mwtmp.ns<<endl;
                }
            }
            */
            /* A bundle has data for multiple frequency bands in the general
             multiwavelet transform.  We generate one PMTimeSeries for
             each band.  The band tag is presently frozen, but perhaps
             should be a parameter. */ 
            for(j=0;j<nbands;++j)
            {
              PMTimeSeries pmts;
              if(avlen>1)
                pmts=PMTimeSeries(*dmwt,j,pmdt,avlen,confidence,bsmultiplier,
                        nsvd);
              else
                pmts=PMTimeSeries(*dmwt,j,confidence,bsmultiplier);
              pmts.put("band",j);
              if(restore_data_to_relative) 
              {
                  pmts.ator(t0shift);
              }
              //DEBUG
              /*
              cerr << "PMTimeSeries basic time series attribtues: "
                  << pmts.dt<<" "<<pmts.ns<<" "<<pmts.t0<<endl;
                  */
              out->write(pmts);
            }
            ++n;
        }
        if(SEISPP_verbose)
        {
          cerr << "Total number of 3C seismograms processed ="<<n<<endl
            << "Number of output PMTimeSeries object="<<n*nbands<<endl;
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

