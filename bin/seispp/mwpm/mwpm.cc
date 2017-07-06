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
    cerr << "mwpm < in > out [-pf pffile --help -binary]"
        <<endl
        << "Processes input data set of ThreeComponentSeismogram objects"<<endl
        << "Output is data set of PMTimeSeries objects"<<endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i,j;
    const int narg_required(0);
    bool binary_data(false);
    string pffile("mwpmi.pf");

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
        int avlen=control.get_int("particle_motion_time_average_length");
        int pmdt(1);
        if(avlen>1)
          pmdt=control.get_int("particle_motion_sampling_decimation_factor");
        MWTransform mwt(pffile);
        int nbands=mwt.number_frequencies();
        if(SEISPP_verbose)
        {
          cerr << "mwpm - processing seismogram with "<<mwt.number_frequencies()
            << " and "<<mwt.number_wavelet_pairs()<<" basis function pairs"
            <<endl;
        }
        ThreeComponentSeismogram d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            MWTBundle dmwt(d,mwt);
            /* A bundle has data for multiple frequency bands in the general
             multiwavelet transform.  We generate one PMTimeSeries for
             each band.  The band tag is presently frozen, but perhaps
             should be a parameter. */ 
            for(j=0;j<nbands;++j)
            {
              PMTimeSeries pmts;
              if(avlen>1)
                pmts=PMTimeSeries(dmwt,j,pmdt,avlen);
              else
                pmts=PMTimeSeries(dmwt,j);
              pmts.put("band",j);
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

