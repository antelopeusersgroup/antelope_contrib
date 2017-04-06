/* This is a filter that will separate a TimeSeries of ThreeComponent
   seismogram into components stripping ensemble header values and
   putting copies into the header of each member.   This is a required
   step before sort or any other process that operates on one seismogram
   at a time.
*/

/* This set of system includes are always required.  Do not remove them.*/
#include <memory>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <vector>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "dismember [-scalar -binary]"
        <<endl
        << "seispp filter separates ensemble into seismogram members"<<endl
        << " Use -scalar for TimeSeriesEnsembles (default is ThreeComponentEnsemble data)"
        << " Use -binary to read/write binary files"<<endl
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    bool scalar_mode(false);
    bool binary_data(false);
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-scalar")
          scalar_mode=true;
        else if(sarg=="-binary")
            binary_data=true;
        else
            usage();
    }
    try{
        shared_ptr<StreamObjectReader<TimeSeriesEnsemble>> ias;
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> ia3c;
        if(binary_data)
        {
          if(scalar_mode)
          {
            ias=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
             (new StreamObjectReader<TimeSeriesEnsemble>('b'));
          }
          else
          {
            ia3c=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
          }
        }
        else
        {
          if(scalar_mode)
          {
            ias=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
             (new StreamObjectReader<TimeSeriesEnsemble>);
          }
          else
          {
            ia3c=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
          }
        }
        shared_ptr<StreamObjectWriter<TimeSeries>> oas;
        shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>> oa3c;
        if(binary_data)
        {
          if(scalar_mode)
          {
            oas=shared_ptr<StreamObjectWriter<TimeSeries>>
             (new StreamObjectWriter<TimeSeries>('b'));
          }
          else
          {
            oa3c=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>('b'));
          }
        }
        else
        {
          if(scalar_mode)
          {
            oas=shared_ptr<StreamObjectWriter<TimeSeries>>
             (new StreamObjectWriter<TimeSeries>);
          }
          else
          {
            oa3c=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>);
          }
        }
        ThreeComponentEnsemble d3c;
        TimeSeriesEnsemble d;
        int nensembles(0),nd(0);
        /* The logic of this gets ugly to support both TimeSeries and
         * 3C ensembles.   Infinite loop broken by different 
         * calls to io handles is the only way I see to do this.. */
        while(1)
        {
            int count;
            if(scalar_mode)
            {
              if(ias->eof()) break;
              d=ias->read();
              count=write_ensemble<TimeSeriesEnsemble,TimeSeries>
                  (d,oas);
              nd+=count;
            }
            else
            {
              if(ia3c->eof()) break;
              d3c=ia3c->read();
              count=write_ensemble<ThreeComponentEnsemble,ThreeComponentSeismogram>
                  (d3c,oa3c);
              nd+=count;
            }
            ++nensembles;
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
