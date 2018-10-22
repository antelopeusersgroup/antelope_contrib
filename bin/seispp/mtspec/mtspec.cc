#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "SeisppError.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
/* At one point I was using this program to test this new generic
 * ensemble implementation.   */
//#include "Ensemble.h"
#include "MTSpectrum.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "mtspec < in > out [-tbp x -t object_type -v --help -text]"
        <<endl
        << "Compute spectral estimates of a data set using the multitaper method"<<endl
        << "The spectra are computed from each seismogram and saved as the same"<<endl
        << "object type as the input.   The dt attribute is replaced with the frequency"<<endl
        << "bin interval, ns may be adjusted, and t0 is set to 0"<<endl
        << "For 3C data each component in the output is the spectrum of that component of the input."<<endl
        << "Note the implementation uses matlab so expect the program to display matlab's splash screen"<<endl
        << " Use -tbp to change the time bandwidth produce (default is 4)"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* This procedure parses an input string (normally from argv)
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCS, TCE,  TS, TSE};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* This example only works with input from stdin and output to stdout.
 * Fairly simple changes to work for files - change the arguments
 * and calls to constructors.  Example returns a count of the
 * number of objects copied. */
template <typename DataType> int mtspec(int tbp, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        MTSpectrum processor;
        int count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            try {
              if(SEISPP_verbose)
                  cerr << "mtspec:  working on object number "<<count<<endl;
              d=processor.spectrum(d);
              outp.write(d);
              ++count;
            }catch(SeisppError& serr)
            {
                cerr << "Error was thrown by spectrum calculator.  Message follows:"<<endl;
                serr.log_error();
            }
        }
        return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    int time_bandwidth_product(4);
    string otype("ThreeComponentSeismogram");

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-tbp")
        {
            ++i;
            if(i>=argc)usage();
            time_bandwidth_product=atoi(argv[i]);
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
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=mtspec<ThreeComponentSeismogram>(time_bandwidth_product,binary_data);
                break;
            case TCE:
                count=mtspec<ThreeComponentEnsemble>(time_bandwidth_product,binary_data);
                break;
            case TS:
                count=mtspec<TimeSeries>(time_bandwidth_product,binary_data);
                break;
            case TSE:
                count=mtspec<TimeSeriesEnsemble>(time_bandwidth_product,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "mtspec:  computed spectra for  "<<count<<" objects"
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
