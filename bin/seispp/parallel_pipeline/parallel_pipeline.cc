#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "ParallelReader.h"
#include "PipelineProcessor.h"
#include "StreamObjectWriter.h"
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi=boost::mpi;
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "parallel_pipeline dataset  [-o outbase -t object_type -v --help -pf pffile]"
        <<endl
        << "Executes a parallel pipeline of commands to a cluster"<<endl
        << "Normally this command would be run with an auxiliary startup program"<<endl
        << "Command example is:  mpirun -np 8 paralle_pipeline mydataset"<<endl
        << "Commands to run in the pipeline on each node are defined by "
        << "a parameter file"<<endl
        << "(Default pf is parallel_pipeline.pf)"<<endl<<end
        << "  dataset - is the input data set defined by a parameter file with this base name"<<endl
        << "   See documentation for DataSetReader object.   Note trailing .pf can be dropped"<<endl

        << " Use -o to specify the base name for output files comging down each processor chain"<<endl
        << "    Note: (1) Default is PipelinProcessorOutput."<<endl
        << "          (2) _rank is added to base name to generate a unique file name"<<endl
        << "              (rank is the MPI rank of the pipeline handling the processing)"<<endl
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
enum AllowedObjects {TCS, TCE,  TS, TSE, PMTS};
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
    else if(otype=="PMTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename Tdata> void parallel_pipeline(const PfStyleMetadata& pf,
    const string outbase)
{
  try{
    DataSetReader<Tdata> dshandle;
    long ndata=dshandle.number_available();
    mpi::environment();
    mpi::communicator world;
    int nworkers=world.size()-1;
    if(world.rank()==0)
    {
      MPICountingForeman foreman(world,ndata,0);
      foreman.run();
      if(SeisppVerbose)
        report good and bad
    }
    else
    {
      int myrank=world.rank();
      ParallelReader<Tdata> prhandle(dshandle,myrank,world);
      string outfile=make_outfilename(outbase,myrank);
      PipelineProcessor<Tdata> pphandle(pf,outfile);
      Tdata d;
      do{
        try{
          d=prhandle.read();
        }catch(SeisppError& serr)
        {
          cerr << "Error reading on processor with rank="<<myrank<<endl
            << "Reader threw the following message:"<<endl;
          serr.log_error();
        }
        if(d.live)
        {
          /* This maybe should be enclosed in a try/catch*/
          pphandle.write(d);
        }
    }while(prhandle.good());
    world.barrier();
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string otype("ThreeComponentEnsemble");
    string infile(argv[1]);
    string outbase("parallel_pipeline");
    string pffile("parallel_pipeline");
    if(SEISPP_verbose) cerr << "parallel_pipeline:  reading dataset="<<infile<<endl;

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-o")
        {
            ++i;
            if(i>=argc)usage();
            outbase=string(argv[i]);
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
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        PfStyleMetadata pf;
        pf=pfread(pffile);
        int count;
        switch (dtype)
        {
            case TCS:
                parallel_pipeline<ThreeComponentSeismogram>(pf,outbase);
                break;
            case TCE:
                parallel_pipeline<ThreeComponentEnsemble>(pf,outbase);
                break;
            case TS:
                parallel_pipeline<TimeSeries>(pf,outbase);
                count=copy_object<TimeSeries>(binary_data);
                break;
            case TSE:
                parallel_pipeline<TimeSeriesEnsemble>(pf,outbase);
                break;
            case PMTS:
                parallel_pipeline<PMTimeSeries>(pf,outbase);
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
