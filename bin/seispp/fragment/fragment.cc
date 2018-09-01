/* This is a filter that will separate a TimeSeries of ThreeComponent
   seismogram into components stripping ensemble header values and
   putting copies into the header of each member.   This is a required
   step before sort or any other process that operates on one seismogram
   at a time.
*/

/* This set of system includes are always required.  Do not remove them.*/
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include "stock.h" // needed for makedir call
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "fragment basename [-dir outdir -v]"
        <<endl
        << "seispp filter fragments file with multiple ensembles into individual files"
        <<endl
        << "basename is root name for each ensemble.  Adds a sequence number for each ensemble"
        <<endl
        << "-dir optional write to outdir (default is .)"
        <<endl
        << "-v verbose output (mostly logs each ensembles gather metadata"
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    bool Verbose(false);
    int i;
    if(argc<2) usage();
    string outdir(".");
    string basename(argv[1]);
    bool binary_data(false);
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-dir")
        {
            ++i;
            if(i>=argc)usage();
            outdir=string(argv[i]);
            if(makedir(const_cast<char *>(outdir.c_str())))
            {
                cerr << "Cannot create requested directory "
                    << outdir<<endl;
                usage();
            }
        }
        else if(sarg=="-v")
            Verbose=true;
        else if(sarg=="-binary")
            binary_data=true;
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
        ThreeComponentEnsemble d3c;
        int nensembles(0);
        int nseis(0);
        while(!ia->eof())
        {
            int count;
            d3c=ia->read();
            char fname[128];
            sprintf(fname,"%s_%d",basename.c_str(),nensembles);
            string path;
            path=outdir+"/"+fname;
            if(Verbose)
            {
                cerr << "ensemble (gather) metadata for output file "
                    << path<<endl;
                cerr << dynamic_cast<Metadata&>(d3c)<<endl;
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
            count=write_ensemble<ThreeComponentEnsemble,ThreeComponentSeismogram>
                (d3c,out);
            ++nensembles;
            nseis+=count;
        }
    }
    catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
