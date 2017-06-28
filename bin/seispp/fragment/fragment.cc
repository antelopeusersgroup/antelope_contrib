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
    cerr << "fragment basename [-i infile -dir outdir -dismember -binary -v --help]"
        <<endl
        << "seispp filter fragments file with multiple ensembles into individual files"
        <<endl
        << "basename is root name for each output ensemble.  Adds a sequence number for each ensemble"
        <<endl
        << "-i optional read from file infile (default is stdin)"<<endl
        << "-dir optional write to outdir (default is .)"
        <<endl
        << "-dismember - ungroup ensembles to build output files as unbundled collection of 3c seismograms"
        <<endl
        << "(Default is one ensemble per output file)"<<endl
        << "-binary - assume input and outputs should be binary format"
        << "(Default is text)"<<endl
        << "-v verbose output (mostly logs each ensembles gather metadata"
        <<endl
        << "Note:   Only works at present with ThreeComponentEnsemble objects"
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
    if(basename=="--help") usage();
    bool binary_data(false);
    bool dismember(false);
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
        else if(sarg=="-dismember")
            dismember=true;
        else if(sarg=="-v")
            Verbose=true;
        else if(sarg=="-binary")
            binary_data=true;
        else if(sarg=="--help")
            usage();
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
        StreamObjectWriter<ThreeComponentSeismogram> *out;
        StreamObjectWriter<ThreeComponentEnsemble> *outens;
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
            if(dismember)
            {
              if(binary_data)
              {
                out=new StreamObjectWriter<ThreeComponentSeismogram>(path,'b');
              }
              else
              {
                out=new StreamObjectWriter<ThreeComponentSeismogram>(path,'t');
              }
              count=write_ensemble<ThreeComponentEnsemble,ThreeComponentSeismogram>
                (d3c,shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>(out));
              //delete out;
            }
            else
            {
              if(binary_data)
              {
                outens=new StreamObjectWriter<ThreeComponentEnsemble>(path,'b');
              }
              else
              {
                outens=new StreamObjectWriter<ThreeComponentEnsemble>(path,'t');
              }
              outens->write(d3c);
              delete outens;
              count=d3c.member.size();
            }
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
