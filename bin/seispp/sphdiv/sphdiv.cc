#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "perf.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "sphdiv [-decay power -text --help] < infile >outfile"
        <<endl
        << "Applies spherical divergence correction to three component"<<endl
        << "data stored in a boost test archive file."<<endl
        << "Amplitudes scaled by metadata offset variable to power."<<endl
        << "There is no normalization so beware"<<endl
        << " -decay - set power factor for correction (Default is 2.0)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    double decay_power(2.0);
    bool binary_data(true);
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-decay")
        {
            ++i;
            if(i>=argc)usage();
            decay_power=atof(argv[i]);
        }
        else if(sarg=="-text")
            binary_data=false;
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
        double scale;
        const double MIN_offset(0.001);
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
            /* Do nothing if the seismogram is marked bad */
            if(dptr->live)
            {
                double offset=dptr->get_double("offset");
                /* sanity check to avoid zeroing data.
                   Assumes rational units. */
                if(offset>MIN_offset)
                {
                    scale=pow(offset,decay_power);
                    /* ThreeComponentSeismogram really needs an operator
                       that does this kind of scale - operator * */
                    dscal(3*dptr->ns,scale,dptr->u.get_address(0,0),1);
                }
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
