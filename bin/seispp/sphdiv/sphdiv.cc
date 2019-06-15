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
    cerr << "sphdiv [-ro xx -decay power -akey keyword -text --help] < infile >outfile"
        <<endl
        << "Applies spherical divergence correction to three component"<<endl
        << "ensembles stored in a seispp serialized stream file"<<endl
        << "Amplitudes scaled by metadata offset variable to power."<<endl
        << "There is no normalization so beware"<<endl
        << " -r0 - offset reference distance to correct data"<<endl
        << "       (data are scaled by (r0/r)^power for spreading correction:"
        << " default is 100)"
        <<endl
        << " -decay - set power factor for correction (Default is 1.0)"<<endl
        << " -akey - Use keyword as the attribute tag for the scale applied "
        << "to each seismogram"<<endl
        << "         (default sphdiv_scale)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    double r0(100.0);
    double decay_power(1.0);
    bool binary_data(true);
    string akey("sphdiv_scale");
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-decay")
        {
            ++i;
            if(i>=argc)usage();
            decay_power=atof(argv[i]);
        }
        else if(sarg=="-r0")
        {
            ++i;
            if(i>=argc)usage();
            r0=atof(argv[i]);
            if(r0<=0)
            {
                cerr << "Illegal reference distance ="<<r0<<endl
                    << "r0 must be >= 0"<<endl;
                usage();
            }
        }
        else if(sarg=="-akey")
        {
            ++i;
            if(i>=argc)usage();
            akey=string(argv[i]);
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
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
            /* Do nothing if the seismogram is marked bad */
            if(dptr->live)
            {
                double offset=dptr->get_double("offset");
                double ratio=offset/r0;
                double scale=pow(ratio,decay_power);
                /* ThreeComponentSeismogram really needs an operator
                       that does this kind of scale - operator * */
                dscal(3*dptr->ns,scale,dptr->u.get_address(0,0),1);
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
