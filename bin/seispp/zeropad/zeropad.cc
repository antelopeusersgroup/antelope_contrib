#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
ThreeComponentSeismogram pad_3cseis(ThreeComponentSeismogram& d,double plen,double tlen)
{
    int i,j;
    /* Return the original if marked dead */
    if(!d.live) return(d);
    /* Sanity checks on plen and tlen */
    const string base_error("pad_3cseis procedure:  ");
    double tracelength=d.endtime()-d.t0;
    if(plen>tracelength)
        throw SeisppError(base_error + "pad length irrational - exceeds data time length");
    const double taper_max_fraction(0.5);
    if(tlen>taper_max_fraction*tracelength)
        throw SeisppError(base_error + "taper length irrational - exceeds half of data time span");
    int nsnew,npad,ntaper;
    npad=plen/d.dt;
    ntaper=tlen/d.dt;
    nsnew=d.ns+npad;
    try {
        /* This buffer holds new data */
        dmatrix upad(3,nsnew);
        double *uptr;
        uptr=upad.get_address(0,0);
        /* This assumes dmatrix uses a single block of memory to hold matrix*/
        for(i=0;i<3*npad;++i,++uptr) *uptr=0.0;
        /* First copy the data before tapering */
        for(j=0;j<d.ns;++j)
            for(i=0;i<3;++i)
                upad(i,j+npad)=d.u(i,j);
        /* now do a linear taper */
        double wt0,wt;
        wt0=1.0/((double)ntaper);
        for(j=0,wt=wt0;j<ntaper-1;++j,wt+=wt0)
            for(i=0;i<3;++i)
                upad(i,j+npad)*=wt;
        d.u=upad;
        d.t0 -= plen;
        return d;
    }catch(...){throw;};
}

void usage()
{
    cerr << "zeropad [-pad dt -taper dt -text --help] < in > out"
        <<endl
        << "Zeropad all elements of a file of 3C ensembles"<<endl
        << " -pad sets zero pad time to dt (default 0.1)"<<endl
        << " -taper sets taper length to dt (default 0.01)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    double padlength(0.1);
    double taperlength(0.01);
    bool binary_data(true);
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pad")
        {
            ++i;
            if(i>=argc)usage();
            padlength=atof(argv[i]);
        }
        else if(sarg=="-taper")
        {
            ++i;
            if(i>=argc)usage();
            taperlength=atof(argv[i]);
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
      int nens(0);
      while(!ia->eof())
      {
        d=ia->read();
        vector<ThreeComponentSeismogram>::iterator dptr;
        try{
          for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
          {
            ThreeComponentSeismogram dpadded=pad_3cseis(*dptr,padlength,taperlength);
            *dptr=dpadded;
          }
          oa->write(d);
        }catch(SeisppError& derr)
        {
          cerr << "Error processing ensemble number "<<nens<<endl;
          derr.log_error();
        }
        ++nens;
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
