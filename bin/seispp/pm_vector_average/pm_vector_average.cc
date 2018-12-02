#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <vector>
#include "coords.h"
#include "SeisppError.h"
#include "pm_wt_avg.h"
#include "D1Jackknife.h"
using namespace std;
using SEISPP::SeisppError;
void usage()
{
    cerr << "pm_vector_average < in > out [ -wt (huber | bisquare | none) -escale x --help]"
        <<endl
        << "computes an average of a set of particle motion vectors"<<endl
        << "Normal operation is a robust m-estimator with the huber penalty function"<<endl
        << "but a simple average and alternative pentalty function is an option."<<endl
        << "Input is assumed to be a table of (white space separated) 3 element vectors "<<endl
        << "with a fourth column containing an estimate of the error in angle of each vector"<<endl
        << "relative to the true value (in degrees)"<<endl
        << "Output is a single line with four numbers:  average (unit) vector and error estimate for average"
        <<endl
        << "-wt "
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}

SupportedPenaltyFunctions get_pfunc_type(string pftype)
{
    if(pftype=="huber")
        return HUBER;
    else if(pftype=="bisquare")
        return BISQUARE;
    else if(pftype=="none")
         return NONE;
    else
    {
        cerr << "Do not know how to handle penalty function type="<<pftype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}


int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string pftype("huber");
    double error_scale(1.0);
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-wt")
        {
            ++i;
            if(i>=argc)usage();
            pftype=sarg;
        }
        else if(sarg=="-escale")
        {
            ++i;
            if(i>=argc)usage();
            error_scale=atof(argv[i]);
        }
        else
            usage();
    }
    try{
      SupportedPenaltyFunctions pfunc=get_pfunc_type(pftype);
      vector<double> errors;
      vector<UnitVector> x;
      vector<double> tags;
      double d[3];
      double ework;
      double other;
      while(fscanf(stdin,"%lf%lf%lf%lf%lf",&d[0],&d[1],&d[2],&ework,&other)==5)
      {
        UnitVector u(d);
        x.push_back(u);
        /* assume input is degrees - internally we do everything in radians */
        ework = rad(ework);
        errors.push_back(ework);
        tags.push_back(other);
      }
      /* First we compute mean from all the data */
      pm_wt_avg pmbar0(x,errors,error_scale,pfunc);
      UnitVector ubar;
      ubar=pmbar0.average();
      /* Now we compute the vector of delete one means */
      int N;
      N=x.size();
      int i,ii;
      vector<UnitVector> xd1,xbard1;
      vector<double> ed1;
      xd1.reserve(N-1);
      ed1.reserve(N-1);
      for(i=0;i<N;++i)
      {
        xd1.clear();
        ed1.clear();
        for(ii=0;ii<N;++ii)
        {
          if(ii!=i)
          {
            xd1.push_back(x[ii]);
            ed1.push_back(errors[ii]);
          }
        }
        pm_wt_avg pval(xd1,ed1,error_scale,pfunc);
        xbard1.push_back(pval.average());
      }
      D1Jackknife<UnitVector> jknife(xbard1,ubar);
      UnitVector jkmean=jknife.mean();
      UnitVector jktmp=jknife.variance();
      /* An oddity I had to do here to match the knife object interface is
      to put the variance estimate for the theta angles in the 0 component
      of the unit vector.  */
      double theta_std=sqrt(jktmp.n[0]);
      cout << jkmean.n[0]<<" "<<jkmean.n[1]<<" "<<jkmean.n[2]<<" "
          <<deg(sqrt(theta_std))<<endl;
    }
    catch(SeisppError& serr){
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
