#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <vector>
#include "coords.h"
#include "SeisppError.h"
#include "pm_wt_avg.h"
#include "D1Jackknife.h"
#include "VectorStatistics.h"
using namespace std;
using SEISPP::SeisppError;
void usage()
{
    cerr << "pm_vector_average < in > out [ -wt (huber | bisquare | none) -escale x -extra -v --help]"
        <<endl
        << "computes an average of a set of particle motion vectors"<<endl
        << "Normal operation is a robust m-estimator with the huber penalty function"<<endl
        << "but a simple average and alternative pentalty function is an option."<<endl
        << "Input is assumed to be a table of (white space separated) 3 element vectors "<<endl
        << "with a fourth column containing an estimate of the error in angle of each vector"<<endl
        << "relative to the true value (in degrees)"<<endl
        << "Output is a single line with four numbers:  average (unit) vector and error estimate for average"
        <<endl
        << "-wt selects robust weighting function (none turns off robust weighting) "<<endl
        << " -v - be more verbose"<<endl
        << "   When off (default) the program only computes the final average, an estimate of angular error,"<<endl
        << "   and (optionally) the median of the extra data column.   When true the input data are exchoed"<<endl
        << "   with the angle between each vector and the robust average"<<endl
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
    bool extra_col(false);
    bool verbose(false);
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
        else if(sarg=="-extra")
          extra_col=true;
        else if(sarg=="-v")
          verbose=true;
        else
            usage();
    }
    try{
      SupportedPenaltyFunctions pfunc=get_pfunc_type(pftype);
      vector<double> errors;
      vector<UnitVector> x;
      vector<double> extra;
      double d[3];
      double ework;
      double other;
      char indat[128];
      while(cin.getline(indat,128))
      {
        stringstream ss(indat);
        ss>>d[0];  ss>>d[1]; ss>>d[2]; ss>>ework;
        if(extra_col)
        {
          ss>>other;
          extra.push_back(other);
        }
        UnitVector u(d);
        x.push_back(u);
        /* assume input is degrees - internally we do everything in radians */
        ework = rad(ework);
        errors.push_back(ework);
      }
      if(x.size()<=0) 
      {
        cerr << "pm_vector_average:   no input data to prorcess"<<endl;
        exit(-1);
      }
      int N;
      N=x.size();

      /* We go ahead and compute the median of the data in the
         extra column right away.  It would typically be something
         like back azimuth.  */
      double medextra;
      if(extra_col)
      {
        SEISPP::VectorStatistics<double> xex(extra);
        medextra=xex.median();
      }
      /* First we compute mean from all the data */
      pm_wt_avg pmbar0(x,errors,error_scale,pfunc);
      UnitVector ubar;
      ubar=pmbar0.average();
      /* We stop here if the size of the data vector is below 
         this threshold defined in pm_wt_avg.h */
      if(x.size()<robust_lower_limit)
      {
        double ubar_error;
        ubar_error=pmbar0.sigma();
        /* this is repetitious but created in laziness.  I did
           not initially implement this out for low degree of 
           freedom data sets. */
        if(verbose)
        {
          cout << "x1 x2 x3 sigma theta(deg) theta/sigma"<<endl;
          for(i=0;i<N;++i)
          {
            cout << x[i].n[0]<<" "<< x[i].n[1]<<" "<< x[i].n[2]<<" "<<deg(errors[i])<<" "
              <<deg(x[i].theta(ubar))<<" "<< x[i].theta(ubar)/errors[i] <<endl;
          }
          cout << "regular mean vector and theta error estimate"<<endl;
        }
        cout << ubar.n[0]<<" "<<ubar.n[1]<<" "<<ubar.n[2]
         <<" "<<deg(ubar_error);
        if(extra_col)
        {
          cout << " "<<medextra<<endl;
        }
        else
        {
          cout << endl;
        }
        exit(0);
      }
      /* This is also repetitious, but useful output.   In verbose mode it is helpful 
         to print the rms statistics. */
      if(verbose)
      {
        cout << "Robust mean and theta error estimate from all data"<<endl
          << "x1 x2 x3 theta_error average_ssq average_chisq robust_rms robust_chisq"<<endl;
        cout << ubar.n[0]<<" "<<ubar.n[1]<<" "<<ubar.n[2]
         <<" "<<deg(pmbar0.sigma())<<" "
         << pmbar0.average_ssq()<<" "
         << pmbar0.average_chisq()<<" "
         << pmbar0.robust_rms()<<" "
         << pmbar0.robust_chisq()<<endl;
      }
      /* Now we compute the vector of delete one means */
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
      if(verbose)
      {
        cout << "x1 x2 x3 sigma theta(deg) theta/sigma"<<endl;
        for(i=0;i<x.size();++i)
        {
          cout << x[i].n[0]<<" "<< x[i].n[1]<<" "<< x[i].n[2]<<" "<<deg(errors[i])<<" "
            <<deg(x[i].theta(jkmean))<<" "<<x[i].theta(jkmean)/errors[i] <<endl;
        }
        cout << "jackknife mean vector and theta error estimate and error statistics"<<endl
          << "x1 x2 x3 sigma_theta(deg) optional_extra"<<endl; 
      }
      cout << jkmean.n[0]<<" "<<jkmean.n[1]<<" "<<jkmean.n[2]
         <<" "<<deg(theta_std);
      if(extra_col)
      {
        cout << " "<<medextra<<endl;
      }
      else
      {
        cout << endl;
      }
    }
    catch(SeisppError& serr){
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
