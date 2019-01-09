#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <vector>
#include "coords.h"
#include "seispp.h"
#include "SeisppError.h"
#include "PfStyleMetadata.h"
#include "pm_wt_avg.h"
#include "UVBootstrap.h"
#include "VectorStatistics.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "pm_vector_average < in > out [-extra -v --help -pf pffile]"
        <<endl
        << "computes an average of a set of particle motion vectors"<<endl
        << "Normal operation is a robust m-estimator with the huber penalty function"<<endl
        << "but a simple average and alternative penalty function is an option."<<endl
        << "Simple average is also automatic for small sample sizes (small defined in parameter file)"
        << endl
        << "Input is assumed to be a table of (white space separated) 3 element vectors "<<endl
        << "with a fourth column containing an estimate of the error in angle of each vector"<<endl
        << "relative to the true value (in degrees)"<<endl
        << "Optionally a 5th column of data can be handled with the -extra option."<<endl
        << "Output is to stdout"<<endl
        << " -v - be more verbose"<<endl
        << "   When off (default) the program only writes summary results on one line"<<endl
        << "   When additional computed quantities are written with headings"<<endl
        << " --help - prints this message"<<endl
        << " -pf - use to change parameter file from default pm_vector_average.pf"<<endl;
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

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool extra_col(false);
    bool verbose(false);
    string pffile("pm_vector_average.pf");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-extra")
          extra_col=true;
        else if(sarg=="-v")
          verbose=true;
        else
            usage();
    }
    try{
      PfStyleMetadata control(pffile);
      string pfunctype=control.get_string("penalty_function");
      SupportedPenaltyFunctions pfunc=get_pfunc_type(pfunctype);
      double error_scale=control.get<double>("error_scale_factor");
      double probability=control.get<double>("robust_scale_probability_level");
      double mrwtr=control.get<double>("robust_weight_level_threshold");
      double cl=control.get<double>("confidence_level");
      int trial_multiplier=control.get<int>("bootstrap_trial_multiplier");
      int boot_min=control.get<int>("smallest_size_for_bootstrap");
      int mintrials=control.get<int>("minimum_number_bootstrap_trials");
      int maxtrials=control.get<int>("maximum_number_bootstrap_trials");
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
        cerr << "pm_vector_average:   no input data to process"<<endl;
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
        if(N==1)
          medextra=extra[0];
        else
        {
          SEISPP::VectorStatistics<double> xex(extra);
          medextra=xex.median();
        }
      }
      /* First we compute mean from all the data */
      pm_wt_avg pmbar0(x,errors,error_scale,pfunc,probability);
      UnitVector ubar;
      ubar=pmbar0.average();
      /* We stop here if the size of the data vector is below
         this threshold defined in pm_wt_avg.h */
      if(x.size()<boot_min)
      {
        double ubar_error;
        ubar_error=pmbar0.sigma();
        /* this is repetitious but created in laziness.  I did
           not initially implement this out for low degree of
           freedom data sets. */
        if(verbose)
        {
          cout <<"Input data"<<endl
             << "x1 x2 x3 sigma theta(deg) theta/sigma"<<endl;
          for(i=0;i<N;++i)
          {
            cout << x[i] << " "<<deg(errors[i])<<" "
              <<deg(x[i].theta(ubar))<<" "<< x[i].theta(ubar)/errors[i] <<endl;
          }        
        }
        /* We put the tag at the head of this line to allow grep for 
           this string to pull them from output.  printing the confidence
        level is potentially wrong but necessary to have output compable to 
        bootstrap output line */
        cout << "Weighted mean vector and theta error estimate:  "<<endl
          << ubar <<" "<<" "<<deg(ubar_error)<<" "<<cl;
        if(extra_col)
        {
          cout << " "<<medextra<<endl;
        }
        else
        {
          cout << endl;
        }
        //exit(0);
      }
      else
      {
        /* Larger data sets will be processed with the bootstrap algorithm here */
        int number_of_trials=trial_multiplier*N;
        if(number_of_trials<mintrials)
          number_of_trials=mintrials;
        else if(number_of_trials>maxtrials)
          number_of_trials=maxtrials;
        UVBootstrap uboot(x,errors,pfunc,
          error_scale,probability,mrwtr,cl,number_of_trials);

        cout << "Robust mean and theta error estimate from all data"<<endl
            << "x1 x2 x3 theta_error average_ssq average_chisq robust_rms robust_chisq N"<<endl;
        cout << "RobustEstimatorResults:  " << ubar
           <<" "<<deg(pmbar0.sigma())<<" "
           << pmbar0.average_ssq()<<" "
           << pmbar0.average_chisq()<<" "
           << pmbar0.robust_rms()<<" "
           << pmbar0.robust_chisq()<<" "
           <<N<<endl;
        if(verbose)
        {
           cout <<"Input data"<<endl
              << "x1 x2 x3 sigma theta(deg) theta/sigma"<<endl;
           for(i=0;i<N;++i)
           {
             cout << x[i] << " "<<deg(errors[i])<<" "
               <<deg(x[i].theta(ubar))<<" "<< x[i].theta(ubar)/errors[i] <<endl;
           }
        }
        cout<<"Bootstrap average and theta error estimate"<<endl
            << "x1 x2 x3 theta_error confidence_level"<<endl;
        UnitVector bootavg=uboot.mean_vector();
        double theta_error=uboot.angle_error();
        cout << bootavg<<" "<<deg(theta_error)<<" "<<cl;
        if(extra_col)
        {
            cout << " "<<medextra<<endl;
        }
        else
        {
            cout << endl;
        }
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
