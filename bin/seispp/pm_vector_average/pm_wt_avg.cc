#include <float.h>
//DEBUG
#include <iostream>
#include "coords.h"

#include "pm_wt_avg.h"
#include "penalty_function.h"
#include "SeisppError.h"
#include "VectorStatistics.h"
using SEISPP::VectorStatistics;
using SEISPP::SeisppError;

pm_wt_avg::pm_wt_avg(vector<UnitVector>& d, vector<double>& e, double scale,
    SupportedPenaltyFunctions pfunc, double mrwtr)
{
  try{
    min_rwt_ratio=mrwtr;
    const string base_error("pm_wt_avg constructor:  ");
    double scalesq=scale*scale;
    /* A basic sanity check */
    if(d.size() != e.size()) throw SEISPP::SeisppError(base_error
       + "data and error vector size does not match\nCannot continue");
    if(scale<0.0) throw SEISPP::SeisppError(base_error
       + "passed negative error scale factor - illegal input");
    double sumwt;
    //double sumsigma;
    double sumsigsq;
    double weight;
    double dwtsum[3];
    int i,k;
    double Nreal=static_cast<double>(d.size());
    for(k=0;k<3;++k) dwtsum[k]=0.0;
    /* Weighted average is used for no robust option.  We always
    compute this becauue the cost is close to zero */
    for(i=0,sumwt=0.0,sumsigsq=0.0;i<d.size();++i)
    {
        weight=1.0/(scale*e[i]);
        sumwt += weight;
        for(k=0;k<3;++k) dwtsum[k]+=d[i].n[k]*weight;
        //sumsigma += scale*e[i];
        sumsigsq+= scalesq*e[i]*e[i];
    }
    /* Normally the average would be computed as dwtsum[i]/sumwt, but since
    we are converting this to a unit vector the scale factor sumwt will
    be absorbed in the normalization */
    avg=UnitVector(dwtsum);
    ssq_avg=0.0;
    chisq_avg=0.0;
    for(i=0;i<d.size();++i)
    {
      double dtheta;
      dtheta=d[i].theta(avg);
      ssq_avg += dtheta*dtheta;
      chisq_avg += dtheta*dtheta/(e[i]*e[i]);   //note we don't apply the scale here 
    }
    ssq_avg/=(Nreal-1.0);   
    chisq_avg /= (Nreal-1.0);
    /* We initialize these here to zero as we exit below for straight average
       or median. This assures they are at least initialized to 0 */
    ssq_robust=0.0;
    chisq_robust=0.0;
    /* This formula copied from matlab script weighted_mean_azbin.m.  The
    script says it may be wrong.  Needs some theoretical checking before
    final use.
    err=sumsigma/Nreal;
    err/=sqrt(Nreal);
    */
    /* I think this may be the right formula */
    err=sqrt(sumsigsq/sumwt);
    if(pfunc==NONE)return;
    /* This block computes robust average using an m-estimator.
    Initial estimate is median of each component normalized to
    remain a unit vector. */
    vector<double> work;
    work.reserve(d.size());
    double xwork[3];
    for(k=0;k<3;++k)
    {
      work.clear();
      for(i=0;i<d.size();++i)
      {
        work.push_back(d[i].n[k]);
      }
      VectorStatistics<double> stats(work);
      xwork[k]=stats.median();
    }
    UnitVector med(xwork);
    if(pfunc==MEDIAN)
    {
      /* We return the same error estimate as mean for median.
      Not correct, but a selfish decision because I'm not sure how
      the median error should be calculated. */
      avg=med;
    }
    else
    {
      /* Set initial estimate to median */
      avg=med;
      vector<double> rwt,theta;
      rwt.reserve(d.size());
      UnitVector avg_last(avg);
      double deltvec;
      const int MAXITERATIONS(50);
      int niterations(0);
      do{
        double sumrwt;  //We compute this to avoid divide by zeros - sum of robust weights
      //DEBUG
      //cout << "pm_wt_avg - starting iteration "<<niterations<<endl;
        theta.clear();
        rwt.clear();
        //DEBUG
        //cout << "i rwt total_weight"<<endl;
        for(i=0;i<d.size();++i)
        {
          /* Compute the vector or robust weights*/
          double angle,scaled_angle;
          angle=avg.theta(d[i]);
          /* If the angle is more than 90 degrees for particle motion vectors
          we have to use the complementary angle.   This is effectively using
          the negative of d[i].   Necessary because the particle motion direction is a
          line not a direction. */
          if(angle>M_PI_2) angle=M_PI-angle;
          scaled_angle=angle/(scale*e[i]);
          //DEBUG
          //cout << "angle (deg)="<<deg(angle)<<" scaled angle="<<scaled_angle<<endl;
          switch(pfunc)
          {
            case BISQUARE:
              rwt.push_back(huber(scaled_angle));
              break;
            case HUBER:
            default:
              rwt.push_back(bisquare(scaled_angle));
          };
        }
        /* Now compute the weighted sum */
        for(k=0;k<3;++k)dwtsum[k]=0.0;   // use this again to accumulate
          for(i=0,sumwt=0.0,sumrwt=0.0;i<d.size();++i)
          {
            weight=rwt[i]/(scale*e[i]);
            //DEBUG
             //cout << i<<" "<<rwt[i]<<" "<<weight<<endl;
            for(k=0;k<3;++k)
            {
              dwtsum[k]+=d[i].n[k]*weight;
            }
            sumwt+=weight;
            sumrwt += rwt[i];
          }
        /*cout << "Unscaled sum of weighted vectors:  ";
        for(k=0;k<3;++k) cout << dwtsum[k]<<" ";
        cout <<endl;
        */
        for(k=0;k<3;++k)dwtsum[k]/=sumwt;
        //cout << "sumwt="<<sumwt<<endl;
        if(sumrwt<min_rwt_ratio) throw SeisppError(base_error
                + "convergence error in robust estimation loop\n"
                + "Residual weight sum dropped below min_rwt_ration threshold");
        UnitVector avgnow(dwtsum);
          //DEBUG
          //cout << "Weighted average iteration "<<niterations<<"="<< avg.n[0]<<" "<<avg.n[1]<<" "<<avg.n[2]<<endl;
        for(k=0,deltvec=0.0;k<3;++k)
        {
            double dvsq;
            dvsq=(avgnow.n[k] - avg_last.n[k]);
            deltvec += dvsq*dvsq;
        }
        deltvec=sqrt(deltvec);
          //DEBUG
          //cout << "deltvect="<<deltvec<<endl;
        avg_last=avgnow;
        avg=avgnow;
        ++niterations;
      }while(deltvec>FLT_EPSILON && (niterations<MAXITERATIONS));
      //DEBUG
      //cout << "Exited robust iteration with iteration count="<<niterations<<endl;
      /* This calculates the predicted uncertainty of theta angles.  As
      above it requires a theoretical check */
      for(i=0,sumsigsq=0.0;i<d.size();++i)
      {
        double esq;
        esq=e[i]*e[i]*scalesq;
        sumsigsq += esq*rwt[i]*rwt[i];
      }
      err=sumsigsq/sqrt(sumwt);
      /* We could put this initialization in the loop but broken out for clarity since
         these are private attributes of this object.  These are misfit measures stored
         as private attributes */
       ssq_robust=0.0;
       chisq_robust=0.0;
       for(i=0;i<d.size();++i)
       {
         double dtheta;
         dtheta=d[i].theta(avg);
         ssq_robust += dtheta*dtheta;
         double totalw;
         totalw=rwt[i]/e[i];
         chisq_robust += totalw*totalw*dtheta*dtheta;
       }
       ssq_robust /= (Nreal-1.0);
       chisq_robust /= (Nreal-1.0);
    }
  }catch(...){throw;};
}
pm_wt_avg::pm_wt_avg(const pm_wt_avg& parent)
{
    avg=parent.avg;
    err=parent.err;
    min_rwt_ratio=parent.min_rwt_ratio;
}
pm_wt_avg& pm_wt_avg::operator=(const pm_wt_avg& parent)
{
  if(this!=&parent)
  {
    avg=parent.avg;
    err=parent.err;
    min_rwt_ratio=parent.min_rwt_ratio;
  }
  return *this;
}
