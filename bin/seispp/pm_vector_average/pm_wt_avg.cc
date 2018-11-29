#include <float.h>
#include "pm_wt_avg.h"
#include "penalty_function.h"
#include "SeisppError.h"
#include "VectorStatistics.h"
using SEISPP::VectorStatistics;
using SEISPP::SeisppError;

pm_wt_avg::pm_wt_avg(vector<UnitVector>& d, vector<double> e, double scale,
    SupportedPenaltyFunctions pfunc)
{
  try{
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
    /* Weighted average is used for no robust average.  We always
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
      vector<double> rwt,theta;
      rwt.reserve(d.size());
      UnitVector avg_last(avg);
      double deltvec;
      const int MAXITERATIONS(50);
      int niterations(0);
      do{
        theta.clear();
        rwt.clear();
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
          switch(pfunc)
          {
            case BISQUARE:
              rwt.push_back(huber(scaled_angle));
              break;
            case HUBER:
            default:
              rwt.push_back(bisquare(scaled_angle));
          };
          /* Now compute the weighted sum */
          for(k=0;k<3;++k)dwtsum[k]=0.0;   // use this again to accumulate
          for(i=0,sumwt=0.0;i<d.size();++i)
          {
            weight=rwt[i]/(scale*e[i]);
            for(k=0;k<3;++k)
            {
              dwtsum[k]=d[i].n[k]*weight;
            }
            sumwt+=weight;
          }
          for(k=0;k<3;++k)dwtsum[k]/=sumwt;
          avg=UnitVector(dwtsum);
          for(k=0,deltvec=0.0;k<3;++k)
          {
            double dvsq;
            dvsq=(avg.n[k] - avg_last.n[k]);
            deltvec += dvsq*dvsq;
          }
          deltvec=sqrt(deltvec);
          ++niterations;
        }
      }while(deltvec<FLT_EPSILON && (niterations<MAXITERATIONS));
      /* This calculates the predicted uncertainty of theta angles.  As
      above it requires a theoretical check */
      for(i=0,sumsigsq=0.0;i<d.size();++i)
      {
        double esq;
        esq=e[i]*e[i]*scalesq;
        sumsigsq += esq*rwt[i]*rwt[i];
      }
      err=sumsigsq/sqrt(sumwt);
    }
  }catch(...){throw;};
}
pm_wt_avg::pm_wt_avg(const pm_wt_avg& parent)
{
    avg=parent.avg;
    err=parent.err;
}
pm_wt_avg& pm_wt_avg::operator=(const pm_wt_avg& parent)
{
  if(this!=&parent)
  {
    avg=parent.avg;
    err=parent.err;
  }
  return *this;
}
