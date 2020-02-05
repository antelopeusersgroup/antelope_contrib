#include <vector>
#include <tuple>
#include <algorithm>
#include <math.h>
#include "perf.h"
#include "SeisppError.h"
#include "pm_wt_avg.h"
#include "VectorStatistics.h"
#include "UVBootstrap.h"
using namespace std;
using namespace SEISPP;
/* Important - through routine assumes x vectors are unit vectors.   Perhaps should verify this, but
   for efficiency probably won't do that. */

UVBootstrap::UVBootstrap(const vector<UnitVector>& x, const vector<double>& xerr,
    const SupportedPenaltyFunctions pen, const double scale,
    const double probability, const double mrwtr,
    const double confidence, const int number_trials)
{
  const string base_error("UVBootstrap constructor:  ");
  /* Sanity check */
  if((confidence>1.0) || (confidence<=0.0))
  {
    throw SeisppError(base_error
        + "Illegal confidence interval requested - must be probability level (i.e greater than 0 and less than 1.0)");
  }
  cl=confidence;
  int nx=x.size();
  int i,j,k;
  /* This is a large memory algorithm.  We load the trials into this matrix */
  vector<UnitVector> trials;
  trials.reserve(number_trials);
  int row;
  vector<UnitVector> resampled;
  resampled.reserve(number_trials);
  vector<double> xerr_resamp;
  xerr_resamp.reserve(number_trials);
  for(i=0;i<number_trials;++i)
  {
    resampled.clear();
    xerr_resamp.clear();
    for(j=0;j<nx;++j)
    {
      row=random_array_index(nx);
      resampled.push_back(x[row]);
      xerr_resamp.push_back(xerr[row]);
    }
    pm_wt_avg robust_avg(resampled,xerr_resamp,scale,pen,probability,mrwtr);
    trials.push_back(robust_avg.average());
  }
  /*  Now from the suite of resampled trail values we need to
  compute the boostrap average and errors for angles expressed
  as dot products.  First compute the average of averages */
  vector<double> work;
  work.reserve(number_trials);
  /* This initialization might not be necessary*/
  for(i=0;i<number_trials;++i) work.push_back(0.0);
  double xbar[3];
  //DEBUG
  //cout << "Computing mean vector"<<endl;
  for(k=0;k<3;++k)
  {
    for(i=0;i<number_trials;++i) work[i]=trials[i].n[k];
    VectorStatistics<double> vswrk(work);
    xbar[k]=vswrk.mean();
  }
  /* The unit vector constructor here is assumed to normalize the
  input vector to create a unit vector - i.e. scales by norm(xbar) */
  this->mean=UnitVector(xbar);
  /* Reuse the work vector to hold theta angles between each trail
  vector and the mean we just computed */
  for(i=0;i<number_trials;++i)
    work[i]=this->mean.theta(trials[i]);
  sort(work.begin(),work.end());
  /* This angle error is one sided - we estimate the probability
     the uncertainty in theta angles is less than the
     confidence value */
  int nconf=rint(((double)number_trials)*confidence);
  //DEBUG
  //cout << "Computed position for confidence="<<nconf<<" of "<<number_trials<<endl;
  if(nconf==number_trials) nconf=number_trials-1;   // Silently handled for roundoff errors
  if(nconf>=number_trials)
    throw SeisppError(base_error + "Coding problem - computed position of confidence outside array bound");
  aci=work[nconf];
}
