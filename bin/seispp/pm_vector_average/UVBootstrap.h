#ifndef _UVBOOTSTRAP_H_
#define _UVBOOTSTRAP_H_
#include <vector>
#include "UnitVector.h"
using namespace std;
/* This is a specialized implementation of the bootstrap to compute confidence intervals
   for angle deviations computed by dot products of a collection of unit vectors.
   It was derived from a similar implementation in the ParticleMotionTools
   libmwtpp library called Vector3Vector3DBootstrapError.

   */
class UVBootstrap
{
  public:
    /*! Construct from a matrix of input values.

    This is the primary constructor for this object.  It uses construction
    is initialization, which in this case means the constructor does the
    bootstrap error estimation.  Estimates bootstrap errors at confidence
    level specified.  This program uses a delete and replace algorithm for
    every resampling.  i.e. randomly grab one vector from the trial set
    number_of_trials times with replacement after each draw.  A robust
    estimator is used for each resampled set.

    \param x - input data of unit vectors
    \param xerr - parallel vector to x of error estimates in theta
       angle for each x_i.  Internally alldata are scaled by wt_i=1/xerr_i
      for base weighting of average
    \param pen - defines panalty function to use for robust estimator
      (allowed values defined in pm_wt_avg.h)
    \param scale scale factor for xerr values passed to pm_wt_avg
    \param probability sets a threshold level used for setting the robust
       scale factor in pm_wt_avg.  Pass through to that algorithm.
    \param mrwtr mrwtr controls an exit condition for unstable robust weighting.
      When the ratio of the sum of residual weights (weights are always
      near one when the scaled residual is small) to the total number of
      data points falls below this value the constructor will throw a
      SeisppError exception with a diagnostic message.
    \param - confidence is the confidence level computed. This must be a
       number greater than 0 and less than 1 or an error is thrown.
    \number_trials - number of resampling trials for the bootstrap.
    */
    UVBootstrap(const vector<UnitVector>& x, const vector<double>& xerr,
        const SupportedPenaltyFunctions pen, const double scale,
        const double probability, const double mrwtr,
        const double confidence, const int number_trials);
    UnitVector mean_vector()
    {
      return mean;
    };
    double angle_error()
    {
      return aci;
    };
    double confidence_level()
    {
      return cl;
    };
  private:
    /*! bootstrap median of 3D vectors */
    UnitVector mean;
    /* Estimated confidence interval.*/
    double aci;
    /* input confidence level */
    double cl;
};
/*! This is a routine in libmwtpp - used hre to avoid needing to
have an include statement for Vector3DBootstrapError from which this
was derived */
int random_array_index(int range);
#endif
