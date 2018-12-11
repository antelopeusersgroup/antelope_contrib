#include <vector>
#include "UnitVector.h"
enum SupportedPenaltyFunctions {HUBER,BISQUARE,MEDIAN,NONE};
using namespace std;
class pm_wt_avg
{
public:
  /*! Default constructor - initializes attriutes to 0 but does nothing.
  Used only as a necessary place holder in some contexts */
  pm_wt_avg();
  /*! Main constructor.

  This constructor uses construction is initialization.   The input data
  vectors d and e are processed to yield a weighted average estimate of
  an average unit vector (held in private and extractable by a method below).

  \param d - is the data vector of normal vectors.
  \param e - is a parallel vector to d containing angle errors (in radians)
    of each vector in d.   Note the assumption is the angle involved is
    angle of cos theta = x.y formula. The algortithm used to compute
    the error in the mean here assumes these are scaled in a consistent
    way.  We compute a standard weighted mean using sum d_i/e_i/sum 1/e_i
  \param scale is a multiplier used to convert e estimates to 1/sigma
    for a normal distribution.  Internally each e_i is converted to a
    weight w_i=1/(e_i*scale).   This is needed since mwpm uses a bootstrap
    error method with a specified confidence.
  \param pen - sets the penalty function to use for robust averaging.
    Allowed choices are BISQUARE and HUBER.   Use NONE for simple
    average.   Robust averages are m-estimators using a median as
    an initial estimate.  Default is NONE.
  \param mrwtr controls an exit condition for unstable robust weighting.
    When the ratio of the sum of residual weights (weights are always 
    near one when the scaled residual is small) to the total number of
    data points falls below this value the constructor will throw a 
    SeisppError exception with a diagnostic message.   Default is 0.10
  */
  pm_wt_avg(vector<UnitVector>& d, vector<double>& e, double scale,
      SupportedPenaltyFunctions pen=NONE,double mrwtr=0.1);
  pm_wt_avg(const pm_wt_avg& parent);
  pm_wt_avg& operator=(const pm_wt_avg& parent);
  UnitVector average(){return avg;};
  double sigma(){return err;};
  double average_ssq(){return deg(sqrt(ssq_avg));};
  double average_chisq(){return chisq_avg;};
  double robust_rms(){return deg(sqrt(ssq_robust));};
  double robust_chisq(){return chisq_robust;};
private:
  UnitVector avg;
  /*! This is estimated error (in radians) of unit vector estimate on the
  unit sphere.  i.e. the angle we would compute if we could know
  x_estimated \dot x_true where x_true is the (unknowable) true unit
  vector.  */
  double err;
  /* This parameter controls exit condition for unstable robust weighting.
   * When the ratio of the sum residual weights (normally near one) divided
   * by the number of data points drops below this number the constructor
   * will throw an exception. */
  double min_rwt_ratio;
  /* These are computed from data.  The _avg values are computed from
     arthmetic mean.   The _robust values are computed from robust 
     estimate using robust weights.   chisq values are scaled by 
     input sigma values while rms values have radian square units. */
  double ssq_avg;
  double chisq_avg;
  double ssq_robust;
  double chisq_robust;
};
