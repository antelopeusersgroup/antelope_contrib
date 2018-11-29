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
    an initial estimate.
  */
  pm_wt_avg(vector<UnitVector>& d, vector<double> e, double scale,
      SupportedPenaltyFunctions pen=NONE);
  pm_wt_avg(const pm_wt_avg& parent);
  pm_wt_avg& operator=(const pm_wt_avg& parent);
  UnitVector average(){return avg;};
  double sigma(){return err;};
private:
  UnitVector avg;
  /*! This is estimated error (in radians) of unit vector estimate on the
  unit sphere.  i.e. the angle we would compute if we could know
  x_estimated \dot x_true where x_true is the (unknowable) true unit
  vector.  */
  double err;
};
