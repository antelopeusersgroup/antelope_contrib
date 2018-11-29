#ifndef _UNITVECTOR_H_
#define _UNITVECTOR_H_
#include <vector>
class UnitVector
{
public:
  double n[3];
  UnitVector()
  {
    for(int k=0;k<3;++k) n[k]=0.0;
  }
  /*! Construct from a C array of values.

  This constructor assumes x is of length 3.  It does not checking.
  It computes the unit normal vector pointing in the direction of x. */
  UnitVector(double *x);
  UnitVector(std::vector<double>& x);
  UnitVector(const UnitVector& parent);
  UnitVector& operator=(const UnitVector& parent);
  double theta(UnitVector& other);
  double dot_product(UnitVector& other);
};
#endif
