#include <math.h>
#include "UnitVector.h"
#include "SeisppError.h"
using namespace std;
using SEISPP::SeisppError;
UnitVector::UnitVector(double *x)
{
  int k;
  double normx(0.0);
  for(k=0;k<3;++k) normx+=x[k]*x[k];
  normx=sqrt(normx);
  for(k=0;k<3;++k)n[k]=x[k]/normx;
}
UnitVector::UnitVector(std::vector<double>& x)
{
  if(x.size()!=3)
    throw SeisppError("UnitVector vector container constructor:  input vector size must be 3");
  int k;
  double normx(0.0);
  for(k=0;k<3;++k) normx+=x[k]*x[k];
  normx=sqrt(normx);
  for(k=0;k<3;++k)n[k]=x[k]/normx;
}
UnitVector::UnitVector(const UnitVector& parent)
{
  for(int k=0;k<3;++k) n[k]=parent.n[k];
}
UnitVector& UnitVector::operator=(const UnitVector& parent)
{
  if(this!=&parent)
  {
    for(int k=0;k<3;++k) n[k]=parent.n[k];
  }
  return *this;
}
double UnitVector::theta(UnitVector& other)
{
  double dp;
  dp=this->dot_product(other);
  /* acos will throw a nan if dp is invalid.  Hence, this
   * simpl trap */
  if(dp>=1.0)
      return M_PI_2;
  else
      return(acos(dp));
}
double UnitVector::dot_product(UnitVector& other)
{
  double dprod(0.0);
  for(int k=0;k<3;++k)dprod += n[k]*other.n[k];
  return dprod;
}
ostream& operator<<(ostream& ofs, const UnitVector& u)
{
  int k;
  for(k=0;k<2;++k)ofs<<u.n[k]<<" ";
  ofs<<u.n[2];
  return ofs;
}
