#include <math.h>
#include "UnitVector.h"
#include "SeisppError.h"
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
  return(acos(dp));
}
double UnitVector::dot_product(UnitVector& other)
{
  double dprod(0.0);
  for(int k=0;k<3;++k)dprod += n[k]*other.n[k];
  return dprod;
}
