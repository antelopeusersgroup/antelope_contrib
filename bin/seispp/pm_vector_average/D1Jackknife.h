#ifndef _D1JACKKNIFE_H_
#define _D1JACKKNIFE_H_
#include <utility>
#include <vector>
#include "UnitVector.h"
using namespace std;
/*! \brief Generic object to compute the delete-one jackknife of a set of data.

The jackknife is a well-known nonparamwtric statistical tool to provide
the mean and standard deviation of a set of numbers without an assumption of
the pdf of the errors.   An alternative is the bootstrap, but the bootstrap is
much more expensive to compute.  Use the delete-one jackknife if the
input numbers are slow to compute.  The object uses construction is initilalization.
In this case that means the jackknife mean and variance are computed by
the constructor.  The method are simple getters. */
template <typename T> class D1Jackknife
{
public:
  /*! Construct from a vector of numbers.

  The jackknife is a well-known nonparametric statistical tool to provide estimates
  of the mean and standard deviation of a set of numbers.

  \param d - is the input vector of data to use for computation

  */
  D1Jackknife(vector<T>& d);
  /*! Construct from precoputd vctor of mean of all data.

  Sometims it is more convenient to precompute the vector of delete one mean
  values and the mean of all data.   The defnitive example here is the
  UnitVector specialization template where the data are computed by a
  more elaborate procedure.

  \param d - is the input vector of delete one mean esimates to use for computation
  \param mean0 - mean computed from all samples

  \return std::pair containing jackknife mean as first and stdev as second.
  */
  D1Jackknife(vector<T>& pseudovalues,T mean0);
  T mean(){return avg;};
  T variance(){return var;};
private:
  T avg;
  T var;
};

/* This is a copy from VectorStatistics.h removing the binding to the
objct. */
namespace SimpleStatistics {
template <class T> T mean(vector<T>& d)
{
  T result;
  result=0;
  for(int i=0;i<d.size();++i)
  {
    result += d[i];
  }
  return (result/d.size());
}
template <class T> T variance(vector<T>& d,double d0)
{
  T result;
  result=0;
  for(int i=0;i<d.size();++i)
  {
    double sq;
    sq=(d[i]-d0)*(d[i]-d0);
    result += sq;
  }
  return (result/(d.size()-1));
}
}

template <typename T> D1Jackknife<T>::D1Jackknife(vector<T>& d)
{
  try{
    T mean0;
    int i,ii;
    vector<T> pseudovalues;
    vector<T> work;
    int N(d.size());
    pseudovalues.reserve(N);
    work.reserve(N-1);
    /* First we compute mean of all data - theta0*/
    mean0=SimpleStatistics::mean<T>(d);
    /* Now compute pseudovalues */
    for(i=0;i<N;++i)
    {
      double mtmp;
      work.clear();
      for(ii=0;ii<N;++i)
      {
        if(ii!=i) work.push_back(d[ii]);
      }
      mtmp=SimpleStatistics::mean<T>(work);
      pseudovalues.push_back(mtmp);
    }
    /* This will be the jackknife mean */
    avg=SimpleStatistics::mean<T>(pseudovalues);
    /* and this the jackknife variance */
    var=SimpleStatistics::variance<T>(pseudovalues,mean0);
  }catch(...){throw;};
}

template <typename T> D1Jackknife<T>::D1Jackknife(vector<T>& d,T mean0)
{
  try{
    int N(d.size());
    double Nf(static_cast<double>(N));
    vector<T> pseudovalues;
    pseudovalues.reserve(N);
    for(int i=0;i<N;++i)
    {
      T  val;
      val = Nf*mean0 - (Nf-1)*d[i];
      pseudovalues.push_back(val);
    }
    /* This will be the jackknife mean */
    avg=SimpleStatistics::mean<T>(pseudovalues);
    /* and this the jackknife variance */
    var=SimpleStatistics::variance<T>(pseudovalues,mean0);
    return(pair<T,T>(mean,sqrt(var)));
  }catch(...){throw;};
}
/*! \brief Specialization for UnitVectors for particle motion analysis.

The approach used by this constructor is not the standard formula.
The mean is computed normally although in a vector sense.  The error,
however, is computed as an estimate of the variance of angles relative
to the mean (angle in the formula cos theta = u_1 \dot u_2 for unit
vectors u_1 and u_2.

\param d is expected to contain the vector of pseudovalues.
\param mean0 is assumed to be the mean computed from all data.
*/
template<> D1Jackknife<UnitVector>::D1Jackknife(vector<UnitVector>& d, UnitVector mean0)
{
  try{
    int i,k;
    int N(d.size());
    double Nf(static_cast<double>(N));
    vector<UnitVector> pseudovalues;
    pseudovalues.reserve(N);
    for(i=0;i<N;++i)
    {
      UnitVector vtmp;
      for(k=0;k<3;++k) vtmp.n[k] = Nf*mean0.n[k] - (Nf-1.0)*d[i].n[k];
      pseudovalues.push_back(vtmp);
    }
    /* This is the mean calculation.   Chose to not implement a operator+
    for the UnitVector object due to the confusion of normalization required
    of unit vectors.  Hence the mean and variance are both computed with
    loops over k */
    for(k=0;k<3;++k)avg.n[k]=0.0;
    for(i=0;i<N;++i)
    {
      for(k=0;k<3;++k)
      {
        avg.n[k] += d[i].n[k];
      }
    }
    for(k=0;k<3;++k) avg.n[k]/=Nf;
    /* now variance - as noted odd because it reduces to angles.   To mesh
    with this template we place that number in n[0] of the var variable.
    Definitely a design problem, but don't see an alternative without
    breaking the simplicity of the more generic algorithm. */
    vector<double> theta;
    theta.reserve(N);
    for(i=0;i<N;++i)
    {
      double thistheta;
      thistheta=d[i].theta(mean0);
      theta.push_back(thistheta);
    }
    for(k=0;k<3;++k)var.n[k]=0.0;  // initialize n - important for 1 and 2 components
    double ssq;
    ssq=0.0;
    for(i=0;i<N;++i) ssq += theta[i]*theta[i];
    var.n[0] = ssq/(Nf - 1.0);
  }catch(...){throw;};
}

#endif
