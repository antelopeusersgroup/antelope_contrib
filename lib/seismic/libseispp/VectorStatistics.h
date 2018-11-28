#include <vector>
#include <algorithm>
#include "SeisppError.h"
namespace SEISPP 
{
using namespace std;
using namespace SEISPP;

/*! \brief Generic object to compute common robust statistics from a vector container of data. 

Robust estimators commonly use statistics based on ranked data.  This fits naturally with an
STL vector container that is by definition sortable by a standard method.  This object 
has methods that return common statistics derived from sorted vector data. */
template <class T> class VectorStatistics
{
public:
	/*! Primary constructor.  

	\param din is vector container from which statistics are to be derived.  Currently
	assume default sort is used.  Probably should have an optional order template parameter
	for the container. */
	VectorStatistics(vector<T> din);
	/*!  Construct from a C style pointer to an array of T.*/
	VectorStatistics(T *din,int n);
	/*! Return median */
	T median();
	/*! Return the mean */
	T mean();
	/*! Return the lower quartile. */
	T q1_4();
	/*! Return the upper (3/4) quartile. */
	T q3_4();
	/*! Return the interquartile (q3/4 - q1/4) */
	T interquartile();
	/*! Return the median absolute distance robust measure of spread. */
	T mad(T center);
	/*! Return sum of squares. */
	T ssq();
	/*! Return largest value in the data set. */
	T upper_bound();
	/*! Return smallest value in the data set. */
	T lower_bound();
	/*! Return full range of data (largest - smallest) */
	T range();
private:
	vector<T> d;
};

template <class T> VectorStatistics<T>::VectorStatistics(vector<T> din)
{
	if(din.size()<=1) throw SeisppError(string("VectorStatistics constructor:  ")
		+ "input vector has insufficient data to compute statistics");
	d=din;
	sort(d.begin(),d.end());
}
template <class T> VectorStatistics<T>::VectorStatistics(T *din,int n)
{
	if(n<=1) throw SeisppError(string("VectorStatistics constructor:  ")
		+ "input vector has insufficient data to compute statistics");
	d.reserve(n);
	for(int i=0;i<n;++i) d.push_back(din[i]);
	sort(d.begin(),d.end());
}
template <class T> T VectorStatistics<T>::median()
{
	int count=d.size();
	int medposition=count/2;
	if(count%2)
		return(d[medposition]);
	else
		return( (d[medposition]+d[medposition-1])/2 );
}
template <class T> T VectorStatistics<T>::mean()
{
	T result;
	result=0;
	for(int i=0;i<d.size();++i)
	{
		result += d[i];
	}
	return (result/d.size());
}
template <class T> T VectorStatistics<T>::q1_4()
{
	int n=d.size();
	double result;
	if(n<4)
		return(d[0]);
	else
	{
		int nover4=(n-1)/4;
		switch(n%4)
		{
		case(0):
			result=static_cast<double>(d[nover4]);
			break;
		case(1):
			result=0.75*static_cast<double>(d[nover4]) + 0.25*static_cast<double>(d[nover4+1]);
			break;
		case(2):
			result=static_cast<double>(d[nover4]) + static_cast<double>(d[nover4+1]);
			result /= 2.0;
			break;
		case(3):
			result=0.25*static_cast<double>(d[nover4]) + 0.75*static_cast<double>(d[nover4+1]);
		}
	}
	return(static_cast<T>(result));
}
template <class T> T VectorStatistics<T>::q3_4()
{
	int n=d.size();
	double result;
	if(n<4)
		return(d[n-1]);
	else
	{
		int n3_4=3*(n-1)/4;
		switch(n%4)
		{
		case(0):
			result=static_cast<double>(d[n3_4]);
			break;
		case(1):
			result=0.75*static_cast<double>(d[n3_4]) + 0.25*static_cast<double>(d[n3_4+1]);
			break;
		case(2):
			result=static_cast<double>(d[n3_4]) + static_cast<double>(d[n3_4+1]);
			result /= 2.0;
			break;
		case(3):
			result=0.25*static_cast<double>(d[n3_4]) + 0.75*static_cast<double>(d[n3_4+1]);
		}
	}
	return(static_cast<T>(result));
}
template <class T> T VectorStatistics<T>::interquartile()
{
	T result;
	T d1_4,d3_4;
	d1_4=this->q1_4();
	d3_4=this->q3_4();
	return(d3_4 - d1_4);
}
template <class T> T VectorStatistics<T>::mad(T center)
{
	vector<T> absdiff;
	int n=d.size();
	int i;
	for(i=0;i<n;++i)
	{
		T diff;
		diff=d[i]-center;
		if(diff<0) diff=-diff;
		absdiff.push_back(diff);
	}
	VectorStatistics<T> result(absdiff);
	return(result.median());
}
template <class T> T VectorStatistics<T>::ssq()
{
	T result;
	int i;
	for(i=0;i<d.size();++i) result=d[i]*d[i];
	return(result);
}
template <class T> T VectorStatistics<T>::upper_bound()
{
	return(d[d.size()-1]);
}
template <class T> T VectorStatistics<T>::lower_bound()
{
	return(d[0]);
}
template <class T> T VectorStatistics<T>::range()
{
	T result;
	result=this->upper_bound();
	result-=this->lower_bound();
	return(result);
}
} /* End SEISPP namespace encapsulation */
