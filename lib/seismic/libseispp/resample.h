#ifndef _RESAMPLE_H_
#define _RESAMPLE_H_
#include <map>
#include <vector>
#include "stock.h"
#include "pf.h"
#include "TimeSeries.h"

// Need this when I add this to seispp
namespace SEISPP {
using namespace std;
using namespace SEISPP;

// Defines a data structure for a vector of doubles after 
// applying a decimator
class DecimatedVector
{
public:
	int lag; // lag in samples relative to parent vector (always positive)
	vector<double>d;
	DecimatedVector(){lag=0;};
	DecimatedVector(int ns);
	DecimatedVector(const DecimatedVector&);
	DecimatedVector& operator=(const DecimatedVector&);
};
	
class Decimator
{
public:
	double decfac;
	Decimator();
	Decimator(string fname, double decfac_expected); 
	Decimator(const Decimator&);
	Decimator& operator=(const Decimator&);
	Decimator(string fnm);
	DecimatedVector& apply(int ns, double *di);  // apply using default for trim
	DecimatedVector& apply(int ns, double *di,bool trim);  // overload switched for trim
	DecimatedVector& apply(vector<double>di,bool trim);
	
private:
	vector<double>coefs;
	int lag;  // position in coefs of zero lag point
};

class ResampleOperator
{
public:
	double low;
	double high;
	double exact;
	list <Decimator> declist;

	ResampleOperator(double e, double l, double h);
	ResampleOperator(double e, Pf *pf);
	ResampleOperator(const ResampleOperator& ro);
	ResampleOperator& operator= (const ResampleOperator&);
	// main method.  Applies decimators to vector s and returns
	// an stl vector as the result
	DecimatedVector& apply(int ns,double *s,double dtin, double dtout,
		bool trim);
};
class Interval
{
public:
	double high;
	double low;
};

class IntervalCompare
{
public:
	bool operator()(const Interval r1, const Interval r2) const
	{return(r1.high<r2.low);};
};

/*
class RangeCompare
{
public:
	bool operator()(const ResampleOperator r1, const ResampleOperator r2) const
	{return(r1.high<r2.low);};
};
*/

class ResamplingDefinitions
{
public:
	map<Interval,ResampleOperator,IntervalCompare> decset;
	ResamplingDefinitions(Pf *pf);
};


// function prototypes that use the objects defined above

TimeSeries ResampleTimeSeries(TimeSeries& ts, 
		ResamplingDefinitions& rd,
			double dtout,
				bool trim);


} // end namespace encapsulation
#endif
