#ifndef _RESAMPLE_H_
#define _RESAMPLE_H_
#include <map>
#include <vector>
#include "pf.h"
#include "seispp.h"

// Need this when I add this to seispp
//namespace SEISPP {

// Defines a data structure for a vector of doubles after 
// applying a decimator
class Decimated_vector
{
public:
	int lag; // lag in samples relative to parent vector (always positive)
	vector<double>d;
	Decimated_vector(){lag=0;};
	Decimated_vector(int ns);
	Decimated_vector(const Decimated_vector&);
	Decimated_vector& operator=(const Decimated_vector&);
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
	Decimated_vector& apply(int ns, double *di);  // apply using default for trim
	Decimated_vector& apply(int ns, double *di,bool trim);  // overload switched for trim
	Decimated_vector& apply(vector<double>di,bool trim);
	
private:
	vector<double>coefs;
	int lag;  // position in coefs of zero lag point
};

class Resample_Operator
{
public:
	double low;
	double high;
	double exact;
	list <Decimator> declist;

	Resample_Operator(double e, double l, double h);
	Resample_Operator(double e, Pf *pf);
	Resample_Operator(const Resample_Operator& ro);
	Resample_Operator& operator= (const Resample_Operator&);
	// main method.  Applies decimators to vector s and returns
	// an stl vector as the result
	Decimated_vector& apply(int ns,double *s,double dtin, double dtout,
		bool trim);
};
class Interval
{
public:
	double high;
	double low;
};

class Interval_Cmp
{
public:
	bool operator()(const Interval r1, const Interval r2) const
	{return(r1.high<r2.low);};
};

/*
class Range_Cmp
{
public:
	bool operator()(const Resample_Operator r1, const Resample_Operator r2) const
	{return(r1.high<r2.low);};
};
*/

class Resampling_Definitions
{
public:
	map<Interval,Resample_Operator,Interval_Cmp> decset;
	Resampling_Definitions(Pf *pf);
};


// function prototypes that use the objects defined above

Time_Series Resample_Time_Series(Time_Series& ts, 
		Resampling_Definitions& rd,
			double dtout,
				bool trim);


//} // end namespace encapsulation
#endif
