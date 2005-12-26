#include <algorithm>
#include <string>
#include "seispp.h"
using namespace std;
using namespace SEISPP;
/* this file contains some prototypes for simple sorts of ensembles.
I think it highly likely these could be made into templates, but for
now will do this for some common examples */

// This defines a functor for sorting by sta:chan
struct less_stachan : public binary_function<TimeSeries,TimeSeries,bool> {
	bool operator()(TimeSeries x, TimeSeries y)
	{
		string xsta=x.get_string("sta");
		string ysta=y.get_string("sta");
		if(xsta<ysta)
			return true;
		else if(xsta==ysta)
		{
			string xchan=x.get_string("chan");
			string ychan=y.get_string("chan");
			if(xchan<ychan)
				return true;
		}
		return false;
	}
};

void StaChanSort(TimeSeriesEnsemble& ensemble)
{
	sort(ensemble.member.begin(),ensemble.member.end(),less_stachan());
}
