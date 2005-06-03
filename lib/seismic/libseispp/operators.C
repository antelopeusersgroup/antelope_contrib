#include "seispp.h"
namespace SEISPP
{
/* This file contains operators for several different object.  
All are very similar.  

A maintenance issue is that most functions contain a pfdup 
call.  This is a hidden method to clone the Metadata connected
to that object.  The problem is if the Metadata implementation
changes this will break this code.
*/

TimeSeries& TimeSeries::operator=(const TimeSeries& tsi)
{
	if(this!=&tsi)
	{
		//Can't figure out how to do this with the 
		// metadata abstraction.  Have to use the private pf 
		pf=pfdup(tsi.pf);
		live=tsi.live;
		dt=tsi.dt;
		t0=tsi.t0;
		ns=tsi.ns;
		tref=tsi.tref;
		if(tsi.live)
		{
			s=tsi.s;
		}
	}
	return(*this);
}			

ThreeComponentSeismogram& ThreeComponentSeismogram::operator
		= (const ThreeComponentSeismogram& seisin)
{
	if(this!=&seisin)
	{
		pf = pfdup(seisin.pf);
		live=seisin.live;
		dt=seisin.dt;
		t0=seisin.t0;
		ns=seisin.ns;
		tref=seisin.tref;
		components_are_orthogonal=seisin.components_are_orthogonal;
		components_are_cardinal=seisin.components_are_cardinal;
		for(int i=0;i<3;++i)
		{
			for(int j=0;j<3;++j)
			{
				tmatrix[i][j]=seisin.tmatrix[i][j];
			}
		}
		u=seisin.u;
	}
	return(*this);
}
//
// assignment operators for ensemble objects (could maybe be templated)
//
TimeSeriesEnsemble& TimeSeriesEnsemble::operator=(const TimeSeriesEnsemble& tseold)
{
	if(this!=&tseold)
	{
		pf = pfdup(tseold.pf);
		int nmembers=tseold.member.size();
		member.reserve(nmembers);
		for(int i=0; i<nmembers; ++i)
			member.push_back(tseold.member[i]);
	}
	return(*this);
}
ThreeComponentEnsemble& ThreeComponentEnsemble::operator=(const ThreeComponentEnsemble& tseold)
{
	if(this!=&tseold)
	{
		pf = pfdup(tseold.pf);
		int nmembers=tseold.member.size();
		member.reserve(nmembers);
		for(int i=0; i<nmembers; ++i)
			member.push_back(tseold.member[i]);
	}
	return(*this);
}
} // Termination of namespace SEISPP definitions
