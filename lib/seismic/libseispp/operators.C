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

Time_Series& Time_Series::operator=(const Time_Series& tsi)
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

Three_Component_Seismogram& Three_Component_Seismogram::operator
		= (const Three_Component_Seismogram& seisin)
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
// assignment operators for ensemble objects (could be templated)
//
Time_Series_Ensemble& Time_Series_Ensemble::operator=(const Time_Series_Ensemble& tseold)
{
	if(this!=&tseold)
	{
		pf = pfdup(tseold.pf);
		int nmembers=tseold.tse.size();
		tse.reserve(nmembers);
		for(int i=0; i<nmembers; ++i)
			tse.push_back(tseold.tse[i]);
	}
	return(*this);
}
Three_Component_Ensemble& Three_Component_Ensemble::operator=(const Three_Component_Ensemble& tseold)
{
	if(this!=&tseold)
	{
		pf = pfdup(tseold.pf);
		int nmembers=tseold.tcse.size();
		tcse.reserve(nmembers);
		for(int i=0; i<nmembers; ++i)
			tcse.push_back(tseold.tcse[i]);
	}
	return(*this);
}
} // Termination of namespace SEISPP definitions
