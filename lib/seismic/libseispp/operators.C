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
/*  Sum operator for TimeSeries object */

void TimeSeries::operator+=(const TimeSeries& data)
{
	int i,i0,iend,ntosum;
	int j,j0=0,jend=ns;
	// Sun's compiler complains about const objects without this.
	TimeSeries& d=const_cast<TimeSeries&>(data);
	// Silently do nothing if d is marked dead
	if(!d.live) return;
	// Silently do nothing if d does not overlap with data to contain sum
	if( (const_cast<double>(d.endtime())<t0) 
		|| (d.t0>(this->endtime())) ) return;
	if(d.tref!=(this->tref)) 
		throw SeisppError("TimeSeries += operator cannot handle data with inconsistent time base\n");
	//
	// First we have to determine range fo sum for d into this 
	//
	i0=d.sample_number(this->t0);
	if(i0<0)
	{
		j=-i0;
		i0=0;
	}
	iend=d.sample_number(this->endtime());
	if(iend>(d.ns-1))
	{
		iend=d.ns-1;
	}
	//
	// IMPORTANT:  This algorithm simply assumes zero_gaps has been called
	// and/or d was checked for gaps befor calling this operatr.  
	// It will produce garbage for most raw gap (sample level) marking schemes
	//
	for(i=i0,j=j0;i<iend;++i,++j)
		this->s[j]+=d.s[i];
}
	
} // Termination of namespace SEISPP definitions
