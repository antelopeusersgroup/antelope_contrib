#include "seispp.h"
namespace SEISPP
{

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
} // Termination of namespace SEISPP definitions
