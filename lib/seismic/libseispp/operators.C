#include "seispp.h"
Time_Series& Time_Series::operator=(const Time_Series& tsi)
{
	if(this!=&tsi)
	{
		live=tsi.live;
		dt=tsi.dt;
		t0=tsi.t0;
		ns=tsi.ns;
		tref=tsi.tref;
		md=tsi.md;  //Metadata assignment operator must create destroy
		if(s!=NULL) delete [] s;
		if(live)
		{
			s=new double[ns];
			for(int i=0;i<tsi.ns;++i) s[i]=tsi.s[i];
		}
		else
			s=NULL;
		
	}
	return(*this);
}			

Three_Component_Seismogram& Three_Component_Seismogram::operator
		= (const Three_Component_Seismogram& seisin)
{
	if(this!=&seisin)
	{
		live=seisin.live;
		md=seisin.md;
		dt=seisin.dt;
		t0=seisin.t0;
		ns=seisin.ns;
		tref=seisin.tref;
		components_are_orthogonal=seisin.components_are_orthogonal;
		components_are_cardinal=seisin.components_are_cardinal;
		for(int i=0;i<3;++i)
		{
			x[i]=seisin.x[i];
			for(int j=0;j<3;++j)
			{
				tmatrix[i][j]=seisin.tmatrix[i][j];
			}
		}
	}
	return(*this);
}
