#include <sstream>
#include "PfStyleMetadata.h"
#include "gclgrid.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
/*! \brief Object to apply tapers at the edges of GCLfield objects.  

In wavefield imaging the edges of the the data projected into the image
volumn need to be tapered to avoid edges. This processing objects
implements this process through a clean interface.  The object
is constructed from a parameter file or Metadata object which allows
a completely general description of it's properties.  

The current implementation uses linear tapers at the edges or none
but the interface is general. */
class TaperGrid
{
public:
	TaperGrid(Metadata& md);
	TaperGrid(string fname);
	TaperGrid(const TaperGrid& parent);
	TaperGrid& operator=(const TaperGrid& parent);
	void apply(GCLscalarfield3d& g);
	void apply(GCLvectorfield3d& g);
private:
	/*! These contain value to assign to low end of tapers.

	This implementation uses only linear tapers.  These define the value
	each taper will fall to at the extremes. e.g. xlvalues[0] is the value
	of the taper for the x1 coordinate at 0 while xhvalues[0] is the value
	of the taper at the opposite end of this coordinate axis x1[n3-1].*/
	double xlvalues[3],xhvalues[3];
	/*! Contains linear taper width parameters.  

	This implementation uses only linear tapers.  These define the width 
	of the tapers on the low and high ends of grid coordinates.  */
	double widthlow[3],widthhigh[3];
	/*! These control if a taper is applied along a particular edge.  

	When true a taper will be applied. 
	*/
	bool applylow[3],applyhigh[3];
};


void constructor_log_error(const string base, int comp, const string detail)
{
	cerr << base <<endl
	<< "Component number "<<comp
	<< detail<<endl;
}
TaperGrid::TaperGrid(Metadata& md)
{
	xlvalues[0]=md.get_double("x1low_edge_value");
	xlvalues[1]=md.get_double("x2low_edge_value");
	xlvalues[2]=md.get_double("x3low_edge_value");
	xhvalues[0]=md.get_double("x1high_edge_value");
	xhvalues[1]=md.get_double("x2high_edge_value");
	xhvalues[2]=md.get_double("x3high_edge_value");
	widthlow[0]=md.get_double("x1low_taper_width");
	widthlow[1]=md.get_double("x2low_taper_width");
	widthlow[2]=md.get_double("x3low_taper_width");
	widthhigh[0]=md.get_double("x1high_taper_width");
	widthhigh[1]=md.get_double("x2high_taper_width");
	widthhigh[2]=md.get_double("x3high_taper_width");
	applylow[0]=md.get_bool("x1low_side_apply_taper");
	applylow[1]=md.get_bool("x2low_side_apply_taper");
	applylow[2]=md.get_bool("x3low_side_apply_taper");
	applyhigh[0]=md.get_bool("x1high_side_apply_taper");
	applyhigh[1]=md.get_bool("x2high_side_apply_taper");
	applyhigh[2]=md.get_bool("x3high_side_apply_taper");
	int i;
	const string baseerror("TaperGrid constructor(Warning):");
	const string negdetail(" negative end value for taper set to 0.0");
	const string posdetail(" large end value for taper set to 0.0");
	for(i=0;i<3;++i)
	{
		if(applylow[i])
		{
			if(xlvalues[i]<0.0)
			{
				constructor_log_error(baseerror, i, negdetail);
				xlvalues[i]=0.0;
			}
			else if(xlvalues[i]>1.0)
			{
				constructor_log_error(baseerror, i, posdetail);
				xlvalues[i]=0.0;
			}
		}
		if(applyhigh[i])
		{
			if(xhvalues[i]<0.0)
			{
				constructor_log_error(baseerror, i, negdetail);
				xhvalues[i]=0.0;
			}
			else if(xhvalues[i]>1.0)
			{
				constructor_log_error(baseerror, i, posdetail);
				xhvalues[i]=0.0;
			}
		}
	}

}
TaperGrid::TaperGrid(string fname)
{
    try {
        PfStyleMetadata mdtmp=pfread(fname);
	*this=TaperGrid(mdtmp);
    }catch(...){throw;};
}
TaperGrid::TaperGrid(const TaperGrid& parent)
{
	int i;
	for(i=0;i<3;++i)
	{
		xlvalues[i]=parent.xlvalues[i];
		xhvalues[i]=parent.xhvalues[i];
		widthlow[i]=parent.widthlow[i];
		widthhigh[i]=parent.widthhigh[i];
		applylow[i]=parent.applylow[i];
		applyhigh[i]=parent.applyhigh[i];
	}
}
TaperGrid& TaperGrid::operator=(const TaperGrid& parent)
{
    if(this != &parent)
    {
	int i;
	for(i=0;i<3;++i)
	{
		xlvalues[i]=parent.xlvalues[i];
		xhvalues[i]=parent.xhvalues[i];
		widthlow[i]=parent.widthlow[i];
		widthhigh[i]=parent.widthhigh[i];
		applylow[i]=parent.applylow[i];
		applyhigh[i]=parent.applyhigh[i];
	}
    }
    return(*this);
}
string build_apply_error_message(const string base, int n, double width)
{
	stringstream ss;
	ss << base
		<< "Requested taper width of "<<(int)width<<" grid cells"<<endl
		<< "Grid to which this was to be applied is only "<<n<<" cells across"<<endl;
	return(ss.str());
}
void TaperGrid::apply(GCLscalarfield3d& g)
{
	int i,j,k,ii;
	double slope;
	double multiplier;
	int endtaper;
	// probably should have a sanity check on the width 
	// parameters.  We'll just throw an exception if
	// we get a crazy request.
	const string baseerror("TaperGrid::apply(GCLscalarfield3d):  taper width inconsistent with this grid\n");
	if(applylow[0])
	{
		if( (widthlow[0]>(g.n1)/2) || (widthlow[0]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n1,widthlow[0]));
		slope=(1.0-xlvalues[0])/widthlow[0];
		endtaper=static_cast<int>(widthlow[0]);
		for(i=0;i<endtaper;++i)
		{
			multiplier=xlvalues[0]+slope*static_cast<double>(i);
			for(j=0;j<g.n2;++j)
				for(k=0;k<g.n3;++k)
					g.val[i][j][k]*=multiplier;
		}
	}
	if(applylow[1])
	{
		if( (widthlow[1]>(g.n2)/2) || (widthlow[1]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n2,widthlow[1]));
		slope=(1.0-xlvalues[1])/widthlow[1];
		endtaper=static_cast<int>(widthlow[1]);
		for(j=0;j<endtaper;++j)
		{
			multiplier=xlvalues[1]+slope*static_cast<double>(j);
			for(i=0;i<g.n1;++i)
				for(k=0;k<g.n3;++k)
					g.val[i][j][k]*=multiplier;
		}
	}
	if(applylow[2])
	{
		if( (widthlow[2]>(g.n2)/2) || (widthlow[2]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n3,widthlow[2]));
		slope=(1.0-xlvalues[2])/widthlow[2];
		endtaper=static_cast<int>(widthlow[2]);
		for(k=0;k<endtaper;++k)
		{
			multiplier=xlvalues[2]+slope*static_cast<double>(k);
			for(i=0;i<g.n1;++i)
				for(j=0;j<g.n2;++j)
					g.val[i][j][k]*=multiplier;
		}
	}
	if(applyhigh[0])
	{
		if( (widthhigh[0]>(g.n1)/2) || (widthhigh[0]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n1,widthhigh[0]));
		slope=(1.0-xhvalues[0])/widthhigh[0];
		endtaper=static_cast<int>(widthhigh[0]);
		for(ii=0,i=(g.n1)-1;ii<endtaper;--i,++ii)
		{
			multiplier=xhvalues[0]+slope*static_cast<double>(ii);
			for(j=0;j<g.n2;++j)
				for(k=0;k<g.n3;++k)
					g.val[i][j][k]*=multiplier;
		}
	}
	if(applyhigh[1])
	{
		if( (widthhigh[1]>(g.n2)/2) || (widthhigh[1]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n2,widthhigh[1]));
		slope=(1.0-xhvalues[1])/widthhigh[1];
		endtaper=static_cast<int>(widthhigh[1]);
		for(ii=0,j=(g.n2)-1;ii<endtaper;--j,++ii)
		{
			multiplier=xhvalues[1]+slope*static_cast<double>(ii);
			for(i=0;i<g.n1;++i)
				for(k=0;k<g.n3;++k)
					g.val[i][j][k]*=multiplier;
		}
	}
	if(applyhigh[2])
	{
		if( (widthhigh[2]>(g.n2)/2) || (widthhigh[2]<0.0))
			throw SeisppError(build_apply_error_message(baseerror,g.n3,widthhigh[2]));
		slope=(1.0-xhvalues[2])/widthhigh[2];
		endtaper=static_cast<int>(widthhigh[2]);
		for(ii=0,k=(g.n2)-1;ii<endtaper;--k,++ii)
		{
			multiplier=xhvalues[2]+slope*static_cast<double>(ii);
			for(i=0;i<g.n1;++i)
				for(j=0;j<g.n2;++j)
					g.val[i][j][k]*=multiplier;
		}
	}
}
	
		
