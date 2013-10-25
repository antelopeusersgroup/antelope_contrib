#include <iostream>
#include <sstream>
#include <string>
#include "gclgrid.h"
#include "dbpp.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;

void usage()
{
	cerr << "gclfmath db gridname infield outfield [-vector -dir outdir] < commands"<<endl;
	exit(-1);
}
class RangePair
{
public:
	int low,high;
	RangePair(int l,int h){low=l;high=h;};
};
enum FDEOperator {ADD,MULTIPLY,ASSIGN};
class FieldEditData
{
public:
	int ilow,ihigh;
	int jlow,jhigh;
	int klow,khigh;
	int llow,lhigh;
	FieldEditData(BasicGCLgrid *g,char *line);
	double apply(double val);
private:
	double opvalue;
	FDEOperator opcode;
};

RangePair parse_token(string s, int length)
{
	if(s=="*")
	{
		/* For all set full range */
		return(RangePair(0,length-1));
	}
	string slow,shigh;
	RangePair result(0,0);
	int dashposition;
	dashposition=s.find_first_of("-",0);
	if(dashposition==0)
	{
		/* Land here for things like "-22" */
		result.low=0;
		shigh.assign(s,1,s.length()-1);
		result.high=atoi(shigh.c_str());
	}
	else if(dashposition<0) 
	{
		/* Land here for things with no dash */
		result.low=atoi(s.c_str());
		result.high=result.low;
	}
	else if(dashposition==(s.length()-1))
	{
		/* land here for things like "2-" */
		result.high=length-1;
		slow.assign(s,0,dashposition);
		result.low=atoi(s.c_str());
	}
	else
	{
		/* For things like 2-44 */
		slow.assign(s,0,dashposition);
		// Use length() because assign stops copy at end of string or count
		shigh.assign(s,dashposition+1,s.length()); 
		result.low=atoi(slow.c_str());
		result.high=atoi(shigh.c_str());
	}
	return(result);
}

FieldEditData::FieldEditData(BasicGCLgrid *g,char *s)
{
	string base_error("FieldEditData constructor:  ");
	GCLscalarfield3d *gsf;
	GCLvectorfield3d *gvf;
	bool vectormode(false);
	gsf=dynamic_cast<GCLscalarfield3d *>(g);
	if(gsf==NULL)
	{
		gvf=dynamic_cast<GCLvectorfield3d *>(g);
		if(gvf==NULL)
			throw SeisppError(string("FieldEditData constructor:  ")
				+ "dynamic cast failed");
		vectormode=true;
	}
	RangePair rp(0,0);
	istringstream in(s);

	string token;
	in >> token;
	int length;
	if(vectormode)
		length=gvf->n1;
	else
		length=gsf->n1;
	rp=parse_token(token,length);
	ilow=rp.low;
	ihigh=rp.high;

	in >> token;
	if(vectormode)
		length=gvf->n2;
	else
		length=gsf->n2;
	rp=parse_token(token,length);
	jlow=rp.low;
	jhigh=rp.high;
	
	in >> token;
	if(vectormode)
		length=gvf->n3;
	else
		length=gsf->n3;
	rp=parse_token(token,length);
	klow=rp.low;
	khigh=rp.high;
		
	in >> token;
	if(vectormode)
	{
		length=gvf->nv;
		rp=parse_token(token,length);
		llow=rp.low;
		lhigh=rp.high;
	}
	else
	{
		llow=0;
		lhigh=0;
	}
	string opstring;
	in >> opstring;
	if(opstring=="*")
		opcode=MULTIPLY;
	else if(opstring=="+")
		opcode=ADD;
	else if(opstring=="=")
		opcode=ASSIGN;
	else
		throw SeisppError(base_error
		 + "Illegal operation code = "
		 + opstring);
	in >> opvalue;
}

double FieldEditData::apply(double val)
{
	double result;
	switch(opcode)
	{
	case ADD:
		result=val+opvalue;
		break;
	case MULTIPLY:
		result=val*opvalue;
		break;
	case ASSIGN:
		result=opvalue;
		break;
	default:
		cerr << "FieldEditData::apply:  illegal opcode="
			<< opcode<<endl;
		cerr << "This should not happen so program aborts"
			<<endl;
		exit(-1);
	}
	return(result);
}


/* Design:  will allow these syntax variations for commands
  1-20 example of range 1 to 20
  * use all
  55 single number 
Each line of form:
i j k l operator value
operator can be * (multiply) or + add.
*/
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	int i,j,k,l;
	if(argc<5) usage();
	string dbname(argv[1]);
	string gridname(argv[2]);
	string infield(argv[3]);
	string outfield(argv[4]);
	bool vectormode(false);
	string outdir("editedfields");
	try {
		for(i=5;i<argc;++i)
		{
			string sarg(argv[i]);
			if(sarg=="-vector")
				vectormode=true;
			else if(sarg=="-dir")
			{
				++i;
				outdir=string(argv[i]);
			}
			else
				usage();
		}
		DatascopeHandle dbh(dbname,false);
		dbh.lookup("gclgdisk");
		GCLscalarfield3d *gsf;
		GCLvectorfield3d *gvf;
		int n1,n2,n3;
		gsf=NULL;
		gvf=NULL;
		if(vectormode)
		{
			gvf=new GCLvectorfield3d(dbh,gridname,infield);
			n1=gvf->n1;
			n2=gvf->n2;
			n3=gvf->n3;
		}
		else
		{
			gsf=new GCLscalarfield3d(dbh,gridname,infield);
			n1=gsf->n1;
			n2=gsf->n2;
			n3=gsf->n3;
		}
		/* Now loop over instruction lines running potentially multiple
		operations on the same data */
		FieldEditData *fed;
		char line[256];
		while(cin.getline(line,256))
		{
			if(vectormode) 
				fed=new FieldEditData(gvf,line);
			else
				fed=new FieldEditData(gsf,line);
			for(i=0;i<n1;++i)
			 for(j=0;j<n2;++j)
			  for(k=0;k<n3;++k)
			    if( (i>=fed->ilow) && (i<=fed->ihigh) 
				 && (j>=fed->jlow) && (j<=fed->jhigh)
				&& (k>=fed->klow) && (k<=fed->khigh) )
			    {
			      if(vectormode)
			      {
			        for(l=fed->llow;l<fed->lhigh;++l)
				    gvf->val[i][j][k][l]=fed->apply(gvf->val[i][j][k][l]);
			      }
			      else
			      {
			        gsf->val[i][j][k]=fed->apply(gsf->val[i][j][k]);
			      }
			    }
		}
		if(vectormode)
			gvf->save(dbh,string(""),outdir,outfield,outfield);
		else
			gsf->save(dbh,string(""),outdir,outfield,outfield);
			
	} catch (int ierr)
	{
		cerr << "GCLgrid error:  something threw error code = "<<ierr<<endl;
		exit(-1);
	}
}
