#include <stdio.h>
#include "stock.h"
#include "seispp.h"
#include "dbpp.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
void usage()
{
	cerr << "exportgrid db gridname [-f fieldname -3d -vector] "<<endl
		<< " outputs GCLgrid or field objects to stdout"<<endl
		<< " Dumps only grid unless -f is used."<<endl;
	exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	elog_init(argc,argv);
	if(argc < 3) usage();
	string dbname(argv[1]);
	string gridname(argv[2]);
	string fieldname("");  //initialization for safety
	bool threedmode(false);
	bool isfield(false);
	bool vectorfield(false);
	for(int i=3;i<argc;i++)
	{
		string sarg(argv[i]);
		if(sarg=="-3d") 
			threedmode=true;
		else if(sarg=="-f")
		{
			++i;
			if(i==argc) usage();
			fieldname=string(argv[i]);
			isfield=true;
		}
		else if(sarg=="-vector")
			vectorfield=true;
		else
			usage();
	}
	try {
		DatascopeHandle dbh(dbname,true);
		if(threedmode)
		{
		    if(isfield)
		    {
			if(vectorfield)
			{
				GCLvectorfield3d field(dbh.db,gridname,fieldname);
				cout << field;
			}
			else
			{
				GCLscalarfield3d field(dbh.db,gridname,fieldname);
				cout << field;
			}
		     }
		     else
		     {
			GCLgrid3d grid(dbh.db,gridname);
			/* fields define operator <<, but there is this is not
			currently defined for grids.  We write a grid in a simpler
			format*/
			for(int k=0;k<grid.n3;++k)
			  for(int j=0;j<grid.n2;++j)
			    for(int i=0;i<grid.n1;++i)
				cout << deg(grid.lon(i,j,k)) <<" "
					<<deg(grid.lat(i,j,k))<<" "
					<<grid.depth(i,j,k)<<endl;
			
		     }
		}
		else
		{
		    if(isfield)
		    {
			if(vectorfield)
			{
				GCLvectorfield field(dbh.db,gridname,fieldname);
				cout << field;
			}
			else
			{
				GCLscalarfield field(dbh.db,gridname,fieldname);
				cout << field;
			}
		     }
		     else
		     {
			GCLgrid grid(dbh.db,gridname);
			for(int j=0;j<grid.n2;++j)
			    for(int i=0;i<grid.n1;++i)
				cout << deg(grid.lon(i,j)) 
                                    <<" "<<deg(grid.lat(i,j))
                                    <<" "<<grid.depth(i,j)<<endl;
		     }
		}
	} catch (int ierr)
	{
		cerr << "The GCL object constructor called threw exception ="<<ierr<<endl;
		cerr << "Antelope elog message:"<<endl;
		elog_die(0,"exportgrid:  failed");
	}
	catch (SeisppError serr)
	{
		serr.log_error();
		usage();
	}
}
