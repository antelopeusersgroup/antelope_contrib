#include <stdio.h>
#include "stock.h"
#include "seispp.h"
#include "dbpp.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
void usage()
{
	cerr << "exportgrid db|fname gridname [-fin -f fieldname -3d -vector n -o outfile -dir outdir] "<<endl
		<< " outputs GCLgrid or field objects to stdout"<<endl
                << " -fin selects file based input with name fname (arg1) - default is db input"<<endl
		<< " Dumps only grid unless -f is used."<<endl
                << " Use -3d if data are defined on a 3d grid (default is 2d)"<<endl
                << " Use -vector if the data are a vector field (default is scalar). n is vector size"<<endl
                << " -o saves to outfile using pfhdr format (used mainly for conversion from db)"<<endl
                << " -dir defines directory where file based saves will be done (default is curent directory)"<<endl
                << "  ( -o not allowed with -fin option)"<<endl;
	exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	elog_init(argc,argv);
        /* This is defined in gclgrid.h and perhaps should use the symbol default_output_format
           used there, but we will code it here.  Potential maintenance issue. */
        const string formstr("pfhdr");
	if(argc < 3) usage();
	string dbname(argv[1]);
        string fname;
	string gridname(argv[2]);
	string fieldname("");  //initialization for safety
        bool dbmode(true);
	bool threedmode(false);
	bool isfield(false);
	bool vectorfield(false);
        bool write_file_output(false);
        int nv;  // expected number of vector components in read
        string outfile;
        string outdir(".");
	for(int i=3;i<argc;i++)
	{
		string sarg(argv[i]);
		if(sarg=="-3d") 
			threedmode=true;
                else if(sarg=="-fin")
                {
                    dbmode=false;
                    fname=dbname;
                }
		else if(sarg=="-f")
		{
			++i;
			if(i==argc) usage();
			fieldname=string(argv[i]);
			isfield=true;
		}
		else if(sarg=="-vector")
                {
			vectorfield=true;
			++i;
			if(i==argc) usage();
                        nv=atoi(argv[i]);
                }
                else if(sarg=="-dir")
                {
			++i;
			if(i==argc) usage();
                        outdir=string(argv[i]);
                }
                else if(sarg=="-o")
                {
			++i;
			if(i==argc) usage();
                        outfile=string(argv[i]);
                        write_file_output=true;
                }
		else
			usage();
	}
        if(write_file_output && (!dbmode)) usage();
	try {
		DatascopeHandle dbh;
                if(dbmode) dbh=DatascopeHandle(dbname,true);
		if(threedmode)
		{
		    if(isfield)
		    {
			if(vectorfield)
			{
                            if(dbmode)
                            {
				GCLvectorfield3d field(dbh,gridname,fieldname,nv);
				cout << field;
                                if(write_file_output) field.save(outfile,outdir);
                            }
                            else
                            {
                                GCLvectorfield3d field(fname,formstr);
                                cout << field;
                            }
			}
			else
			{
                            if(dbmode)
                            {
				GCLscalarfield3d field(dbh,gridname,fieldname);
				cout << field;
                                if(write_file_output) field.save(outfile,outdir);
                            }
                            else
                            {
                                GCLscalarfield3d field(fname,formstr);
                                cout << field;
                            }
			}
		     }
		     else
		     {
			GCLgrid3d grid;
                        if(dbmode)
                            grid=GCLgrid3d(dbh,gridname);
                        else
                            grid=GCLgrid3d(fname,formstr);
			/* fields define operator <<, but there is this is not
			currently defined for grids.  We write a grid in a simpler
			format*/
			for(int k=0;k<grid.n3;++k)
			  for(int j=0;j<grid.n2;++j)
			    for(int i=0;i<grid.n1;++i)
				cout << deg(grid.lon(i,j,k)) <<" "
					<<deg(grid.lat(i,j,k))<<" "
					<<grid.depth(i,j,k)<<endl;
                         if(write_file_output) grid.save(outfile,outdir);
			
		     }
		}
		else
		{
		    if(isfield)
		    {
			if(vectorfield)
			{
				GCLvectorfield field(dbh,gridname,fieldname,nv);
				cout << field;
                                if(write_file_output) field.save(outfile,outdir);
			}
			else
			{
				GCLscalarfield field(dbh,gridname,fieldname);
				cout << field;
                                if(write_file_output) field.save(outfile,outdir);
			}
		     }
		     else
		     {
			GCLgrid grid;
                        if(dbmode)
                            grid=GCLgrid(dbh,gridname);
                        else
                            grid=GCLgrid(fname,formstr);
			for(int j=0;j<grid.n2;++j)
			    for(int i=0;i<grid.n1;++i)
				cout << deg(grid.lon(i,j)) <<" "<<deg(grid.lat(i,j))<<endl;
                        if(write_file_output) grid.save(outfile,outdir);
		     }
		}
	} catch (int ierr)
	{
		cerr << "The GCL object constructor called threw exception ="<<ierr<<endl;
		cerr << "Antelope elog message:"<<endl;
		elog_die(0,"exportgrid:  failed");
	}
	catch (SeisppError& serr)
	{
		serr.log_error();
		usage();
	}
        catch (std::exception& stexp)
        {
            cerr << stexp.what()<<endl;
        }
}
