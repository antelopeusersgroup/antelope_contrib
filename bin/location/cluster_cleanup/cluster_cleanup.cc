#include <string>
#include <vector>
#include <algorithm>
#include <functional>
#include "dbpp.h"
#include "Metadata.h"
#include "Hypocenter.h"
#include "gclgrid.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;

void usage()
{
	cbanner("$Revision$ $Date$",
		"dbin dbout [-v -pf pfname]",
		"Gary Pavlis",
		"Indiana University",
		"pavlis@indiana.edu") ;
	elog_die(0,"Exit on usage error\n");
}
// Hypocentroid object which is the centroid of an ensemble of 
// hypocenters.   We inherit Hypocenter, but ignore the time
// field since time is not a useful concept for a hypocentroid.
// Note operator < is specialized for this program.  Do not 
// steal this class definition elsewhere without considering this.
// We use operator < here to sort cluster groups to find member
// with maximum number of associations.
class Hypocentroid : public Hypocenter
{
public:
	int gridid;
	int nassoc;
	double zmin;
	double zmax;
	double gridlat;
	double gridlon;
	double gridz;
	Hypocentroid (Dbptr db);
	Hypocentroid (double latin, double lonin, double zin,
		int gin, int nassin, double zminin, double zmaxin,
		double glat, double glon, double gz);
	Hypocentroid (const Hypocentroid&);
	Hypocentroid(DatascopeHandle& db);
};
bool operator < (const Hypocentroid& h1, const Hypocentroid h2)
{
	return(h1.nassoc<h2.nassoc);
}
	
	
Hypocentroid::Hypocentroid (double latin, double lonin, double zin,
		int gin, int nassin, double zminin, double zmaxin,
		double glat, double glon, double gz)
	: Hypocenter(latin, lonin, zin, 0.0, string("ttaup"),string("iasp91"))
{
	gridid=gin; nassoc=nassin;
	zmin=zminin; zmax=zmaxin;
	gridlat=lat;  gridlon=glon;  gridz=gz;
}
Hypocentroid::Hypocentroid(const Hypocentroid& hin)
	: Hypocenter(hin)
{
	gridid=hin.gridid;
	nassoc=hin.nassoc;
	zmin=hin.zmin;
	zmax=hin.zmax;
	gridlat=hin.gridlat;
	gridlon=hin.gridlon;
	gridz=hin.gridz;
}
//
// Assumes dbh is a valid handle pointing to a row of the hypocentroid
// table.  Reads frozen set of attributes.
//
Hypocentroid::Hypocentroid(DatascopeHandle& dbh)
{
	lat=dbh.get_double("hclat");
	lat=rad(lat);
	lon=dbh.get_double("hclon");
	lon=rad(lon);
	z=dbh.get_double("hcdepth");
	time=0.0;
	gridid=dbh.get_int("gridid");
	nassoc=dbh.get_int("nass");
	zmin=dbh.get_double("ztop");
	zmax=dbh.get_double("zbot");
	gridlat=dbh.get_double("dlat");
	gridlon=dbh.get_double("dlon");
	gridlat=rad(gridlat);
	gridlon=rad(gridlon);
	gridz=dbh.get_double("depth");
}

double get_cell_radius(GCLgrid3d& grid,int i, int j, int k)
{
	double dx1,dx2,dx3;
	double cellx1,cellx2;
	double result;
	if(i==0)
	{
		dx1=grid.x1[i+1][j][k]-grid.x1[i][j][k];
		dx2=grid.x2[i+1][j][k]-grid.x2[i][j][k];
		dx3=grid.x3[i+1][j][k]-grid.x3[i][j][k];
		cellx1=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
	}
	else if(i==(grid.n1-1))
	{
		dx1=grid.x1[i][j][k]-grid.x1[i-1][j][k];
		dx2=grid.x2[i][j][k]-grid.x2[i-1][j][k];
		dx3=grid.x3[i][j][k]-grid.x3[i-1][j][k];
		cellx1=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
	}
	else
	{
		dx1=grid.x1[i+1][j][k]-grid.x1[i-1][j][k];
		dx2=grid.x2[i+1][j][k]-grid.x2[i-1][j][k];
		dx3=grid.x3[i+1][j][k]-grid.x3[i-1][j][k];
		cellx1=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
		cellx1 /= 2.0;
	}
	if(j==0)
	{
		dx1=grid.x1[i][j+1][k]-grid.x1[i][j][k];
		dx2=grid.x2[i][j+1][k]-grid.x2[i][j][k];
		dx3=grid.x3[i][j+1][k]-grid.x3[i][j][k];
		cellx2=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
	}
	else if(j==(grid.n2-1))
	{
		dx1=grid.x1[i][j][k]-grid.x1[i][j-1][k];
		dx2=grid.x2[i][j][k]-grid.x2[i][j-1][k];
		dx3=grid.x3[i][j][k]-grid.x3[i][j-1][k];
		cellx2=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
	}
	else
	{
		dx1=grid.x1[i][j+1][k]-grid.x1[i][j-1][k];
		dx2=grid.x2[i][j+1][k]-grid.x2[i][j-1][k];
		dx3=grid.x3[i][j+1][k]-grid.x3[i][j-1][k];
		cellx2=sqrt(dx1*dx1+dx2*dx2+dx3*dx3);
		cellx1 /= 2.0;
	}
	result=max(cellx1,cellx2);
	result=max(result,sqrt(cellx1*cellx1+cellx2*cellx2));
	result /= 2.0;  // need a radius not the cell size
	return(result);
}
double get_cell_dz(GCLgrid3d& g,int i, int j, int k)
{
	double top, bot, result;
	if(k==0)
	{
		bot=g.depth(i,j,k);
		top=g.depth(i,j,k+1);
		result=bot-top;
	}
	else if(k==(g.n3-1))
	{
		bot=g.depth(i,j,k-1);
		top=g.depth(i,j,k);
		result=bot-top;
	}
	else
	{
		bot=g.depth(i,j,k-1);
		top=g.depth(i,j,k+1);
		result=(bot-top)/2.0;
	}
	return(result);
}
void dbsave(DatascopeHandle dbh, Hypocentroid h, string gridname)
{
	int ierr;
	ierr=dbaddv(dbh.db,0,
		"gridname",gridname.c_str(),
		"gridid",h.gridid,
		"dlat",deg(h.gridlat),
		"dlon",deg(h.gridlon),
		"depth",h.gridz,
		"nass",h.nassoc,
		"ztop",h.zmin,
		"zbot",h.zmax,
		"hclat",deg(h.lat),
		"hclon",deg(h.lon),
		"hcdepth",h.z,0);
	if(ierr<0) cerr<< "dbaddv err9r saving result for gridid "<<h.gridid<<endl;
}
int index_to_gridid(GCLgrid3d& g, int i, int j, int k)
{
	return( 1+k+j*(g.n3)+i*(g.n2)*(g.n3) );
}

	
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
	ios::sync_with_stdio();
	int i,j,k;
	bool Verbose=false;

	elog_init(argc,argv);
	elog_notify (0, "$Revision$ $Date$") ;
	if(argc<3) usage();

	string dbin(argv[1]);
	string dbout(argv[2]);
	string pfin("cluster_cleanup");

	for(i=3;i<argc;++i)
	{
                if(!strcmp(argv[i],"-pf"))
                {
                        ++i;
                        if(i>=argc) usage();
                        pfin = string(argv[i]);
                }
		else if(!strcmp(argv[i],"-v"))
		{
			Verbose=true;
			SEISPP_verbose=true;
		}
		else
			usage();
	}
	Pf *pf;
	if(pfread(const_cast<char *>(pfin.c_str()),&pf)) 
		elog_die(0,"pfread error for pffile=%s\n",pfin.c_str());
	try {
		Metadata params(pf);
		string gridname=params.get_string("gridname");
		DatascopeHandle dbh(dbin,true);
		DatascopeHandle dbhout(dbout,false);
		dbh.lookup("hypocentroid");
		dbhout.lookup("hypocentroid");
		string sstring(string("gridname=~/")+gridname+string("/"));
		dbh.subset(sstring);
		dbh.rewind();
		vector<Hypocentroid> hypocen;
		for(i=0;i<dbh.number_tuples();++dbh,++i)
		{
			Hypocentroid h(dbh);
			if(h.nassoc>0) hypocen.push_back(h);
		}
		cout << "Number of hypocentroids to process="
			<< hypocen.size()<<endl;

		GCLgrid3d grid(dbh.db,gridname);
		double cell_radius,cell_dz,zmin,zmax;
		list<Hypocentroid> hcell;
		vector<Hypocentroid>::iterator hptr;
		double hcdistkm;
		int gridid;
		for(i=0;i<grid.n1;++i)
		     for(j=0;j<grid.n2;++j)
			for(k=0;k<grid.n3;++k)
			{
				double lat=grid.lat(i,j,k);
				double lon=grid.lon(i,j,k);
				double depth=grid.depth(i,j,k);
				cell_radius=get_cell_radius(grid,i,j,k);
				cell_dz=get_cell_dz(grid,i,j,k);
				zmin=depth-cell_dz/2.0;
				zmax=depth+cell_dz/2.0;
				// Push hypocentroids inside this cell
				// to list hcell
				for(hptr=hypocen.begin();hptr!=hypocen.end();
					++hptr)
				{
					hcdistkm=deg(hptr->distance(lat,lon));
					hcdistkm=deg2km(hcdistkm);
					if( ((hptr->z)<zmin)
					  || ((hptr->z)>zmax) ) continue;
					if(hcdistkm>cell_radius) continue;
					hcell.push_back(*hptr);
				}	
				// Now find the member of hcell with 
				// the largest number of associations
				
				list<Hypocentroid>::iterator hmax;
				if(hcell.size()>0)
				{
					if(Verbose)
					{
						cout << "Grid i,j,k="<<i<<","<<j<<","<<k<<endl;
						cout << "This cell has "<<hcell.size()<<" hits"<<endl;
					}
					if(hcell.size()==1)
						hmax=hcell.begin();
					else
						hmax=max_element(hcell.begin(),
                                        	   hcell.end(),
						   less<Hypocentroid>());
					// Need to reset the gridid 
					gridid=index_to_gridid(grid,i,j,k);
					if(Verbose)cout << hmax->gridid <<"->"<<gridid;
					//hmax->gridid=gridid;
					dbsave(dbhout,*hmax,gridname);
				}
				hcell.clear();
			}
	}
	catch (MetadataGetError mdge)
	{
		mdge.log_error();
		exit(-1);
	}
	catch (SeisppDberror sdbe)
	{
		sdbe.log_error();
		exit(-2);
	}
	catch (...)
	{
		elog_die(0,"Unhandled exception");
	}
	exit(0);
}
