#include <string>
#include <vector>
#include <list>
#include <map>
#include <set>
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
/* local typedefs for this program */

/* this is used to hold a list of evids that are associated to one gridid */
typedef struct {
	int gridid;
	int nass;
	set<int> evids;
} EvidData;
/* An EvidMap is used to have a keyed list of evids that are tested to remove redundant
clusters */
typedef map<int,EvidData> EvidMap;  // convenient typedef to avoid confusing declarations below
/* This map is build and edited to remove redundant hypocentroids.  Survivors are written to output */
typedef map<int,Hypocentroid> HypoMap;
/* used to control redundancy check method.  */
typedef enum {EXACTMATCH, SUBSETMATCH, DISTANCEONLY} CheckMethod;


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
/* Simple parser to convert a word into the CheckMethod enum */
CheckMethod strtocm(string s)
{
	CheckMethod result;
	if(s=="exact")
		result=EXACTMATCH;
	else if(s=="maximum_associations")
		result=SUBSETMATCH;
	else if(s=="distance_only")
		result=DISTANCEONLY;
	else
		throw SeisppError("strtcocm: parameter redundant_check_method value="+s
		   + string(" is invalid\nMust be:  exact, maximum_associations, or distance_only"));
	return(result);
}

bool is_redundant_test(EvidData& ed1, EvidData& ed2, CheckMethod rcm)
{
	bool result;
	set<int>::iterator evs1e,evs2e,evsptr,evs2ptr;
	evs1e=ed1.evids.end();
	evs2e=ed2.evids.end();
	switch (rcm)
	{
	case EXACTMATCH:
		if(ed1.evids.size()==ed2.evids.size())
			result=false;
		else
		{
			result=true;
			for(evsptr=ed1.evids.begin();evsptr!=evs1e;++evsptr)
			{
				evs2ptr=ed2.evids.find(*evsptr);
				if(evs2ptr==evs2e)
				{
					result=false;
					break;
				}
			}
		}
		break;
	case SUBSETMATCH:
		if(ed1.evids.size()<ed2.evids.size())
			throw SeisppError("is_redundant_test:  second group is larger than first.  Probably coding or db error");
		else
		{
			/*Since we can be sure ed2 is smaller than ed1 we just need to match 2 to 1 and stop if there
			are any mismatches.  Note this is the reverse order of EXACTMATCH */
			result=true;
			for(evs2ptr=ed2.evids.begin();evs2ptr!=evs2e;++evs2ptr)
			{
				evsptr=ed1.evids.find(*evs2ptr);
				if(evsptr==evs1e)
				{
					result=false;
					break;
				}
			}
		}
		break;
	case DISTANCEONLY:
	default:
		result= false;
	}
	return(result);
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
		string redundant_check_method=params.get_string("redundant_check_method");
		CheckMethod rcm=strtocm(redundant_check_method);
		DatascopeHandle dbh(dbin,true);
		DatascopeHandle dbhout(dbout,false);
		dbh.lookup("hypocentroid");
		dbhout.lookup("hypocentroid");
		string sstring(string("gridname=~/")+gridname+string("/"));
		dbh.subset(sstring);
		dbh.rewind();
		vector<Hypocentroid> *hypocen=new vector<Hypocentroid>;
		for(i=0;i<dbh.number_tuples();++dbh,++i)
		{
			Hypocentroid h(dbh);
			if(h.nassoc>0) hypocen->push_back(h);
		}
		cout << "Number of hypocentroids to process="
			<< hypocen->size()<<endl;

		GCLgrid3d grid(dbh.db,gridname);
		double cell_radius,cell_dz,zmin,zmax;
		list<Hypocentroid> hcell;
		vector<Hypocentroid>::iterator hptr;
		HypoMap pass1survivors;
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
				for(hptr=hypocen->begin();hptr!=hypocen->end();
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
					pass1survivors.insert(pair<int,Hypocentroid>(hmax->gridid,*hmax));
				}
				hcell.clear();
			}
		cout << "First pass distance check reduced grid count from "
			<< dbh.number_tuples()  << " to " << pass1survivors.size()<<endl;
		if(rcm!=DISTANCEONLY)
		{
		cout << "Running secondary cleanup with method="<<redundant_check_method<<endl;
		/* Pass 2 searchs for totally redundant cluster groups.  Algorithm is
		to work through in order of number of associations.  Delete any hypocentroid 
		for which the evid members are entirely contained in another cell with the
		the same or more associations.  First step is to load information from a join
		of hypocentroid and cluster.  We use this double stl container to manage this.
		We use the database to do the sort by nass*/
		list<string> sortkeys,grpkeys;
		sortkeys.push_back("nass");
		dbh.sort(sortkeys);
		dbh.natural_join("cluster");
		/* there is a one to many relation for hypocentroid->cluster so we don't need another sort here */
		grpkeys.push_back("gridid");
		dbh.group(grpkeys);
		EvidMap emap;
		/* load emap by looping through this dbh view */
		dbh.rewind();
		EvidData thisgroup;
		/* this will contain a list of all gridids loaded in nass order.  This will be used as the pattern
		to work through the data.  We'll selectively destroy the map contents. */
		list<int> gridids;
		for(i=0;i<dbh.number_tuples();++dbh,++i)
		{
			DBBundle dbb=dbh.get_range();
			Dbptr parent=dbb.parent;
			for(parent.record=dbb.start_record;parent.record<dbb.end_record;++parent.record)
			{
				/* a bit inefficient to constantly reset gridid and nass in thisgroup, but minor
				compared to overhead of making the dbgetv call */
				int thisevid;
				dbgetv(parent,0,"gridid",&(thisgroup.gridid),
					"evid",&thisevid,
					"nass",&(thisgroup.nass),0);
				thisgroup.evids.insert(thisevid);
			}
			emap.insert(pair<int,EvidData>(thisgroup.gridid,thisgroup));
			/* use push front to reverse the list, avoids a confusing reverse iterator */
			gridids.push_front(thisgroup.gridid);
			thisgroup.evids.clear();
		}
		/* Now the really messy algorithm.  Work through gridid as the master list.  
		If a gridid isn't found in both lists immediately jump to the next gridid.  
		Otherwise enter the search or redundant lists block. */
		list<int>::iterator grdptr,grdpass2;
		/* use these a lot so we set them here once */
		EvidMap::iterator emend=emap.end();
		HypoMap::iterator hmend=pass1survivors.end();
		EvidMap::iterator ed,ed2;
		HypoMap::iterator hc,hc2;
		
		for(grdptr=gridids.begin();grdptr!=gridids.end();++grdptr)
		{
			gridid=*grdptr;  // recycle this symbol declared far up there
			ed=emap.find(gridid);
			hc=pass1survivors.find(gridid);
			if(ed!=emend && hc!=hmend)
			{
				grdpass2=grdptr;
				++grdpass2;  // set to next.  Produces odd construct in this for loop
				for(;grdpass2!=gridids.end();++grdpass2)
				{
					int gridid2=*grdpass2;
					ed2=emap.find(gridid2);
					hc2=pass1survivors.find(gridid2);
					if(ed2!=emend && hc2!=hmend)
					{
						if(is_redundant_test(ed->second,ed2->second,rcm))
						{
							emap.erase(gridid2);
							pass1survivors.erase(gridid2);
							/* I think this is necessary as these
							are invalidated after an erase */
							emend=emap.end();
							hmend=pass1survivors.end();
						}
					}
				}
			}
		}
		}
		/* All cases land here to save contents of pass1survivors.  In the
		DISTANCEONLY case (only case available in older versions) this is
		unedited.  For others his list is beaten down by more complex algorithms
		above */
		cout << "Writing new hypocentroid table with "<<pass1survivors.size()<<" rows"<<endl;
		HypoMap::iterator hc;
		for(hc=pass1survivors.begin();hc!=pass1survivors.end();++hc)
		{
			dbsave(dbhout,hc->second,gridname);
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
	catch (SeisppError serr)
	{
		serr.log_error();
		exit(-3);
	}
	catch (...)
	{
		elog_die(0,"Unhandled exception");
	}
	exit(0);
}
