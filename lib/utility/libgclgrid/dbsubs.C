#include <string>
#include <cstdio>
#include "gclgrid.h"
// We get byte swap procedures from here.
#include "seispp.h" 
using namespace std;
using namespace SEISPP;
/* Major change June 15, 2004 (GLP):

Previously a gclgrid stored both a lat,lon,r and x1,x2,x3 representation
of the grid.  I realized this was VERY inefficient in memory use so
I dropped the lat,lon,r for internal use, but turned them into 
a function for so what used to be lat[i][j][k] became lat(i,j,k)
(3d example, 2d similar).  

This impacted this group of function strong as a question arises
how to externally represent this type of data.  I chose to external
store these data in the opposite way.  That is, the external files
store lat,lon,r form of the grid.  This made sense as geographic
form is more portable externally not requiring the internal 
transformations that produce the cartesian form.
*/

/* Major change January 2005.
 *
 * Got rid of all char * variables in argument lists.  Left most
 * char used internally as it avoids clashes with plain C of Antelope.
 */
/* June 2007
 * Added support for automatic byte swapping. 
*/



// Constructor creating a GCLgrid object by reading data from
// an Antelope database.  Uses an extension table used to index
// GCLgrid objects.  This is the 3d version.  The 2d Version 
// is below.
//
//Arguments:   
//	db - database to access to look up this grid file under
//	namein - name of requested grid object  (key of the table)
//
//Throws a nonzero integer exception when creation of the GCLgrid 
//object fails.  Every thrown error here means the process failed
//completely so this condition must be caught and handled.  The
//details are assumed to be told to the user through messages
//written through the antelope elog mechanism.
// Feb 2003:  Removed throw function prototypes and function
// declarations.  Although the compiler allowed this I found 
// on reading that this is ignored for a constructor anyway.  
// They stil throw a simple int, which is not very elegant, ut
// since constructors are primitive I see no reason why this
// shouldn't work.  It is as described in the man page.
GCLgrid3d::GCLgrid3d(Dbptr db, string gridname)
{
	Dbptr dbgrd;
	char sstring[256];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	long foff;
	char *base_message=(char *)"Cannot create GCL3Dgrid object: ";
	char *read_error=(char *)"fread failed";
	long nrec;
	int retcode;
	long gridsize;  
	FILE *fp;
	double ***plat,***plon,***pr;
	bool need_to_swap_bytes;

	dbgrd = dblookup(db,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.table == dbINVALID) 
	{
		elog_notify(0,(char *)"%s gclgdisk table not defined in schema definition\n",base_message);
		throw 1;
	}
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 3",
				gridname.c_str());
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,(char *)"%s grid with name %s not found in database\n",
			base_message,gridname.c_str());
		throw 1;
	}
	dbgrd.record = 0;
	name=gridname;
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
        long n1in,n2in,n3in,i0in,j0in,k0in;
	if(dbgetv(dbgrd,0,
		"lat",&(lat0),
		"lon",&(lon0),
		"radius",&(r0),
		"azimuth_y",&(azimuth_y),
		"dx1nom",&(dx1_nom),
		"dx2nom",&(dx2_nom),
		"dx3nom",&(dx3_nom),
		"n1",&(n1in),
		"n2",&(n2in),
		"n3",&(n3in),
		"xlow",&(x1low),
		"xhigh",&(x1high),
		"ylow",&(x2low),
		"yhigh",&(x2high),
		"zlow",&(x3low),
		"zhigh",&(x3high),
		"i0",&(i0in),
		"j0",&(j0in),
		"k0",&(k0in),
		"datatype",datatype,
		"dir",dir,
		"dfile",dfile,
		"foff",&foff,
		NULL) == dbINVALID)
	{
		elog_notify(0,(char *)"%s dbgetv error reading gclgdisk table\n",
			base_message);
		throw 1;
	}
        n1=n1in;
        n2=n2in;
        n3=n3in;
        i0=i0in;
        j0=j0in;
        k0=k0in;
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	azimuth_y = rad(azimuth_y);
	string sdt(datatype);
	bool little_endian=IntelByteOrder();
	if( (sdt=="t8") && little_endian)
		need_to_swap_bytes=true;
	else if ( (sdt=="u8") && !little_endian)
		need_to_swap_bytes=true;
	else
		need_to_swap_bytes=false;
		
	if( (sdt!="t8") && (sdt!="u8") )
	{
		elog_notify(0,(char *)"%s data type %s not allowed.  Currently only support u8 or t8\n",
			base_message, datatype);
		throw 2;
	}
	/* Get the file name to read the gclgrid data from.*/
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"%s Cannot find grid file named %s in directory %s\n",
			base_message,dfile,dir);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"%s file %s cannot be openned for read\n",
			base_message,filename);
		throw 2;
	}
	fseek(fp,foff,SEEK_SET);
	/* We alloc all memory first before reading so we can call a 
	free routine in case any reads fail */
	x1 = create_3dgrid_contiguous(n1,n2,n3);
	x2 = create_3dgrid_contiguous(n1,n2,n3); 
	x3 = create_3dgrid_contiguous(n1,n2,n3); 
	plat = create_3dgrid_contiguous(n1,n2,n3);
	plon = create_3dgrid_contiguous(n1,n2,n3); 
	pr = create_3dgrid_contiguous(n1,n2,n3); 

	//read data from file.  Assume the destructor will be called
	//when throw an exception
	gridsize = (n1)*(n2)*(n3);
	if(fread(plat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading latitude\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}
	if(fread(plon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading longitude\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}

	if(fread(pr[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading radius\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}
	if(need_to_swap_bytes)
	{
		swapdvec(plat[0][0],gridsize);
		swapdvec(plon[0][0],gridsize);
		swapdvec(pr[0][0],gridsize);
	}
	
	fclose(fp);
	// essential  -- cannot convert to cartesian until this is set
	set_transformation_matrix();
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
			{
				Cartesian_point p;
				p=gtoc(plat[i][j][k],plon[i][j][k],
						pr[i][j][k]);
				x1[i][j][k]=p.x1;			
				x2[i][j][k]=p.x2;			
				x3[i][j][k]=p.x3;			
			}
	free_3dgrid_contiguous(plat,n1,n2);
	free_3dgrid_contiguous(plon,n1,n2);
	free_3dgrid_contiguous(pr,n1,n2);
}
/* close companion to the above for 2d grid */
GCLgrid::GCLgrid(Dbptr db,string gridname)
{
	Dbptr dbgrd;
	char sstring[256];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	long foff;
	char *base_message=(char *)"Cannot create GCL2Dgrid object: ";
	char *read_error=(char *)"fread failed";
	long nrec;
	int retcode;
	long gridsize;  
	FILE *fp;
	double **plat, **plon, **pr;
	bool need_to_swap_bytes;

	dbgrd = dblookup(db,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.table == dbINVALID) 
	{
		elog_notify(0,(char *)"%s gclgdisk table not defined in schema definition\n",base_message);
		throw 1;
	}
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 2",gridname.c_str());
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,(char *)"%s grid with name %s not found in database\n",
			base_message,gridname.c_str());
		throw 1;
	}
	dbgrd.record = 0;
	name=gridname;
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
        long n1in,n2in,i0in,j0in;
	if(dbgetv(dbgrd,0,
		(char *)"lat",&(lat0),
		(char *)"lon",&(lon0),
		(char *)"radius",&(r0),
		(char *)"azimuth_y",&(azimuth_y),
		(char *)"dx1nom",&(dx1_nom),
		(char *)"dx2nom",&(dx2_nom),
		(char *)"n1",&(n1in),
		(char *)"n2",&(n2in),
		(char *)"xlow",&(x1low),
		(char *)"xhigh",&(x1high),
		(char *)"ylow",&(x2low),
		(char *)"yhigh",&(x2high),
		(char *)"zlow",&(x3low),
		(char *)"zhigh",&(x3high),
		(char *)"i0",&(i0in),
		(char *)"j0",&(j0in),
		(char *)"datatype",datatype,
		(char *)"dir",dir,
		(char *)"dfile",dfile,
		(char *)"foff",&foff,
		NULL) == dbINVALID)
	{
		elog_notify(0,(char *)"%s dbgetv error reading gclgdisk table\n",
			base_message);
		throw 1;
	}
        n1=n1in;
        n2=n2in;
        i0=i0in;
        j0=j0in;
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	azimuth_y = rad(azimuth_y);
	bool little_endian=IntelByteOrder();
	string sdt(datatype);
	if( (sdt=="t8") && little_endian)
		need_to_swap_bytes=true;
	else if ( (sdt=="u8") && !little_endian)
		need_to_swap_bytes=true;
	else
		need_to_swap_bytes=false;
		
	if( (sdt!="t8") && (sdt!="u8") )
	{
		elog_notify(0,(char *)"%s data type %s not allowed.  Currently only support u8 or t8\n",
			base_message, datatype);
		throw 2;
	}
	/* Get the file name to read the gclgrid data from.*/
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"%s Cannot find grid file named %s in directory %s\n",
			base_message,dfile,dir);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"%s file %s cannot be openned for read\n",
			base_message,filename);
		throw 2;
	}
	fseek(fp,foff,SEEK_SET);
	/* We alloc all memory first before reading so we can call a 
	free routine in case any reads fail */
	x1 = create_2dgrid_contiguous(n1,n2);
	x2 = create_2dgrid_contiguous(n1,n2); 
	x3 = create_2dgrid_contiguous(n1,n2); 
	plat = create_2dgrid_contiguous(n1,n2);
	plon = create_2dgrid_contiguous(n1,n2); 
	pr = create_2dgrid_contiguous(n1,n2); 

	/* read data trapping read errors */
	gridsize = (n1)*(n2);
	if(fread(plat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading latitude\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}
	if(fread(plon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading longitude\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}

	if(fread(pr[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading radius\n",
			base_message,read_error);
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(need_to_swap_bytes)
	{
		swapdvec(plat[0],gridsize);
		swapdvec(plon[0],gridsize);
		swapdvec(pr[0],gridsize);
	}
	set_transformation_matrix();
	int i,j;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
		{
			Cartesian_point p;
			p=gtoc(plat[i][j],plon[i][j],
					pr[i][j]);
			x1[i][j]=p.x1;			
			x2[i][j]=p.x2;			
			x3[i][j]=p.x3;			
		}
	free_2dgrid_contiguous(plat,n1);
	free_2dgrid_contiguous(plon,n1);
	free_2dgrid_contiguous(pr,n1);
}
/* All the field functions need this little function.  It returns true
if the data for the field will need to be byte swapped.  The approach
used here is brutally inefficient, but was preferable in my mind to 
a change in the schema or the interface.  It requires that the field
data and the grid be the same data type.  i.e. if one needs to be swapped
so does the other.  dim must be either 2 or 3.  Needed so the
same function can be used for 2 or 3d fields*/
bool test_for_byteswap(Dbptr db, string gridname,int dim)
{
	// no test needed for any of the db function calls as they
	// repeat parallel calls that had to have succeeded in the constructor
	// called before entry into the field constructor code.
	// Don't ever reuse this function and not fix that detail!
	db=dblookup(db,0,(char *)"gclgdisk",0,0);
	char sstring[256];
	sprintf(sstring,
            "gridname =~ /%s/ && dimensions == %d",
                  gridname.c_str(),dim);
	db=dbsubset(db,sstring,0);
	long nrec;
	dbquery(db,dbRECORD_COUNT,&nrec);
	if(nrec<=0)
	{
		cerr <<"Warning:  test_for_byteswap failed to find a grid named "
			<< gridname<<endl
			<< "Beware orphan gclfield records"<<endl;
		return(false);
	}
	char dtype[4];
	db.record=0;
	dbgetv(db,0,"datatype",dtype,NULL);
	string datatype(dtype);
	dbfree(db);
	bool little_endian=IntelByteOrder();
	if((datatype=="t8") && (little_endian))
		return(true);
	else if( (datatype=="u8") && (!little_endian))
		return(true);
	else
		return(false);
	
}
//
// This set of functions construct fields. 
// Each have the feature that if the fieldname is NULL the grid is constructed
// but the field variable (val) is initialized to 0s.
//
GCLscalarfield::GCLscalarfield(Dbptr db,
		string gclgnamein,
		string fieldname) : GCLgrid(db, gclgnamein)
{
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);

	char sstring[256];
	long foff;
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	long gridsize;
	long nrec;

	dbgrd = dblookup(db,0,(char *)"gclfield",0,0);
	if(dbgrd.table == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
	        throw 1;
        }
	if(fieldname.length()==0)
	{
		val=create_2dgrid_contiguous(n1, n2);
		for(int i=0;i<n1;++i)
			for(int j=0;j<n2;++j)  val[i][j]=0.0;
	}
	else
	{	
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 2 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
			elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
			                        gclgname,fieldname.c_str());
	                throw 1;
	        }
		dbgrd.record = 0;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
			elog_notify(0,(char *)"Cannot find external file for gclfield %s\n",fieldname.c_str());
			throw 2;
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
			elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
					filename,fieldname.c_str());
			throw 2;
		}
		long foff;
		long nv,dim;
		if(dbgetv(dbgrd,0,"foff",&foff,
			"dimensions",&dim,"nv",&nv,NULL)==dbINVALID)
		{
                        fclose(fp);
			elog_notify(0,(char *)"dbgetv error fetching foff from gclfield table\n");
			throw 2;
		}
		if(nv>1)
		{
			elog_notify(0,(char *)"GCLscalarfield constructor nv=%ld is illegal",
				nv);
			throw 2;
		}
		if(dim!=2)
		{
			elog_notify(0,
			 (char *)"GCLscalarfield: database dimension %ld mismatch for fieldname=%s",
				dim,fieldname.c_str());
			throw 2;
		}
		fseek(fp,foff,SEEK_SET);
		dbfree(dbgrd);
		gridsize = n1*n2;
		val=create_2dgrid_contiguous(n1, n2);
		if(fread(val[0],sizeof(double),gridsize,fp) != gridsize)
		{
			elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
			fclose(fp);
			throw 2;
		}
		fclose(fp);
		if(test_for_byteswap(db,gclgname,2))
			swapdvec(val[0],gridsize);
	}
}
//
// Similar constructor for a 3D scalar field
//
GCLscalarfield3d::GCLscalarfield3d(Dbptr db,
		string gclgnamein,
		string fieldname) : GCLgrid3d(db, gclgnamein)
{
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	long foff;
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	long gridsize;
	long nrec;

	if(fieldname.length()==0)
	{
		val=create_3dgrid_contiguous(n1, n2, n3);
		for(int i=0;i<n1;++i)
			for(int j=0;j<n2;++j)  
				for(int k=0;k<n3;++k) val[i][j][k]=0.0;
	}
	else
	{
		dbgrd = dblookup(db,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
			elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		        throw 1;
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
			elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
			                        gclgname,fieldname.c_str());
	                throw 1;
	        }
		dbgrd.record = 0;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
			elog_notify(0,(char *)"Cannot open external file %s for gclfield %s\n",
				filename,fieldname.c_str());
			throw 2;
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
			elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
					filename,fieldname.c_str());
			throw 2;
		}
		long foff;
		long nv,dim;
		if(dbgetv(dbgrd,0,"foff",&foff,
			"dimensions",&dim,"nv",&nv,NULL)==dbINVALID)
		{
                        fclose(fp);
			elog_notify(0,(char *)"dbgetv error fetching foff from gclfield table\n");
			throw 2;
		}
		if(nv>1)
		{
			elog_notify(0,(char *)"GCLscalarfield3d constructor nv=%ld is illegal",
				nv);
			throw 2;
		}
		if(dim!=3)
		{
			elog_notify(0,
			 (char *)"GCLscalarfield3d: database dimension %ld mismatch for fieldname=%s",
				dim,fieldname.c_str());
			throw 2;
		}
		fseek(fp,foff,SEEK_SET);
		dbfree(dbgrd);
		gridsize = n1*n2*n3;
		val=create_3dgrid_contiguous(n1, n2, n3);
		if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
		{
			elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
			fclose(fp);
			throw 2;
		}
		if(test_for_byteswap(db,gclgname,3))
			swapdvec(val[0][0],gridsize);
		fclose(fp);
	}
}
// Note comment below on 3d version of this function applies here too
GCLvectorfield::GCLvectorfield(Dbptr db,
		string gclgnamein,
		string fieldname,
		int nvsize) : GCLgrid(db, gclgnamein)
{
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	long gridsize;
	long nrec;
	if(fieldname.length()==0)
	{
		if(nvsize<=0)
		{
			elog_notify(0,"GCLvectorfield database constructor:  invalid vector length request = %d\n",
				nvsize);
			throw 3;
		}
		nv=nvsize;
		val=create_3dgrid_contiguous(n1, n2, nv);
		for(int i=0;i<n1;++i)
			for(int j=0;j<n2;++j)  
				for(int k=0;k<nv;++k) val[i][j][k]=0.0;
	}
	else
	{	
		dbgrd = dblookup(db,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
			elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		        throw 1;
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 2 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
			elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
			                        gclgname,fieldname.c_str());
	                throw 1;
	        }
		dbgrd.record = 0;
		long nvdb;
		long foff;
		long dim;
		if(dbgetv(dbgrd,0,"nv",&nvdb,"foff",&foff,
			"dimensions",&dim,NULL)==dbINVALID)
		{
			elog_notify(0,(char *)"GCLvectorfield: dbgetv error fetching attributes from gclfield table for field=%s\n",
				fieldname.c_str());
			throw 2;
		}
		if(dim!=2)
		{
			elog_notify(0,
			 (char *)"GCLvectorfield: database dimension %ld mismatch for fieldname=%s",
				dim,fieldname.c_str());
			throw 2;
		}
		if(nvsize>0)
		{
			if(nvdb!=nvsize)
			{
				elog_notify(0,(char *)"GCLvectorfield database constructor warning:\nnvsize=%ld passed to constructor does not match nv in database = %ld\nUsing database value\n",
					nvsize,nvdb);
			}
		}
		nv=nvdb;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
			elog_notify(0,(char *)"Cannot find external file for gclfield %s\n",fieldname.c_str());
			throw 2;
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
			elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
					filename,fieldname.c_str());
			throw 2;
		}
		dbfree(dbgrd);
		gridsize = n1*n2*nv;
		val=create_3dgrid_contiguous(n1, n2, nv);
		fseek(fp,foff,SEEK_SET);
		if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
		{
			elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
			fclose(fp);
			throw 2;
		}
		fclose(fp);
		if(test_for_byteswap(db,gclgname,2))
			swapdvec(val[0][0],gridsize);
	}
}
/* April 2005:  Changed an oddity of this.  Previously there was an odd logic that
allowed fieldname to be null and an nvsize parameter that allowed one to create a
field with all zeros with nvsize components per point.  This was preserved, but 
used a default feature that makes the nzsize parameter not required by most 
callers. */
GCLvectorfield3d::GCLvectorfield3d(Dbptr db,
		string gclgnamein,
		string fieldname,
		int nvsize) : GCLgrid3d(db, gclgnamein)
{
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	long foff;
	char dir[65],dfile[36];
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	long gridsize;
	long nrec;

	if(fieldname.length()==0)
	{
		if(nvsize<=0)
		{
			elog_notify(0,"GCLvectorfield3d database constructor:  invalid vector length request = %ld\n",
				nvsize);
			throw 3;
		}
		nv=nvsize;
		val=create_4dgrid_contiguous(n1, n2, n3, nv);
		for(int i=0;i<n1;++i)
			for(int j=0;j<n2;++j)  
				for(int k=0;k<n3;++k) 
					for(int l=0;l<nv;++l) val[i][j][k][l]=0.0;
	}
	else
	{
		dbgrd = dblookup(db,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
			elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		        throw 1;
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
			elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
			                        gclgname,fieldname.c_str());
	                throw 1;
	        }
		dbgrd.record = 0;
		long nvdb;
		long foff;
		long dim;
		if(dbgetv(dbgrd,0,"nv",&nvdb,"foff",&foff,
			"dimensions",&dim,NULL)==dbINVALID)
		{
			elog_notify(0,(char *)"GCLvectorfield3d: dbgetv error fetching attributes from gclfield table for field=%s\n",
				fieldname.c_str());
			throw 2;
		}
		if(dim!=3)
		{
			elog_notify(0,
			 (char *)"GCLvectorfield3d: database dimension %ld mismatch for fieldname=%s",
				dim,fieldname.c_str());
			throw 2;
		}
		if(nvsize>0)
		{
			if(nvdb!=nvsize)
			{
				elog_notify(0,(char *)"GCLvectorfield3d database constructor warning:\nnvsize=%ld passed to constructor does not match nv in database = %ld\nUsing database value\n",
					nvsize,nvdb);
			}
		}
		nv=nvdb;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
			elog_notify(0,(char *)"Cannot find external file for gclfield %s\n",fieldname.c_str());
			throw 2;
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
			elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
					filename,fieldname.c_str());
			throw 2;
		}
		dbfree(dbgrd);
		gridsize = n1*n2*n3*nv;
		val=create_4dgrid_contiguous(n1, n2, n3,nv);
		fseek(fp,foff,SEEK_SET);
		if(fread(val[0][0][0],sizeof(double),gridsize,fp) != gridsize)
		{
			elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
			fclose(fp);
			throw 2;
		}
		fclose(fp);
		if(test_for_byteswap(db,gclgname,3))
			swapdvec(val[0][0][0],gridsize);
	}
}

/* The following two parallel functions are the inverse of the load
routines above.  That is, they take a db pointer and a pointer to 
a grid object and save the contents to external files and a database.
dir is the directory where the results are stored.  The file name
is always created from the gridname.  

Errors of any kind leave a warning message on the error log and 
invoke a throw.  This should be caught with a simple int handler.
-1 means total failure, positive count is number of i/o errors encounterd.

Author:  G Pavlis
Date:  August 2000
Modified:  December 2002
Converted to C++ with subsequent name change.  Little of the code
changed.
*/
void GCLgrid3d::dbsave(Dbptr dbo, string dirin) throw(int)
{
	char dir[64];
	strncpy(dir,dirin.c_str(),64);
	FILE *fp;
	string filename;
	int fssize;
	long foff;
	long gridsize;
	int dimensions=3;
	char *fwerr=(char *)"fwrite error on file %s";
	Dbptr db;

	db = dblookup(dbo,0,(char *)"gclgdisk",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(dir))
	{
		elog_notify(0,"Cannot create directory %s",dir);
		throw 1;
	}
	/*Save the data first so that in the event of a failure we don't 		
	have to patch the database afterwards.   The data are always 
	saved in a file with the name of the grid*/
	filename = dirin+"/"+string(name);
	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
		elog_notify(0,(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on Solaris where they are switching from
	32 to 64.   This is safe if overkill*/
	foff = ftell(fp);
	gridsize = (n1)*(n2)*(n3);
	// convert to external representation
	double ***plat=create_3dgrid_contiguous(n1,n2,n3);
	double ***plon=create_3dgrid_contiguous(n1,n2,n3);
	double ***pr=create_3dgrid_contiguous(n1,n2,n3);
	int i,j,k;
	Geographic_point p;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
			{
				p = ctog(x1[i][j][k],
					x2[i][j][k],
					x3[i][j][k]);
				plat[i][j][k]=p.lat;
				plon[i][j][k]=p.lon;
				pr[i][j][k]=p.r;
			}
	// brutal to keep pushing fwrites here, but otherwise
	// there is excessive cleanup code.  Any write errors
	// lead to throwing an error.  Write errors like this
	// may well cause the program to (properly) abort anyway
	bool writeok=true;
	if(fwrite(plat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
	}
	if(fwrite(plon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
	}
	if(fwrite(pr[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
	}
	free_3dgrid_contiguous(plat,n1,n2);
	free_3dgrid_contiguous(plon,n1,n2);
	free_3dgrid_contiguous(pr,n1,n2);
	fclose(fp);
	if(!writeok) throw 2;
	string datatype;
	if(IntelByteOrder())
		datatype=string("u8");
	else
		datatype=string("t8");
	/* Now we write a row in the database for this grid.  Note
	some quantities have to be converted from radians to degrees.*/
	if(dbaddv(db,0,
		"gridname",name.c_str(),
		"dimensions",dimensions,
		"lat",deg(lat0),
		"lon",deg(lon0),
		"radius",r0,
		"azimuth_y",deg(azimuth_y),
		"dx1nom",dx1_nom,
		"dx2nom",dx2_nom,
		"dx3nom",dx3_nom,
		"n1",static_cast<long>(n1),
		"n2",static_cast<long>(n2),
		"n3",static_cast<long>(n3),
		"xlow",x1low,
		"xhigh",x1high,
		"ylow",x2low,
		"yhigh",x2high,
		"zlow",x3low,
		"zhigh",x3high,
		"i0",static_cast<long>(i0),
		"j0",static_cast<long>(j0),
		"k0",static_cast<long>(k0),
		"datatype",datatype.c_str(),
		"dir",dir,
		"dfile",name.c_str(),
		"foff",foff,
		NULL) < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 3d grid into gclgdisk table\n");
		throw 1;
	}
}
/* Parallel routine for 2d*/
void GCLgrid::dbsave(Dbptr dbo, string dirin) throw(int)
{
	char dir[64];
	strncpy(dir,dirin.c_str(),64);
	FILE *fp;
	string filename;
	int fssize;
	long foff;
	long gridsize;
	int dimensions=2;
	char *fwerr=(char *)"fwrite error on file %s";

	Dbptr db = dblookup(dbo,0,(char *)"gclgdisk",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(dir))
	{
		elog_notify(0,"Cannot create directory %s",dir);
		throw 1;
	}
	/*Save the data first so that in the event of a failure we don't 		have to patch the database afterwards. */
	filename = dirin+"/"+string(name);
	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
		elog_notify(0,(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = (n1)*(n2);
	// convert to external representation
	double **plat=create_2dgrid_contiguous(n1,n2);
	double **plon=create_2dgrid_contiguous(n1,n2);
	double **pr=create_2dgrid_contiguous(n1,n2);
	int i,j;
	Geographic_point p;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
		{
			p = ctog(x1[i][j],
				x2[i][j],
				x3[i][j]);
			plat[i][j]=p.lat;
			plon[i][j]=p.lon;
			pr[i][j]=p.r;
		}
	bool writeok=true;
	if(fwrite(plat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
		fclose(fp);
		throw 2;
	}
	if(fwrite(plon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
	}
	if(fwrite(pr[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		writeok=false;
	}
	free_2dgrid_contiguous(plat,n1);
	free_2dgrid_contiguous(plon,n1);
	free_2dgrid_contiguous(pr,n1);
	fclose(fp);
	string datatype;
	if(IntelByteOrder())
		datatype=string("u8");
	else
		datatype=string("t8");
	if(!writeok) throw 2;
	/* Now we write a row in the database for this grid*/
	if(dbaddv(db,0,
		"gridname",name.c_str(),
		"dimensions",dimensions,
		"lat",deg(lat0),
		"lon",deg(lon0),
		"radius",r0,
		"azimuth_y",deg(azimuth_y),
		"dx1nom",dx1_nom,
		"dx2nom",dx2_nom,
		"n1",static_cast<long>(n1),
		"n2",static_cast<long>(n2),
		"xlow",x1low,
		"xhigh",x1high,
		"ylow",x2low,
		"yhigh",x2high,
		"zlow",x3low,
		"zhigh",x3high,
		"i0",static_cast<long>(i0),
		"j0",static_cast<long>(j0),
		"datatype",datatype.c_str(),
		"dir",dir,
		"dfile",name.c_str(),
		"foff",foff,
		NULL) < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 2d grid into gclgdisk table\n");
		throw 1;
	}
}
// The following are parallel to the above for 2d and 3d fields.  
// All use a perhaps dangerous assumption if gclgdir is a NULL saving
// the parent GCLgrid is bypassed.  When gclgdir is not null we use
// a dynamic_cast operator to use the base class dbsave routine for the
// parent GCLgrid object.  This allows multiple fields to be associated
// with a single parent GCLgrid object.  Since these things can easily
// get huge this is a good idea although it is more error prone if this
// fact is not understood.  The output filename is created as fielddir+"/"+fieldname.
// The gclfield table is keyed by the combination of the GCLgrid name and 
// the fieldname.  Hence the fieldname must be unique or this routine
// will throw an error by dbaddv.  The output stage that writes data in
// is more forgiving because the file name, dfile, is passed as a separate 
// argument.  
void GCLscalarfield::dbsave(Dbptr dbo, 
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) throw(int)
{
	long gridsize;
	string filename;
	FILE *fp;
	long foff;
	int dimensions=2;
	int nv=1;

	//Save the parent GCLgrid when gclgdir is defined
	//Skip if this string was not defined
	if(gclgdir.length()>0)
	{
		try {
			GCLgrid *g;
			g = dynamic_cast<GCLgrid *> (this);
			g->dbsave(dbo,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	else
	{
		if(test_for_byteswap(dbo,this->name,2) )
			swapdvec(this->val[0],n1*n2);
	}
		
	Dbptr db = dblookup(dbo,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
		elog_notify(0,"Cannot create directory %s",fielddir.c_str());
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
		elog_notify(0,(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2;	
	if(fwrite(val[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		(char *)"gridname",name.c_str(),
		(char *)"dimensions",dimensions,
		(char *)"nv",static_cast<long>(nv),
		(char *)"dir",fielddir.c_str(),
		(char *)"dfile",dfile.c_str(),
		(char *)"foff",foff,
		(char *)"fieldname",fieldname.c_str(),
		NULL)  < 0)
	{
		elog_notify(0,
		  (char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}

void GCLscalarfield3d::dbsave(Dbptr dbo, 
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) throw(int)
{
	long gridsize;
	string filename;
	FILE *fp;
	long foff;
	int dimensions=3;
	int nv=1;

	//Save the parent GCLgrid when gclgdir is defined
	//Skip if this string was not defined
	if(gclgdir.length()>0)
	{
		try {
			GCLgrid3d *g;
			g = dynamic_cast<GCLgrid3d *> (this);
			g->dbsave(dbo,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	else
	{
		if(test_for_byteswap(dbo,this->name,3) )
			swapdvec(this->val[0][0],n1*n2*n3);
	}
	Dbptr db = dblookup(dbo,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,
		 (char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
		elog_notify(0,"Cannot create directory %s",fielddir.c_str());
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),(char *)"a+");
	if(fp==NULL)
	{
		elog_notify(0,
			(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*n3;	
	if(fwrite(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		"gridname",name.c_str(),
		"dimensions",dimensions,
		"nv",static_cast<long>(nv),
		"dir",fielddir.c_str(),
		"dfile",dfile.c_str(),
		"foff",foff,
		"fieldname",fieldname.c_str(),
		NULL)  < 0)
	{
		elog_notify(0,
		  (char *)"dbaddv error for 3d grid into gclfield table\n");
		throw 1;
	}
}
void GCLvectorfield::dbsave(Dbptr dbo, 
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) throw(int)
{
	long gridsize;
	string filename;
	FILE *fp;
	long foff;
	int dimensions=2;

	//Save the parent GCLgrid when gclgdir is defined
	//Skip if this string was not defined
	if(gclgdir.length()>0)
	{
		try {
			GCLgrid *g;
			g = dynamic_cast<GCLgrid *> (this);
			g->dbsave(dbo,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	else
	{
		if(test_for_byteswap(dbo,this->name,2) )
			swapdvec(this->val[0][0],n1*n2*nv);
	}
	Dbptr db = dblookup(dbo,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,
		 (char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
		elog_notify(0,"Cannot create directory %s",fielddir.c_str());
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
		elog_notify(0,
			(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*nv;	
	if(fwrite(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		(char *)"gridname",name.c_str(),
		(char *)"dimensions",dimensions,
		(char *)"nv",static_cast<long>(nv),
		(char *)"dir",fielddir.c_str(),
		(char *)"dfile",dfile.c_str(),
		(char *)"foff",foff,
		(char *)"fieldname",fieldname.c_str(),
		NULL)  < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}

void GCLvectorfield3d::dbsave(Dbptr dbo, 
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) throw(int)
{
	long gridsize;
	string filename;
	FILE *fp;
	long foff;
	int dimensions=3;

	//Save the parent GCLgrid when gclgdir is defined
	//Skip if this string was not defined
	if(gclgdir.length()>0)
	{
		try {
			GCLgrid3d *g;
			g = dynamic_cast<GCLgrid3d *> (this);
			g->dbsave(dbo,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	else
	{
		if(test_for_byteswap(dbo,this->name,3) )
			swapdvec(this->val[0][0][0],n1*n2*n3*nv);
	}
	Dbptr db = dblookup(dbo,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
		elog_notify(0,"Cannot create directory %s",fielddir.c_str());
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),(char *)"a+");
	if(fp==NULL)
	{
		elog_notify(0,(char *)"Cannot open output file %s\nNothing save\n",
			filename.c_str());
		throw 2;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*n3*nv;	
	if(fwrite(val[0][0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		(char *)"gridname",name.c_str(),
		(char *)"dimensions",dimensions,
		(char *)"nv",static_cast<long>(nv),
		(char *)"dir",fielddir.c_str(),
		(char *)"dfile",dfile.c_str(),
		(char *)"foff",foff,
		(char *)"fieldname",fieldname.c_str(),
		NULL)  < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}
