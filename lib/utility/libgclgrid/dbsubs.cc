#include <string>
#include <cstdio>
#include <sstream>
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
/* Nov. 2010 
   Large changes to abstract the interface.  Previous interface
   used an explicit dbsave routine with an antelope Dbptr handle.
   This was changed to an abstract DatabaseHandle which
   a DatascopeHandle (used in this implementation) inherits.
   This allows alternative database access without changing 
   programs that use this library - classic use of polymorphism.

   This implementation was done by patching the older code, not by
   a complete rewrite.  This was practical and sensible since this
   code was well tested and this required little more than a few
   lines of translation of arguments code at the beginning of each
   member procedure.
   */
/* March 2011
   Converted from dbsave to generic save with a generic database handle. */

/* This is a common interface routine to take a generic DatabaseHandle 
   and convert it to a Dbptr.  Pretty much every routine in
   this file uses this routine as a simply way to adapt the older
   code that used a straight Dbptr */
Dbptr ConvertGenericHandle(DatabaseHandle *dbh)
{
    DatascopeHandle *dbhandle;
    /* Most books consider this evil, but this is a quick path 
       to solution for a working model.  DatabaseHandle may 
       likely require a redesign to work correctly as a polymorphic
       type.  For now I simply dynamic_cast and throw an exception if
       the cast fails */
    try {
        DatascopeHandle *dbhandle=dynamic_cast<DatascopeHandle *>(dbh);
        return(dbhandle->db);
    }catch (exception& e)
    {
        throw GCLgridError(
                string("dynamic_cast to DatascopeHandle failed\n")
                + "Currently only a DatascopeHandle is implemented\n"
                + string("System message:  ")+e.what());
    }
}



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
GCLgrid3d::GCLgrid3d(DatabaseHandle& dbh, string gridname, bool fl)
{
    fast_lookup=fl;
    const string base_error("GCLgrid3d Database constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char sstring[256];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	long foff;
	long nrec;
	int retcode;
	long gridsize;  
	FILE *fp;
	double ***plat,***plon,***pr;
	bool need_to_swap_bytes;

	dbgrd = dblookup(dbgrd,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.table == dbINVALID) 
            throw GCLgridError(base_error
                    + "gclgdisk table lookup failed\n"
                    + "Table is probably not defined in the schema\n");
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 3",
				gridname.c_str());
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
            throw GCLgridError(base_error
                    + "grid with name="
                    + gridname
                    + " not found in gclgdisk");
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
            throw GCLgridError(base_error
                    + "dbgetv error reading gclgdisk table");
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
            throw GCLgridError(base_error
                    + "data type=" + datatype
                    + "not allowed.\n"
                    + "Can only handle u8 or t8\n");
	}
	/* Get the file name to read the gclgrid data from.*/
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
            throw GCLgridError(base_error
                    + "Cannot find grid file named "
                    + dfile 
                    + " in directory="+dir);
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
            throw GCLgridError(base_error
                    + "fopen failed on file " + filename);
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
	//when throw an exception is thrown to release memory
	gridsize = (n1)*(n2)*(n3);
	if(fread(plat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
	    fclose(fp);
            throw GCLgridError(base_error
                    + "fread error while reading latitude array");
	}
	if(fread(plon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
	    fclose(fp);
            throw GCLgridError(base_error
                    + "fread error while reading longitude array");
	}

	if(fread(pr[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
	    fclose(fp);
            throw GCLgridError(base_error
                    + "fread error while reading radius array");
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
GCLgrid::GCLgrid(DatabaseHandle&  dbh,string gridname)
{
    const string base_error("GCLgrid Database constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
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

	dbgrd = dblookup(dbgrd,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.table == dbINVALID) 
            throw GCLgridError(base_error
                    + "gclgdisk table not defined in schema definition");
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 2",gridname.c_str());
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
        {
            stringstream ss;
            ss << base_error<<"grid with name "<<gridname
                << " not found in database"<<endl
                <<"Check gclgdisk table contents"<<endl;
            throw GCLgridError(ss.str());
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
            throw GCLgridError(base_error
                    + "dbgetv error reading gclgdisk table");
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
            stringstream ss;
            ss << base_error << "datatype attribute"
                << datatype << " not allowd."<<endl
                << "Currently only support u8 or t8"<<endl;
            throw GCLgridError(ss.str());
	}
	/* Get the file name to read the gclgrid data from.*/
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
            stringstream ss;
            ss << base_error<<"Cannot find grid filename="
                <<dfile<<" in directory="<<dir<<endl;
            throw GCLgridError(ss.str());
        }
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
            stringstream ss;
            ss << base_error<<" Cannot open file "<<filename
                << " for reading"<<endl;
            throw GCLgridError(ss.str());
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
            fclose(fp);
            stringstream ss;
            ss << base_error<<read_error
                << " reading latitude data from file="<<filename<<endl;
            throw GCLgridError(ss.str());
	}
	if(fread(plon[0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error<<read_error
                << " reading longitude data from file="<<filename<<endl;
            throw GCLgridError(ss.str());
	}

	if(fread(pr[0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error<<read_error
                << " reading radius data from file="<<filename<<endl;
            throw GCLgridError(ss.str());
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
GCLscalarfield::GCLscalarfield(DatabaseHandle& dbh,
		string gclgnamein,
		string fieldname) : GCLgrid(dbh, gclgnamein)
{
    const string base_error("GCLscalarfield database constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);

	char sstring[256];
	long foff;
	char filename[512];
	FILE *fp;
	long gridsize;
	long nrec;

	dbgrd = dblookup(dbgrd,0,(char *)"gclfield",0,0);
	if(dbgrd.table == dbINVALID)
            throw GCLgridError(base_error
                    + "lookup failed for gclfield table.\n"
                    + "This extension table is probably not installed\n");
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
                    throw GCLgridError(base_error
                            +"Grid with name="
                            +gclgnamein
                            + " and fieldname="
                            +fieldname
                            + "was not found in database");
		dbgrd.record = 0;
		if(dbextfile(dbgrd,0,filename) <=0)
                    throw GCLgridError(base_error
                            +"Cannot find external file for gclfield "
                            +fieldname
                            + " associated with grid "
                            +gclgnamein); 
		fp = fopen(filename,"r");
		if(fp == NULL)
                    throw GCLgridError(base_error
                            + "fopen failed on file "
                            + filename
                            + " which is expected to hold data for field "
                            + fieldname);
		long foff;
		long nv,dim;
		if(dbgetv(dbgrd,0,"foff",&foff,
			"dimensions",&dim,"nv",&nv,NULL)==dbINVALID)
                {
                    fclose(fp);
                    throw GCLgridError(base_error
                            + "dbgetv error fetching foff from gclfield table");
                }
		if(nv>1)
                {
                    fclose(fp);
                    throw GCLgridError(base_error
                            + "Scalar constructor found nv>1");
                }
		fseek(fp,foff,SEEK_SET);
		gridsize = n1*n2;
		val=create_2dgrid_contiguous(n1, n2);
		fclose(fp);
		if(fread(val[0],sizeof(double),gridsize,fp) != gridsize)
                {
                    dbfree(dbgrd);
                    throw GCLgridError(base_error
                            + "fread error reading field values from file "
                            + filename);
		}
		if(test_for_byteswap(dbgrd,gclgname,2))
			swapdvec(val[0],gridsize);
		dbfree(dbgrd);
	}
}
//
// Similar constructor for a 3D scalar field
//
GCLscalarfield3d::GCLscalarfield3d(DatabaseHandle& dbh,
		string gclgnamein,
		string fieldname) : GCLgrid3d(dbh, gclgnamein)
{
    const string base_error("GCLscalarfield3d Database Constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	long foff;
	char filename[512];
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
		dbgrd = dblookup(dbgrd,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
                    throw GCLgridError(base_error+"lookup failed for gclfield table.\n"
                            +"Extension table probably not defined");
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
                    stringstream ss;
                    ss << base_error<<"Grid with name="
                        <<gclgname<<" and fieldname="<<fieldname
                        <<" was not found in database"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
	        }
		dbgrd.record = 0;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
                    stringstream ss;
                    ss << base_error<<"External file "
                        <<filename<<" containing data for fieldname="
                        <<fieldname<<" does not exist"<<endl
                        <<"You probably copied database incorrectly";
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
                    stringstream ss;
                    ss << base_error<<"Cannot open external file "
                        <<filename<<" containing data for fieldname="
                        <<fieldname<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		long foff;
		long nv,dim;
		if(dbgetv(dbgrd,0,"foff",&foff,
			"dimensions",&dim,"nv",&nv,NULL)==dbINVALID)
		{
                    fclose(fp);
                    stringstream ss;
                    ss << base_error<<"dbgetv error fetching foff from gclfield table"
                        <<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(nv>1)
		{
                    fclose(fp);
                    stringstream ss;
                    ss << base_error<<"nv="<<nv<<" must be 1 for a scalar field"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(dim!=3)
		{
                    fclose(fp);
                    stringstream ss;
                    ss << base_error<<"database dimension attribute="<<dim
                        <<" for fieldname="<<fieldname
                        <<" is inconsistent with request"<<endl
                        <<"Must be 3 for a GCLscalarfield3d object"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		fseek(fp,foff,SEEK_SET);
		gridsize = n1*n2*n3;
		val=create_3dgrid_contiguous(n1, n2, n3);
		if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
		{
                    fclose(fp);
                    stringstream ss;
                    ss << base_error<<"Error reading field data from external file="
                        <<filename<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(test_for_byteswap(dbgrd,gclgname,3))
			swapdvec(val[0][0],gridsize);
		dbfree(dbgrd);
		fclose(fp);
	}
}
// Note comment below on 3d version of this function applies here too
GCLvectorfield::GCLvectorfield(DatabaseHandle& dbh,
		string gclgnamein,
		string fieldname,int nvsize) : GCLgrid(dbh, gclgnamein)
{
    const string base_error("GCLvectorfield Database Constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	char filename[512];
	FILE *fp;
	long gridsize;
	long nrec;
	if(fieldname.length()==0)
	{
		if(nvsize<=0)
		{
                    stringstream ss;
                    ss << base_error<<"invalid vector length request nv="
                        <<nvsize<<endl;
                    throw GCLgridError(ss.str());
		}
		nv=nvsize;
		val=create_3dgrid_contiguous(n1, n2, nv);
		for(int i=0;i<n1;++i)
			for(int j=0;j<n2;++j)  
				for(int k=0;k<nv;++k) val[i][j][k]=0.0;
	}
	else
	{	
		dbgrd = dblookup(dbgrd,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
                    dbfree(dbgrd);
                    throw GCLgridError(base_error
                            + "lookup failed for gclfield table.");
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 2 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
                    stringstream ss;
                    ss << base_error<<"Grid with name="
                        <<gclgname<<" and fieldname="<<fieldname
                        <<" not found in database"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
	        }
		dbgrd.record = 0;
		long nvdb;
		long foff;
		long dim;
		if(dbgetv(dbgrd,0,"nv",&nvdb,"foff",&foff,
			"dimensions",&dim,NULL)==dbINVALID)
		{
                    stringstream ss;
                    ss << base_error<<"dbgetv error fetching attributes from"
                        <<" gclfield table."<<endl
                        <<"Constructor failed for fieldname="<<fieldname<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(dim!=2)
		{
                    stringstream ss;
                    ss << base_error<<"database dimension attribute="
                        <<dim<<" is inconsistent with expected value of 2"
                        <<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(nvsize>0)
		{
			if(nvdb!=nvsize)
			{
                            stringstream ss;
                            ss << base_error<<"Expected vector size="<<nvsize
                                <<" does not match size of "<<nvdb
                                <<" tabulated in the database"<<endl;
                            dbfree(dbgrd);
                            throw GCLgridError(ss.str());
			}
		}
		nv=nvdb;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
                    stringstream ss;
                    ss << base_error<<"Cannot find external file "
                        <<filename<<" containing data for fieldname="
                        <<fieldname<<endl
                        <<"Remember external files are only indexed in the db"
                        <<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
                    stringstream ss;
                    ss << base_error<<"Open failed on file "<<filename
                        <<" containing data for field name "
                        <<fieldname<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		gridsize = n1*n2*nv;
		val=create_3dgrid_contiguous(n1, n2, nv);
		fseek(fp,foff,SEEK_SET);
		if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
		{
                    fclose(fp);
                    dbfree(dbgrd);
                    stringstream ss;
                    ss << base_error<<"fread error reading field data from file "
                        <<filename<<endl;
                    throw GCLgridError(ss.str());
		}
		fclose(fp);
		if(test_for_byteswap(dbgrd,gclgname,2))
			swapdvec(val[0][0],gridsize);
		dbfree(dbgrd);
	}
}
/* April 2005:  Changed an oddity of this.  Previously there was an odd logic that
allowed fieldname to be null and an nvsize parameter that allowed one to create a
field with all zeros with nvsize components per point.  This was preserved, but 
used a default feature that makes the nzsize parameter not required by most 
callers. */
GCLvectorfield3d::GCLvectorfield3d(DatabaseHandle& dbh,
		string gclgnamein,
		string fieldname,
		int nvsize) : GCLgrid3d(dbh, gclgnamein)
{
    const string base_error("GCLvectorfield3d Database Constructor:  ");
    Dbptr dbgrd;
    try {
        dbgrd=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char gclgname[16];
	strncpy(gclgname,gclgnamein.c_str(),16);
	char sstring[256];
	long foff;
	char dir[65],dfile[36];
	char filename[512];
	FILE *fp;
	long gridsize;
	long nrec;

	if(fieldname.length()==0)
	{
		if(nvsize<=0)
		{
                    stringstream ss;
                    ss << base_error<<"invalid vector length request of "
                        <<nvsize<<" components per grid point"<<endl;
                    throw GCLgridError(ss.str());
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
		dbgrd = dblookup(dbgrd,0,(char *)"gclfield",0,0);
		if(dbgrd.table == dbINVALID)
		{
                    throw GCLgridError(base_error
                            + "lookup failed for gclfield table.");
	        }
		sprintf(sstring,
			"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
			gclgname,fieldname.c_str());
		dbgrd = dbsubset(dbgrd,sstring,0);
		dbquery(dbgrd,dbRECORD_COUNT,&nrec);
		if(nrec <= 0)
		{
                    stringstream ss;
                    ss << base_error<<"Grid with name="<<gclgname
                        <<" and fieldname="<<fieldname
                        <<" is not defined in input database"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
	        }
		dbgrd.record = 0;
		long nvdb;
		long foff;
		long dim;
		if(dbgetv(dbgrd,0,"nv",&nvdb,"foff",&foff,
			"dimensions",&dim,NULL)==dbINVALID)
		{
                    stringstream ss;
                    ss << base_error<<"dbgetv error fetching attributes from "
                        << "gclfield table for fieldname="<<fieldname<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(dim!=3)
		{
                    stringstream ss;
                    ss << base_error<<"field dimension mismatch for gridname="
                        <<gclgname
                        <<" and fieldname="
                        <<fieldname<<endl
                        <<"Database has dimension="<<dim<<" but must be 3 "
                        <<"for a 3d vector field object"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		if(nvsize>0)
		{
			if(nvdb!=nvsize)
			{
                            stringstream ss;
                            ss << base_error<<"vector component size mismatch"<<endl
                                <<"gridname="<<gclgname<<", fieldname="<<fieldname<<endl
                                <<"Constructor expected number of components="<<nvsize
                                <<" but database defines nv="<<nvdb<<" for this field"
                                <<endl;
                            dbfree(dbgrd);
                            throw GCLgridError(ss.str());
			}
		}
		nv=nvdb;
		if(dbextfile(dbgrd,0,filename) <=0)
		{
                    stringstream ss;
                    ss << base_error<<"Cannot find external file="<<filename
                        <<endl
                        <<"Defined as data for fieldname="<<fieldname
                        <<" associated with gridname="<<gclgname<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		fp = fopen(filename,"r");
		if(fp == NULL)
		{
                    stringstream ss;
                    ss << base_error
                        <<"gridname="<<gclgname<<" fieldname="<<fieldname<<endl
                        <<"Cannot open file "<<filename<<" for reading"<<endl;
                    dbfree(dbgrd);
                    throw GCLgridError(ss.str());
		}
		gridsize = n1*n2*n3*nv;
		val=create_4dgrid_contiguous(n1, n2, n3,nv);
		fseek(fp,foff,SEEK_SET);
		if(fread(val[0][0][0],sizeof(double),gridsize,fp) != gridsize)
		{
                    fclose(fp);
                    dbfree(dbgrd);
                    stringstream ss;
                    ss << base_error
                        <<"gridname="<<gclgname<<" fieldname="<<fieldname<<endl
                        <<"Error reading field values from file "<<filename<<endl;
                    throw GCLgridError(ss.str());
		}
		fclose(fp);
		if(test_for_byteswap(dbgrd,gclgname,3))
			swapdvec(val[0][0][0],gridsize);
		dbfree(dbgrd);
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
March 2011:  converted to use generic db handle
AND int errors were all changed to an error object that is a child
of std::exception
*/
void GCLgrid3d::save(DatabaseHandle& dbh, string dirin) 
{
    const string base_error("GCLgrid3::save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char dir[64];
	strncpy(dir,dirin.c_str(),64);
	FILE *fp;
	string filename;
	int fssize;
	long foff;
	long gridsize;
	int dimensions=3;
        string fwerr("fwrite error on file ");

	db = dblookup(db,0,(char *)"gclgdisk",0,0);
	if(db.table == dbINVALID)
	{
            throw GCLgridError(base_error
                    + "lookup failed for gclgdisk table.");
	}
	/* Insure the directory exists */
	if(makedir(dir))
	{
            stringstream ss;
            ss << base_error
                << "Cannot create directory "
                << dir<<endl;
            throw GCLgridError(ss.str());
	}
	/*Save the data first so that in the event of a failure we don't 		
	have to patch the database afterwards.   The data are always 
	saved in a file with the name of the grid*/
	filename = dirin+"/"+string(name);
	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"Try to load gridname="<<this->name<<endl
                <<"Open failed for output file="<<filename<<endl; 
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing latitude section"<<endl;
            fclose(fp);
            throw GCLgridError(ss.str());
        }
	if(fwrite(plon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing longitude section"<<endl;
            fclose(fp);
            throw GCLgridError(ss.str());
	}
	if(fwrite(pr[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing radius section"<<endl;
            fclose(fp);
            throw GCLgridError(ss.str());
	}
	free_3dgrid_contiguous(plat,n1,n2);
	free_3dgrid_contiguous(plon,n1,n2);
	free_3dgrid_contiguous(pr,n1,n2);
	fclose(fp);
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
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                <<"dbaddv error saving attributes defining grid in gclgdisk table"
                <<endl;
            throw GCLgridError(ss.str());
	}
}
/* Parallel routine for 2d*/
void GCLgrid::save(DatabaseHandle& dbh, string dirin) 
{
    const string base_error("GCLgrid::save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
	char dir[64];
	strncpy(dir,dirin.c_str(),64);
	FILE *fp;
	string filename;
	int fssize;
	long foff;
	long gridsize;
	int dimensions=2;
        string fwerr("fwrite error on file ");

	db = dblookup(db,0,(char *)"gclgdisk",0,0);
	if(db.table == dbINVALID)
	{
            throw GCLgridError(base_error
                    + "lookup failed for gclgdisk table.");
	}
	/* Insure the directory exists */
	if(makedir(dir))
	{
            stringstream ss;
            ss << base_error
                << "Cannot create directory "
                << dir<<endl;
            throw GCLgridError(ss.str());
	}
	/*Save the data first so that in the event of a failure we don't 		have to patch the database afterwards. */
	filename = dirin+"/"+string(name);
	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"Try to load gridname="<<this->name<<endl
                <<"Open failed for output file="<<filename<<endl; 
            throw GCLgridError(ss.str());
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
	if(fwrite(plat[0],sizeof(double),gridsize,fp) != gridsize)
	{
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing latitude section"<<endl;
	    fclose(fp);
            throw GCLgridError(ss.str());
	}
	if(fwrite(plon[0],sizeof(double),gridsize,fp) != gridsize)
	{
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing longitude section"<<endl;
	    fclose(fp);
            throw GCLgridError(ss.str());
	}
	if(fwrite(pr[0],sizeof(double),gridsize,fp) != gridsize)
	{
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                << fwerr << filename
                <<" writing radius section"<<endl;
	    fclose(fp);
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"Trying to save gridname="<<this->name<<endl
                <<"dbaddv error saving attributes defining grid in gclgdisk table"
                <<endl;
            throw GCLgridError(ss.str());
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
void GCLscalarfield::save(DatabaseHandle& dbh,
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) 
{
    const string base_error("GCLscalarfield::save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
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
			g->save(dbh,gclgdir);
		}
                catch(...){throw;};
	}
	else
	{
		if(test_for_byteswap(db,this->name,2) )
			swapdvec(this->val[0],n1*n2);
	}
		
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
            throw GCLgridError(base_error
                    + "lookup failed for gclgrid table.");
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot create directory "<<fielddir<<endl;
            throw GCLgridError(ss.str());
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot open output field data output file "
                <<filename<<" Save method failed."<<endl;
            throw GCLgridError(ss.str());
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2;	
	if(fwrite(val[0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"fwrite error on output file "<<filename<<endl;
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"dbaddv error writing attributes for gclfield table"<<endl;
            throw GCLgridError(ss.str());
	}
}

void GCLscalarfield3d::save(DatabaseHandle& dbh,
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) 
{
    const string base_error("GCLscalarfield3d::save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
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
			g->save(dbh,gclgdir);
		}
                catch(...){throw;};
	}
	else
	{
		if(test_for_byteswap(db,this->name,3) )
			swapdvec(this->val[0][0],n1*n2*n3);
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"lookup failed for gclfield table."<<endl;
            throw GCLgridError(ss.str());
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot create directory "<<fielddir<<endl;
            throw GCLgridError(ss.str());
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),(char *)"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                << "cannot open output file "<<filename<<endl;
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*n3;	
	if(fwrite(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"fwrite error writing field output file "<<filename<<endl;
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"dbaddv error writing attributes to gclfield table"<<endl;
            throw GCLgridError(ss.str());
	}
}
void GCLvectorfield::save(DatabaseHandle& dbh,
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile) 
{
    const string base_error("GCLvectorfield::save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
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
			g->save(dbh,gclgdir);
		}
                catch(...){throw;};
	}
	else
	{
		if(test_for_byteswap(db,this->name,2) )
			swapdvec(this->val[0][0],n1*n2*nv);
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"lookup failed for gclfield table."<<endl;
            throw GCLgridError(ss.str());
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot create directory "<<fielddir<<endl;
            throw GCLgridError(ss.str());
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot open output field data file "<<filename<<endl;
            throw GCLgridError(ss.str());
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*nv;	
	if(fwrite(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"fwrite error on output file "<<filename<<endl;
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"dbaddv writing attributes to gclfield table"<<endl;
            throw GCLgridError(ss.str());
        }
}

void GCLvectorfield3d::save(DatabaseHandle& dbh,
	string gclgdir,
	string fielddir,
	string fieldname,
	string dfile)
{
    const string base_error("GCLvectorfield3d save method:  ");
    Dbptr db;
    try {
        db=ConvertGenericHandle(&dbh);
    } catch (GCLgridError& gerr)
    {
        throw GCLgridError(base_error+"\n"+gerr.what()+"\n");
    }
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
			g->save(dbh,gclgdir);
		}
                catch(...){throw;};
	}
	else
	{
		if(test_for_byteswap(db,this->name,3) )
			swapdvec(this->val[0][0][0],n1*n2*n3*nv);
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.table == dbINVALID)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"lookup failed for gclfield table."<<endl;
            throw GCLgridError(ss.str());
	}
	/* Insure the directory exists */
	if(makedir(const_cast<char *>(fielddir.c_str())))
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot create directory "<<fielddir<<endl;
            throw GCLgridError(ss.str());
	}
	//First create a filename and save the val array with a binary write
	filename = fielddir+"/"+dfile;

	fp = fopen(filename.c_str(),(char *)"a+");
	if(fp==NULL)
	{
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"Cannot open output file "<<filename<<endl;
            throw GCLgridError(ss.str());
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	foff = ftell(fp);
	gridsize = n1*n2*n3*nv;	
	if(fwrite(val[0][0][0],sizeof(double),gridsize,fp) != gridsize)
	{
            fclose(fp);
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"fwrite error on field data output file "<<filename<<endl;
            throw GCLgridError(ss.str());
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
            stringstream ss;
            ss << base_error
                <<"gridname="<<this->name<<" fieldname="<<fieldname<<endl
                <<"dbaddv error writing attributes to gclfield table"<<endl;
            throw GCLgridError(ss.str());
	}
}
