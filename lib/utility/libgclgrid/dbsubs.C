#include <string>
#include <cstdio>
#include "gclgrid.h"
using namespace std;
// Constructor creating a GCLgrid object by reading data from
// an Antelope database.  Uses an extension table used to index
// GCLgrid objects.  This is the 3d version.  The 2d Version 
// is below.
//
//Arguments:   
//	db - database to access to look up this grid file under
//	gridnmae - name of requested grid object  (key of the table)
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
GCLgrid3d::GCLgrid3d(Dbptr db, char *gridname) 
{
	Dbptr dbgrd;
	char sstring[80];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	int foff;
	char *base_message=(char *)"Cannot create GCL3Dgrid object: ";
	char *read_error=(char *)"fread failed";
	int nrec;
	char cdef[2],gdef[2];
	int retcode;
	int gridsize;  
	FILE *fp;

	dbgrd = dblookup(db,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.record == dbINVALID) 
	{
		elog_notify(0,(char *)"%s gclgdisk table not defined in schema definition\n",base_message);
		throw 1;
	}
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 3",gridname);
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,(char *)"%s grid with name %s not found in database\n",
			base_message,gridname);
		throw 1;
	}
	dbgrd.record = 0;
	strcpy(name,gridname);
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
	if(dbgetv(dbgrd,0,
		"lat",&(lat0),
		"lon",&(lon0),
		"radius",&(r0),
		"azimuth_y",&(azimuth_y),
		"dx1nom",&(dx1_nom),
		"dx2nom",&(dx2_nom),
		"dx3nom",&(dx3_nom),
		"n1",&(n1),
		"n2",&(n2),
		"n3",&(n3),
		"xlow",&(x1low),
		"xhigh",&(x1high),
		"ylow",&(x2low),
		"yhigh",&(x2high),
		"zlow",&(x3low),
		"zhigh",&(x3high),
		"i0",&(i0),
		"j0",&(j0),
		"k0",&(k0),
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype",datatype,
		"dir",dir,
		"dfile",dfile,
		"foff",&foff,
		0) == dbINVALID)
	{
		elog_notify(0,(char *)"%s dbgetv error reading gclgdisk table\n",
			base_message);
		throw 1;
	}
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	azimuth_y = rad(azimuth_y);
	if(!strcmp(cdef,"n") || !strcmp(gdef,"n") )
	{
		elog_notify(0,(char *)"%s Cartesian and Geographical mapping (cdefined and geodefined attributes) must both be defined for input\n",
			base_message);
		throw 1;
	}
	else
	{
		cartesian_defined=1;
		geographic_defined=1;
	}
	if(strcmp(datatype,"t8"))
	{
		elog_notify(0,(char *)"%s data type %s not allowed.  Currently only support t8\n",
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
	lat = create_3dgrid_contiguous(n1,n2,n3);
	lon = create_3dgrid_contiguous(n1,n2,n3); 
	r = create_3dgrid_contiguous(n1,n2,n3); 

	//read data from file.  Assume the destructor will be called
	//when throw an exception
	gridsize = (n1)*(n2)*(n3);
	if(fread(x1[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x1\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(x2[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x2\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(x3[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x3\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(lat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading latitude\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(lon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading longitude\n",
			base_message,read_error);
		throw 2;
	}

	if(fread(r[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading radius\n",
			base_message,read_error);
		throw 2;
	}
	set_transformation_matrix();
}
/* close companion to the above for 2d grid */
GCLgrid::GCLgrid(Dbptr db,char *gridname) 
{
	Dbptr dbgrd;
	char sstring[80];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	int foff;
	char *base_message=(char *)"Cannot create GCL2Dgrid object: ";
	char *read_error=(char *)"fread failed";
	int nrec;
	char cdef[2],gdef[2];
	int retcode;
	int gridsize;  
	FILE *fp;

	dbgrd = dblookup(db,0,(char *)"gclgdisk",0,0); 
	if(dbgrd.record == dbINVALID) 
	{
		elog_notify(0,(char *)"%s gclgdisk table not defined in schema definition\n",base_message);
		throw 1;
	}
	sprintf(sstring,(char *)"gridname =~ /%s/ && dimensions == 2",gridname);
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,(char *)"%s grid with name %s not found in database\n",
			base_message,gridname);
		throw 1;
	}
	dbgrd.record = 0;
	strcpy(name,gridname);
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
	if(dbgetv(dbgrd,0,
		(char *)"lat",&(lat0),
		(char *)"lon",&(lon0),
		(char *)"radius",&(r0),
		(char *)"azimuth_y",&(azimuth_y),
		(char *)"dx1nom",&(dx1_nom),
		(char *)"dx2nom",&(dx2_nom),
		(char *)"n1",&(n1),
		(char *)"n2",&(n2),
		(char *)"xlow",&(x1low),
		(char *)"xhigh",&(x1high),
		(char *)"ylow",&(x2low),
		(char *)"yhigh",&(x2high),
		(char *)"zlow",&(x3low),
		(char *)"zhigh",&(x3high),
		(char *)"i0",&(i0),
		(char *)"j0",&(j0),
		(char *)"cdefined",cdef,
		(char *)"geodefined",gdef,
		(char *)"datatype",datatype,
		(char *)"dir",dir,
		(char *)"dfile",dfile,
		(char *)"foff",&foff,
		0) == dbINVALID)
	{
		elog_notify(0,(char *)"%s dbgetv error reading gclgdisk table\n",
			base_message);
		throw 1;
	}
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	azimuth_y = rad(azimuth_y);

	if(!strcmp(cdef,(char *)"n") || !strcmp(gdef,(char *)"n") )
	{
		elog_notify(0,(char *)"%s Cartesian and Geographical mapping (cdefined and geodefined attributes) must both be defined for input\n",
			base_message);
		throw 1;
	}
	else
	{
		cartesian_defined=1;
		geographic_defined=1;
	}
	if(strcmp(datatype,"t8"))
	{
		elog_notify(0,(char *)"%s data type %s not allowed.  Currently only support t8\n",
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
	lat = create_2dgrid_contiguous(n1,n2);
	lon = create_2dgrid_contiguous(n1,n2); 
	r = create_2dgrid_contiguous(n1,n2); 

	/* read data trapping read errors */
	gridsize = (n1)*(n2);
	if(fread(x1[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x1\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(x2[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x2\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(x3[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading x3\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(lat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading latitude\n",
			base_message,read_error);
		throw 2;
	}
	if(fread(lon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading longitude\n",
			base_message,read_error);
		throw 2;
	}

	if(fread(r[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"%s %s reading radius\n",
			base_message,read_error);
		throw 2;
	}
	set_transformation_matrix();
}
//
// This set of functions construct fields.  The algorithm
// used is memory intensive because we end up creating\
// two copies of the GCLgrid during creation.  We assume
// the one is destroyed when it goes out of scope.
//
GCLscalarfield::GCLscalarfield(Dbptr db,
		char *gclgname,
		char *fieldname) : GCLgrid(db, gclgname) 
{
	char sstring[80];
	int foff;
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	int gridsize;
	int nrec;

	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
	        throw 1;
        }
	sprintf(sstring,
		"gridname =~ /%s/ && dimensions == 2 && fieldname =~ /%s/",
		gclgname,fieldname);
	dbgrd = dbsubset(db,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0)
	{
		elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
		                        gclgname,fieldname);
                throw 1;
        }
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"Cannot file external file for gclfield %s\n",fieldname);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
				filename,fieldname);
		throw 2;
	}
	dbfree(dbgrd);
	gridsize = n1*n2;
	val=create_2dgrid_contiguous(n1, n2);
	if(fread(val[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
		throw 2;
	}
}
//
// Similar constructor for a 3D scalar field
//
GCLscalarfield3d::GCLscalarfield3d(Dbptr db,
		char *gclgname,
		char *fieldname) : GCLgrid3d(db, gclgname) 
{
	char sstring[80];
	int foff;
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	int gridsize;
	int nrec;

	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
	        throw 1;
        }
	sprintf(sstring,
		"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
		gclgname,fieldname);
	dbgrd = dbsubset(db,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0)
	{
		elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
		                        gclgname,fieldname);
                throw 1;
        }
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"Cannot open external file %s for gclfield %s\n",
			filename,fieldname);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
				filename,fieldname);
		throw 2;
	}
	dbfree(dbgrd);
	gridsize = n1*n2*n3;
	val=create_3dgrid_contiguous(n1, n2, n3);
	if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
		throw 2;
	}
}
GCLvectorfield::GCLvectorfield(Dbptr db,
		char *gclgname,
		char *fieldname)  : GCLgrid(db, gclgname)
{
	char sstring[80];
	int foff;
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	int gridsize;
	int nrec;

	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
	        throw 1;
        }
	sprintf(sstring,
		"gridname =~ /%s/ && dimensions == 2 && fieldname =~ /%s/",
		gclgname,fieldname);
	dbgrd = dbsubset(db,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0)
	{
		elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
		                        gclgname,fieldname);
                throw 1;
        }
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"Cannot file external file for gclfield %s\n",fieldname);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
				filename,fieldname);
		throw 2;
	}
	dbfree(dbgrd);
	gridsize = n1*n2*nv;
	val=create_3dgrid_contiguous(n1, n2, nv);
	if(fread(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
		throw 2;
	}
}
GCLvectorfield3d::GCLvectorfield3d(Dbptr db,
		char *gclgname,
		char *fieldname)  : GCLgrid3d(db, gclgname)
{
	char sstring[80];
	int foff;
	char dir[65],dfile[36];
	char filename[512];
	Dbptr dbgrd;
	FILE *fp;
	int gridsize;
	int nrec;

	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
	        throw 1;
        }
	sprintf(sstring,
		"gridname =~ /%s/ && dimensions == 3 && fieldname =~ /%s/",
		gclgname,fieldname);
	dbgrd = dbsubset(db,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0)
	{
		elog_notify(0,(char *)"Grid with name=%s and fieldname= %s not found in database\n",
		                        gclgname,fieldname);
                throw 1;
        }
	dbgrd.record = 0;
	if(dbextfile(dbgrd,0,filename) <=0)
	{
		elog_notify(0,(char *)"Cannot file external file for gclfield %s\n",fieldname);
		throw 2;
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		elog_notify(0,(char *)"Cannot open file %s to read gclfield %s\n",
				filename,fieldname);
		throw 2;
	}
	dbfree(dbgrd);
	gridsize = n1*n2*n3*nv;
	val=create_4dgrid_contiguous(n1, n2, n3,nv);
	if(fread(val[0][0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"Error reading field values from file %s\n",filename);
		throw 2;
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
void GCLgrid3d::dbsave(Dbptr db, char *dir) throw(int)
{
	FILE *fp;
	string filename;
	int fssize;
	long int fpos;
	int foff;
	int gridsize;
	int dimensions=3;
	char *fwerr=(char *)"fwrite error on file %s";
	char *cdef=(char *)"d",*gdef=(char *)"d";

	db = dblookup(db,0,(char *)"gclgdisk",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		throw 1;
	}
	/*Save the data first so that in the event of a failure we don't 		
	have to patch the database afterwards.   The data are always 
	saved in a file with the name of the grid*/
	filename = ((string)dir)+"/"+((string)name);
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
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = (n1)*(n2)*(n3);
	if(fwrite(x1[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(x2[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(x3[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(lat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(lon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(r[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	/* Now we write a row in the database for this grid.  Note
	some quantities have to be converted from radians to degrees.*/
	if(dbaddv(db,0,
		"gridname",name,
		"dimensions",dimensions,
		"lat",deg(lat0),
		"lon",deg(lon0),
		"radius",r0,
		"azimuth_y",deg(azimuth_y),
		"dx1nom",dx1_nom,
		"dx2nom",dx2_nom,
		"dx3nom",dx3_nom,
		"n1",n1,
		"n2",n2,
		"n3",n3,
		"xlow",x1low,
		"xhigh",x1high,
		"ylow",x2low,
		"yhigh",x2high,
		"zlow",x3low,
		"zhigh",x3high,
		"i0",i0,
		"j0",j0,
		"k0",k0,
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype","t8",
		"dir",dir,
		"dfile",name,
		"foff",foff,
		0) < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 3d grid into gclgdisk table\n");
		throw 1;
	}
}
/* Parallel routine for 2d*/
void GCLgrid::dbsave(Dbptr db, char *dir) throw(int)
{
	FILE *fp;
	string filename;
	int fssize;
	long int fpos;
	int foff;
	int gridsize;
	int dimensions=2;
	char *fwerr=(char *)"fwrite error on file %s";
	char cdef[2]="d",gdef[2]="d";

	db = dblookup(db,0,(char *)"gclgdisk",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		throw 1;
	}
	/*Save the data first so that in the event of a failure we don't 		have to patch the database afterwards. */
	filename = ((string)dir)+"/"+((string)name);
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
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = (n1)*(n2);
	if(fwrite(x1[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(x2[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(x3[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(lat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(lon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	if(fwrite(r[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	/* Now we write a row in the database for this grid*/
	if(dbaddv(db,0,
		"gridname",name,
		"dimensions",dimensions,
		"lat",deg(lat0),
		"lon",deg(lon0),
		"radius",r0,
		"azimuth_y",deg(azimuth_y),
		"dx1nom",dx1_nom,
		"dx2nom",dx2_nom,
		"n1",n1,
		"n2",n2,
		"xlow",x1low,
		"xhigh",x1high,
		"ylow",x2low,
		"yhigh",x2high,
		"zlow",x3low,
		"zhigh",x3high,
		"i0",i0,
		"j0",j0,
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype","t8",
		"dir",dir,
		"dfile",name,
		"foff",foff,
		0) < 0)
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
void GCLscalarfield::dbsave(Dbptr db, 
	char *gclgdir, 
	char *fielddir, 
	char *fieldname,
	char *dfile) throw(int)
{
	int gridsize;
	string filename;
	FILE *fp;
	long int fpos;
	int foff;
	int dimensions=2;
	int nv=1;

	//Save the parent GCLgrid when gclgdir is not NULL
	if(gclgdir!=NULL)
	{
		try {
			GCLgrid *g;
			g = dynamic_cast<GCLgrid *> (this);
			g->dbsave(db,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = ((string)fielddir)+"/"+((string)dfile);

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
	fpos = ftell(fp);
	foff = (int)fpos;
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
		(char *)"gridname",name,
		(char *)"dimensions",dimensions,
		(char *)"nv",nv,
		(char *)"dir",fielddir,
		(char *)"dfile",dfile,
		(char *)"foff",foff,
		(char *)"fieldname",fieldname,
		0)  < 0)
	{
		elog_notify(0,
		  (char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}

void GCLscalarfield3d::dbsave(Dbptr db, 
	char *gclgdir, 
	char *fielddir, 
	char *fieldname,
	char *dfile) throw(int)
{
	int gridsize;
	string filename;
	FILE *fp;
	long int fpos;
	int foff;
	int dimensions=3;
	int nv=1;

	//Save the parent GCLgrid when gclgdir is not NULL
	if(gclgdir!=NULL)
	{
		try {
			GCLgrid3d *g;
			g = dynamic_cast<GCLgrid3d *> (this);
			g->dbsave(db,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,
		 (char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = ((string)fielddir)+"/"+((string)dfile);

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
	fpos = ftell(fp);
	foff = (int)fpos;
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
		"gridname",name,
		"dimensions",dimensions,
		"nv",nv,
		"dir",fielddir,
		"dfile",dfile,
		"foff",foff,
		"fieldname",fieldname,
		0)  < 0)
	{
		elog_notify(0,
		  (char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}
void GCLvectorfield::dbsave(Dbptr db, 
	char *gclgdir, 
	char *fielddir, 
	char *fieldname,
	char *dfile) throw(int)
{
	int gridsize;
	string filename;
	FILE *fp;
	long int fpos;
	int foff;
	int dimensions=2;

	//Save the parent GCLgrid when gclgdir is not NULL
	if(gclgdir!=NULL)
	{
		try {
			GCLgrid *g;
			g = dynamic_cast<GCLgrid *> (this);
			g->dbsave(db,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,
		 (char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = ((string)gclgdir)+"/"+((string)dfile);

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
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = n1*n2*nv;	
	if(fwrite(val[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		(char *)"gridname",name,
		(char *)"dimensions",dimensions,
		(char *)"nv",nv,
		(char *)"dir",fielddir,
		(char *)"dfile",dfile,
		(char *)"foff",foff,
		(char *)"fieldname",fieldname,
		0)  < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}

void GCLvectorfield3d::dbsave(Dbptr db, 
	char *gclgdir, 
	char *fielddir, 
	char *fieldname,
	char *dfile) throw(int)
{
	int gridsize;
	string filename;
	FILE *fp;
	long int fpos;
	int foff;
	int dimensions=3;

	//Save the parent GCLgrid when gclgdir is not NULL
	if(gclgdir!=NULL)
	{
		try {
			GCLgrid3d *g;
			g = dynamic_cast<GCLgrid3d *> (this);
			g->dbsave(db,gclgdir);
		}
		catch(int dbserr)
		{
			throw(dbserr);
		}
	}
	db = dblookup(db,0,(char *)"gclfield",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,(char *)"lookup failed for gclfield table.  Extension table probably not defined\n");
		throw 1;
	}
	//First create a filename and save the val array with a binary write
	filename = ((string)gclgdir)+"/"+((string)dfile);

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
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = n1*n2*n3*nv;	
	if(fwrite(val[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,(char *)"fwrite error for file %s\n",
			filename.c_str());
		fclose(fp);
		throw 2;
	}
	fclose(fp);
	if(dbaddv(db,0,
		(char *)"gridname",name,
		(char *)"dimensions",dimensions,
		(char *)"nv",nv,
		(char *)"dir",fielddir,
		(char *)"dfile",dfile,
		(char *)"foff",foff,
		(char *)"fieldname",fieldname,
		0)  < 0)
	{
		elog_notify(0,(char *)"dbaddv error for 2d grid into gclfield table\n");
		throw 1;
	}
}
