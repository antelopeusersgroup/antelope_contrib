#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "gclgrid.h"
/* Loads a 3d GCLgrid object using an extension table and datascope.
The table contains only rows to locate a file which is then read in.

Arguments:   
	db - database to access to look up this grid file under
	gridnmae - name of requested grid object  (key of the table)

Normally returns a pointer to the GCL3Dgrid object.  Returns a NULL
pointer if errors occur and leaves messages in the error log.
Note this will leave a memory leak if this is called excessively
with repeated failures in the read block.  This function is 
sloppy about recovery from these errors under an assumption the
caller should die in this condition.
*/
GCL3Dgrid *GCL3Dgrid_load_db(Dbptr db,char *gridname)
{
	Dbptr dbgrd;
	char sstring[40];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	int foff;
	char *base_message="Cannot create GCL3Dgrid object: ";
	char *read_error="fread failed";
	GCL3Dgrid *g;
	int nrec;
	char cdef[2],gdef[2];
	int retcode;
	int gridsize;  
	FILE *fp;

	dbgrd = dblookup(db,0,"gclgdisk",0,0); 
	if(dbgrd.record == dbINVALID) 
	{
		elog_notify(0,"%s gclgdisk table not defined in schema definition\n",base_message);
		return(NULL);
	}
	sprintf(sstring,"gridname =~ /%s/ && dimensions == 3",gridname);
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,"%s grid with name %s not found in database\n",
			base_message,gridname);
		return(NULL);
	}
	dbgrd.record = 0;
	allot(GCL3Dgrid *,g,1);
	strcpy(g->name,gridname);
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
	if(dbgetv(dbgrd,0,
		"lat",&(g->lat0),
		"lon",&(g->lon0),
		"radius",&(g->r0),
		"azimuth_y",&(g->azimuth_y),
		"dx1nom",&(g->dx1_nom),
		"dx2nom",&(g->dx2_nom),
		"dx3nom",&(g->dx3_nom),
		"n1",&(g->n1),
		"n2",&(g->n2),
		"n3",&(g->n3),
		"xlow",&(g->xlow),
		"xhigh",&(g->xhigh),
		"ylow",&(g->ylow),
		"yhigh",&(g->yhigh),
		"zlow",&(g->zlow),
		"zhigh",&(g->zhigh),
		"i0",&(g->i0),
		"j0",&(g->j0),
		"k0",&(g->k0),
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype",datatype,
		"dir",dir,
		"dfile",dfile,
		"foff",&foff,
		0) == dbINVALID)
	{
		free(g);
		elog_notify(0,"%s dbgetv error reading gclgdisk table\n",
			base_message);
		return(NULL);
	}
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	g->lat0 = rad(g->lat0);
	g->lon0 = rad(g->lon0);
	g->azimuth_y = rad(g->azimuth_y);
	if(!strcmp(cdef,"n") || !strcmp(gdef,"n") )
	{
		free(g);
		elog_notify(0,"%s Cartesian and Geographical mapping (cdefined and geodefined attributes) must both be defined for input\n",
			base_message);
		return(NULL);
	}
	else
	{
		g->cartesian_defined=1;
		g->geographic_defined=1;
	}
	if(strcmp(datatype,"t8"))
	{
		free(g);
		elog_notify(0,"%s data type %s not allowed.  Currently only support t8\n",
			base_message, datatype);
		return(NULL);
	}
	/* Get the file name to read the gclgrid data from.*/
	if(dbextfile(dbgrd,"gclgdisk",filename) <=0)
	{
		free(g);
		elog_notify(0,"%s Cannot find grid file named %s in directory %s\n",
			base_message,dfile,dir);
		return(NULL);
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		free(g);
		elog_notify(0,"%s file %s cannot be openned for read\n",
			base_message,filename);
		return(NULL);
	}
	fseek(fp,foff,SEEK_SET);
	/* We alloc all memory first before reading so we can call a 
	free routine in case any reads fail */
	g->x1 = create_3dgrid_contiguous(g->n1,g->n2,g->n3);
	g->x2 = create_3dgrid_contiguous(g->n1,g->n2,g->n3); 
	g->x3 = create_3dgrid_contiguous(g->n1,g->n2,g->n3); 
	g->lat = create_3dgrid_contiguous(g->n1,g->n2,g->n3);
	g->lon = create_3dgrid_contiguous(g->n1,g->n2,g->n3); 
	g->r = create_3dgrid_contiguous(g->n1,g->n2,g->n3); 

	/* read data trapping read errors */
	gridsize = (g->n1)*(g->n2)*(g->n3);
	if(fread(g->x1[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x1\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->x2[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x2\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->x3[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x3\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->lat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading latitude\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->lon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading longitude\n",
			base_message,read_error);
		
	return(NULL);
	}

	if(fread(g->r[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading radius\n",
			base_message,read_error);
		return(NULL);
	}
	return(g);
}
/* close companion to the above for 2d grid */

GCL2Dgrid *GCL2Dgrid_load_db(Dbptr db,char *gridname)
{
	Dbptr dbgrd;
	char sstring[40];
	char datatype[4];
	char dir[65], dfile[36];
	char filename[512];  
	int foff;
	char *base_message="Cannot create GCL2Dgrid object: ";
	char *read_error="fread failed";
	GCL2Dgrid *g;
	int nrec;
	char cdef[2],gdef[2];
	int retcode;
	int gridsize;  
	FILE *fp;

	dbgrd = dblookup(db,0,"gclgdisk",0,0); 
	if(dbgrd.record == dbINVALID) 
	{
		elog_notify(0,"%s gclgdisk table not defined in schema definition\n",base_message);
		return(NULL);
	}
	sprintf(sstring,"gridname =~ /%s/ && dimensions == 2",gridname);
	dbgrd = dbsubset(dbgrd,sstring,0);
	dbquery(dbgrd,dbRECORD_COUNT,&nrec);
	if(nrec <= 0) 
	{
		elog_notify(0,"%s grid with name %s not found in database\n",
			base_message,gridname);
		return(NULL);
	}
	dbgrd.record = 0;
	allot(GCL2Dgrid *,g,1);
	strcpy(g->name,gridname);
	/* We intentionally ignore the dimension field because the subset
	should have assured we match the right record */
	if(dbgetv(dbgrd,0,
		"lat",&(g->lat0),
		"lon",&(g->lon0),
		"radius",&(g->r0),
		"azimuth_y",&(g->azimuth_y),
		"dx1nom",&(g->dx1_nom),
		"dx2nom",&(g->dx2_nom),
		"n1",&(g->n1),
		"n2",&(g->n2),
		"xlow",&(g->xlow),
		"xhigh",&(g->xhigh),
		"ylow",&(g->ylow),
		"yhigh",&(g->yhigh),
		"zlow",&(g->zlow),
		"zhigh",&(g->zhigh),
		"i0",&(g->i0),
		"j0",&(g->j0),
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype",datatype,
		"dir",dir,
		"dfile",dfile,
		"foff",&foff,
		0) == dbINVALID)
	{
		free(g);
		elog_notify(0,"%s dbgetv error reading gclgdisk table\n",
			base_message);
		return(NULL);
	}
	/* These parameters are stored in the database in degrees but
	are converted to radians for internal use */
	g->lat0 = rad(g->lat0);
	g->lon0 = rad(g->lon0);
	g->azimuth_y = rad(g->azimuth_y);

	if(!strcmp(cdef,"n") || !strcmp(gdef,"n") )
	{
		free(g);
		elog_notify(0,"%s Cartesian and Geographical mapping (cdefined and geodefined attributes) must both be defined for input\n",
			base_message);
		return(NULL);
	}
	else
	{
		g->cartesian_defined=1;
		g->geographic_defined=1;
	}
	if(strcmp(datatype,"t8"))
	{
		free(g);
		elog_notify(0,"%s data type %s not allowed.  Currently only support t8\n",
			base_message, datatype);
		return(NULL);
	}
	/* Get the file name to read the gclgrid data from.*/
	if(dbextfile(dbgrd,"gclgdisk",filename) <=0)
	{
		free(g);
		elog_notify(0,"%s Cannot find grid file named %s in directory %s\n",
			base_message,dfile,dir);
		return(NULL);
	}
	fp = fopen(filename,"r");
	if(fp == NULL)
	{
		free(g);
		elog_notify(0,"%s file %s cannot be openned for read\n",
			base_message,filename);
		return(NULL);
	}
	fseek(fp,foff,SEEK_SET);
	/* We alloc all memory first before reading so we can call a 
	free routine in case any reads fail */
	g->x1 = create_2dgrid_contiguous(g->n1,g->n2);
	g->x2 = create_2dgrid_contiguous(g->n1,g->n2); 
	g->x3 = create_2dgrid_contiguous(g->n1,g->n2); 
	g->lat = create_2dgrid_contiguous(g->n1,g->n2);
	g->lon = create_2dgrid_contiguous(g->n1,g->n2); 
	g->r = create_2dgrid_contiguous(g->n1,g->n2); 

	/* read data trapping read errors */
	gridsize = (g->n1)*(g->n2);
	if(fread(g->x1[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x1\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->x2[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x2\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->x3[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading x3\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->lat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading latitude\n",
			base_message,read_error);
		return(NULL);
	}
	if(fread(g->lon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading longitude\n",
			base_message,read_error);
		return(NULL);
	}

	if(fread(g->r[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,"%s %s reading radius\n",
			base_message,read_error);
		return(NULL);
	}
	return(g);
}

/* The following two parallel functions are the inverse of the load
routines above.  That is, they take a db pointer and a pointer to 
a grid object and save the contents to external files and a database.
dir is the directory where the results are stored.  The file name
is always created from the gridname.  

Errors of any kind leave a warning message on the error log and result
in a nonzero return count of number of errors.  Normal return is zero.

Author:  G Pavlis
Date:  August 2000
*/
int save_3dgclgrid(GCL3Dgrid *g,Dbptr db,char *dir)
{
	FILE *fp;
	char *filename;
	int fssize;
	int errcount=0;
	long int fpos;
	int foff;
	int gridsize;
	int dimensions=3;
	char *fwerr="fwrite error on file %s";
	char cdef[2]="d",gdef[2]="d";

	db = dblookup(db,0,"gclgdisk",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		return(-1);
	}
	/*Save the data first so that in the event of a failure we don't 		have to patch the database afterwards. */
	fssize = strlen(dir) + strlen(g->name) + 1;
	allot(char *,filename,fssize);
	
	fp = fopen(filename,"a+");
	if(fp==NULL)
	{
		elog_notify(0,"Cannot open output file %s\nNothing save\n",
			filename);
		return(-1);
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on Solaris where they are switching from
	32 to 64.   This is safe if overkill*/
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = (g->n1)*(g->n2)*(g->n3);
	if(fwrite(g->x1[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->x2[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->x3[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->lat[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->lon[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->r[0][0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	fclose(fp);
	/* Now we write a row in the database for this grid.  Note
	some quantities have to be converted from radians to degrees.*/
	if(dbaddv(db,0,
		"gridname",g->name,
		"dimensions",dimensions,
		"lat",deg(g->lat0),
		"lon",deg(g->lon0),
		"radius",g->r0,
		"azimuth_y",deg(g->azimuth_y),
		"dx1nom",g->dx1_nom,
		"dx2nom",g->dx2_nom,
		"dx3nom",g->dx3_nom,
		"n1",g->n1,
		"n2",g->n2,
		"n3",g->n3,
		"xlow",g->xlow,
		"xhigh",g->xhigh,
		"ylow",g->ylow,
		"yhigh",g->yhigh,
		"zlow",g->zlow,
		"zhigh",g->zhigh,
		"i0",g->i0,
		"j0",g->j0,
		"k0",g->k0,
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype","t8",
		"dir",dir,
		"dfile",g->name,
		"foff",foff,
		0) < 0)
	{
		elog_notify(0,"dbaddv error for 3d grid into gclgdisk table\n");
		return(-1);
	}
	free(filename);
	return(errcount);
}
/* Parallel routine for 2d*/
int save_2dgclgrid(GCL2Dgrid *g,Dbptr db,char *dir)
{
	FILE *fp;
	char *filename;
	int fssize;
	int errcount=0;
	long int fpos;
	int foff;
	int gridsize;
	int dimensions=2;
	char *fwerr="fwrite error on file %s";
	char cdef[2]="d",gdef[2]="d";

	db = dblookup(db,0,"gclgdisk",0,0);
	if(db.record == dbINVALID)
	{
		elog_notify(0,"lookup failed for gclgdisk table.  Extension table probably not defined\n");
		return(-1);
	}
	/*Save the data first so that in the event of a failure we don't 		have to patch the database afterwards. */
	fssize = strlen(dir) + strlen(g->name) + 1;
	allot(char *,filename,fssize);
	
	strcpy(filename,dir);
	strcat(filename,"/");
	strcat(filename,g->name);
	fp = fopen(filename,"a+");
	if(fp==NULL)
	{
		elog_notify(0,"Cannot open output file %s\nNothing save\n",
			filename);
		return(-1);
	}
	fseek(fp,0,SEEK_END);
	/* The use of the int cast is unnecessary on some machines,
	but may be problematic on current versions of solaris so I'll
	be safe */
	fpos = ftell(fp);
	foff = (int)fpos;
	gridsize = (g->n1)*(g->n2);
	if(fwrite(g->x1[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->x2[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->x3[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->lat[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->lon[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	if(fwrite(g->r[0],sizeof(double),gridsize,fp) != gridsize)
	{
		elog_notify(0,fwerr,filename);
		++errcount;
	}
	fclose(fp);
	/* Now we write a row in the database for this grid*/
	if(dbaddv(db,0,
		"gridname",g->name,
		"dimensions",dimensions,
		"lat",deg(g->lat0),
		"lon",deg(g->lon0),
		"radius",g->r0,
		"azimuth_y",deg(g->azimuth_y),
		"dx1nom",g->dx1_nom,
		"dx2nom",g->dx2_nom,
		"n1",g->n1,
		"n2",g->n2,
		"xlow",g->xlow,
		"xhigh",g->xhigh,
		"ylow",g->ylow,
		"yhigh",g->yhigh,
		"zlow",g->zlow,
		"zhigh",g->zhigh,
		"i0",g->i0,
		"j0",g->j0,
		"cdefined",cdef,
		"geodefined",gdef,
		"datatype","t8",
		"dir",dir,
		"dfile",g->name,
		"foff",foff,
		0) < 0)
	{
		elog_notify(0,"dbaddv error for 2d grid into gclgdisk table\n");
		return(-1);
	}
	free(filename);
	return(errcount);
}
