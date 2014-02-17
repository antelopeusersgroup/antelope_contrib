#include <fstream>
#include <typeinfo>
#include "seispp.h"
#include "Metadata.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
/* Common code to load attributes to Metadata object for saving */
Metadata load_common_GCL_attributes(BasicGCLgrid *g)
{
    Metadata m;
    m.put("name",g->name);
    m.put("origin_latitude",deg(g->lat0));
    m.put("origin_longitude",deg(g->lon0));
    m.put("origin_radius",g->r0);
    m.put("azimuth_y",deg(g->azimuth_y));
    m.put("dx1_nom",g->dx1_nom);
    m.put("dx2_nom",g->dx2_nom);
    m.put("n1",g->n1);
    m.put("n2",g->n2);
    m.put("x1low",g->x1low);
    m.put("x1high",g->x1high);
    m.put("x2low",g->x2low);
    m.put("x2high",g->x2high);
    m.put("x3low",g->x3low);
    m.put("x3high",g->x3high);
    bool little_endian=IntelByteOrder();  //true if host is little
    /* byte_order and datatype are redundant, but use byte_order
       largely to make the pf file easier to comprehend*/
    if(little_endian)
    {
        m.put("byte_order","little_endian");
        m.put("datatype","u8");
    }
    else
    {
        m.put("byte_order","big_endian");
        m.put("datatype","t8");
    }
    string obname(typeid(*g).name());
    m.put("object_type",obname);
    return m;
}
/* In this library 3D grids have some extra attributes 
   that have to be added.  Thus each 3d save routine needs 
   to call this routine to append these attributes to
   the Metadata object. This procedure depends upon
   the implementation here that 3d objects are all
   children of a GCLgrid3d.*/
void load_3d_attributes(GCLgrid3d& g, Metadata& m)
{
    m.put("dx3_nom",g.dx3_nom);
    m.put("n3",g.n3);
    m.put("k0",g.k0);
}
/* A small helper to handle directory and file names correctly. 
This is used repeatedly here to combine a dir and fname field
WITHOUT the "ext" fields used for the default format  */
string makepath(string dir, string base)
{
    /* first step is to make sure the directory exists */
    if(makedir(const_cast<char *>(dir.c_str())))
        throw GCLgridError(string("makepath:  cannot create directory=")
                + dir);
    string result;
    /*Handle null dir correctly */
    if(dir.length()>0)
        result=dir+"/"+base;
    else
        result=base;
    return result;
}
void pfsave_attributes(Metadata& attributes,string base)
{
    string pffilename=base+".pf";
    try {
        ofstream outstrm;
        outstrm.open(pffilename.c_str(),ios::out);
        outstrm << attributes;
        outstrm.close();
    }catch(...){
        throw GCLgridError(string("pfsave_attributes failed on file")
                + pffilename);
    }
}
/* This painfully parallel set of helper procedures are used to 
   write the coordinate data in the default format of pfhdr.  
   There are several I could have reduced the redundant code, but
   took this path as I think it will simply be clearer.  Beware, however,
   that an error in one of these procedures nearly guarantees a parallel
   error in the other. */
void pfhdr_save_griddata(GCLgrid& g,string base)
{
    try {
        const string base_error("pfhdr format GCLgrid (2d)  writing routine:  ");
        string fullname=base+"."+dfileext;
        /* Intentionally open the output file in append mode.  Then check
           for file position and throw and error if foff is not zero. */
        FILE *fp=fopen(fullname.c_str(),"a");
        if(fp==NULL)
            throw GCLgridError(base_error + "fopen filed on file="
                    + fullname);
        if(ftell(fp)!=0)
        {
            fclose(fp);
            throw GCLgridError(base_error + "file "
                    + fullname 
                    + " exists\nChange file name or move this file away");
        }
        size_t npoints;
        npoints=g.n1*g.n2;
        if(fwrite((const void *)(&g.x1[0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x1 coordinates to file "
                    + fullname + "\nOutput file is incomplete");
        }
        if(fwrite((const void *)(&g.x2[0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x2 coordinates to file "
                    + fullname + "\nOutput file is incomplete");
        }
        if(fwrite((const void *)(&g.x3[0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x3 coordinates to file "
                    + fullname +"\nOutput file is incomplete");
        }
        fclose(fp);
    } catch(...){throw;};
}
/* 3D equivalent to above */
void pfhdr_save_griddata(GCLgrid3d& g,string base)
{
    try {
        const string base_error("pfhdr format GCLgrid3d writing routine:  ");
        string fullname=base+"."+dfileext;
        /* Intentionally open the output file in append mode.  Then check
           for file position and throw and error if foff is not zero. */
        FILE *fp=fopen(fullname.c_str(),"a");
        if(fp==NULL)
            throw GCLgridError(base_error + "fopen filed on file="
                    + fullname);
        if(ftell(fp)!=0)
        {
            fclose(fp);
            throw GCLgridError(base_error + "file "
                    + fullname 
                    + " exists\nChange file name or move this file away");
        }
        size_t npoints;
        npoints=g.n1*g.n2*g.n3;
        if(fwrite((const void *)(&g.x1[0][0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x1 coordinates to file "
                    + fullname+"\nOutput file is incomplete");
        }
        if(fwrite((const void *)(&g.x2[0][0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x2 coordinates to file "
                    + fullname+"\nOutput file is incomplete");
        }
        if(fwrite((const void *)(&g.x3[0][0][0]),sizeof(double),npoints,fp)
                != npoints)
        {
            fclose(fp);
            throw GCLgridError(base_error 
                    + "fwrite error while saving x3 coordinates to file "
                    + fullname+"\nOutput file is incomplete");
        }
        fclose(fp);
    } catch(...){throw;};
}
/* This is a generic writer for the field data of any GCLgrid based
   field object.  This works ONLY because the grids are constructed
   as contiguous memory blocks.  BE WARNED if that feature of this 
   library ever changes.  

  This procedure tries to opens a file fbase+".dat"
  If the file is empty it will throw an exception as it assumes the 
  grid data have already been stored.  Note this is NOT a bombproof
  method to handle this, but it should be workable here since in 
  all cases this procedure is called immediately after writing the 
  grid data.  This will only be a problem if this procedure is 
  recycled in a different context.  It then writes n double vavlues
  that is presumes are in a contiguous block of memory referened by
  the pointer d.
 */
void pfhdr_save_field_data(string fbase, double *d, size_t n)
{
    const string base_error("pfhdr_save_field_data procedure:  ");
    string fullname=fbase+"."+dfileext;
    FILE *fp=fopen(fullname.c_str(),"a");
    if(fp==NULL)
        throw GCLgridError(base_error+"fopen failed on file="
                + fullname);
    long current_length=ftell(fp);
    if(current_length<=0) 
    {
        fclose(fp);
        throw GCLgridError(base_error + "file="
                + fullname
                + "contains no data.\nLikely unhandled write error or coding error.");
    }
    if(fwrite((void *)d,sizeof(double),n,fp)!=n)
    {
        fclose(fp);
        throw GCLgridError(base_error + "fwrite error while writing field data to file="
                + fullname);
    }
    fclose(fp);
}

void GCLgrid::save(string fname, string dir,string format)
{
    const string base_error("GCLgrid::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        string fbase=makepath(dir,fname);
        if(format==default_output_format)
        {
            pfsave_attributes(attributes,fbase);
            pfhdr_save_griddata(*this,fbase);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);

    }catch(...){throw;};
}
void GCLgrid3d::save(string fname, string dir,string format)
{
    const string base_error("GCLgrid3d::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        load_3d_attributes(*this,attributes);
        string fbase=makepath(dir,fname);
        if(format==default_output_format)
        {
            pfsave_attributes(attributes,fbase);
            pfhdr_save_griddata(*this,fbase);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);
    }catch(...){throw;};
}
void GCLscalarfield::save(string fname, string dir,string format)
{
    const string base_error("GCLscalarfield::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        GCLgrid *g=dynamic_cast<GCLgrid*>(this);
        g->save(fname,dir,format);
        /* To allow using common code to save the grid we have to 
           segregate writing the field data.  Currently only support
           one format so this issue does not come up as a restriction.
           Beware if this is extended as this may have to be 
           reorganized. */
        if(format==default_output_format)
        {
            string fbase=makepath(dir,fname);
            size_t npts=n1*n2;
            pfhdr_save_field_data(fbase,&(val[0][0]),npts);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);
    }catch(...){throw;};
}
void GCLvectorfield::save(string fname, string dir,string format)
{
    const string base_error("GCLvectorfield::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        attributes.put("nv",nv);
        /* This duplicates code in save method for grid*/
        string fbase=makepath(dir,fname);
        if(format==default_output_format)
        {
            pfsave_attributes(attributes,fbase);
            pfhdr_save_griddata(*this,fbase);
        /* To allow using common code to save the grid we have to 
           segregate writing the field data.  Currently only support
           one format so this issue does not come up as a restriction.
           Beware if this is extended as this may have to be 
           reorganized. */
            string fbase=makepath(dir,fname);
            size_t npts=n1*n2*nv;
            pfhdr_save_field_data(fbase,&(val[0][0][0]),npts);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);
    }catch(...){throw;};
}
void GCLscalarfield3d::save(string fname, string dir,string format)
{
    const string base_error("GCLscalarfield3d::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        GCLgrid3d *g=dynamic_cast<GCLgrid3d*>(this);
        g->save(fname,dir,format);
        if(format==default_output_format)
        {
            string fbase=makepath(dir,fname);
            size_t npts=n1*n2*n3;
            pfhdr_save_field_data(fbase,&(val[0][0][0]),npts);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);
    }catch(...){throw;};
}
void GCLvectorfield3d::save(string fname, string dir,string format)
{
    const string base_error("GCLvectorfield3d::save:  ");
    try{
        Metadata attributes=load_common_GCL_attributes(this);
        load_3d_attributes(*this,attributes);
        attributes.put("nv",nv);
        string fbase=makepath(dir,fname);
        if(format==default_output_format)
        {
            pfsave_attributes(attributes,fbase);
            pfhdr_save_griddata(*this,fbase);
            size_t npts=n1*n2*n3*nv;
            pfhdr_save_field_data(fbase,&(val[0][0][0][0]),npts);
        }
        else
            throw GCLgridError(base_error
                    + "Do know how to write data with format tag "
                    + format);
    }catch(...){throw;};
}
