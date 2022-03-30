#include <stdio.h>
/*#include "su.h"*/
#include "par.h"
/* This is from su.h.  su.h generated a lot of errors so I pulled this
   in selectively*/
typedef union { /* storage for arbitrary type */
	char s[8];
	short h;
	unsigned short u;
	long l;
	unsigned long v;
	int i;
	unsigned int p;
	float f;
	double d;
	unsigned int U:16;
	unsigned int P:32;
} Value;
/* From newer version of su.h */
typedef char *cwp_String;
#include "segy.h"
#include "TimeSeries.h"
using namespace SEISPP;
TimeSeries ReadSegyTrace(FILE *fp,bool load_coordinates)
{
    const string base_error("ReadSegyTrace:  ");
    segy tr;
    int iret;
    /*This is the base SU routine to read an SU trace C struct.
      It returns a nonzero value when successful and 0 if the read
      fails.  This is a bit obnoxious since it does not distinguish
      between a fail and an eof.   We handle this in a less than
      perfect way be returning an empty TimeSeries object if the 
      read failed. */
    iret=fgettr(fp,&tr);
    if(iret==0) return TimeSeries();
    TimeSeries d;
    d.s.reserve(tr.ns);
    /* We extract only a fixed set of header values.   This is not
       general, but the first attempt at this using a HeaderMap 
       object was more hassle than it was worth.  Instead this
       just hard codes what is extracted.   Hack this if 
       the code needs to be adapted to another project.  
       
       First the required elements for a TimeSeries object. */
    try{
        d.ns=tr.ns;
        d.dt = ((double)tr.dt)*1.0e-6;
        if(tr.trid==2)
            d.live=false;
        else
            d.live=true;
        d.ns=tr.ns;
        d.dt = ((double)tr.dt)*1.0e-6;
        if(tr.trid==2)
            d.live=false;
        else
            d.live=true;
        d.t0=0.0;   // always zero for shot data which is assumed here
        d.tref=relative;
        /* Now we copy a few things to the Metadata header */
        d.put("tracl",tr.tracl);
        d.put("tracr",tr.tracr);
        d.put("fldr",tr.fldr);
        d.put("tracf",tr.tracf);
        // alias for downstream
        char sbuf[10];
        sprintf(sbuf,"%d",tr.tracf);
        d.put("chan",sbuf);
        d.put("ep",tr.ep);
        // another useful alias
        d.put("evid",tr.ep);
        d.put("nvs",tr.nvs);
        if(load_coordinates)
        {
            /* These are coordinates.  We convert them to 
               real numbers here from su where they are stored
               as int.  su borrows scalco from segy to add precision
               which we retain here. */
            double rx,ry,sx,sy;
            double scale=(double)(tr.scalco);
            rx=(double)(tr.gx);
            rx/=scale;
            d.put("rx",rx); // note conversion of gx,gy to rx,ry
            ry=(double)(tr.gy);
            ry/=scale;
            d.put("ry",ry);
            sx=(double)(tr.sx);
            sx/=scale;
            d.put("sx",sx);
            sy=(double)(tr.sy);
            sy/=scale;
            d.put("sy",sy);
            d.put("relev",(double)tr.gelev);
            /* To mesh with ParticleMotionVTKconverter we have to store the
               sensor elevation in units of km under the site.elev tag.*/
            double elev=(double)tr.gelev;
            elev /= 1000.0;
            d.put("site.elev",elev);
            d.put("selev",(double)tr.selev);
            /* We save offset in the the original segy int form and compute
               it form the other coordinates.   The later can be more 
               accurate when scalco is greater than one.*/
            double offset;
            elev=(double)(tr.gelev-tr.selev);
            offset=sqrt((rx-sx)*(rx-sx) + (ry-sy)*(ry-sy) + elev*elev);
            d.put("offset_from_segy",(double)tr.offset);
            d.put("offset",offset);
        }
        /* Double these in metadata */
        d.put("nsamp",(int)tr.ns);
        d.put("int_dt",(int)tr.dt);
        d.put("dt",d.dt);
        d.put("samprate",1.0/(d.dt));
        d.put("time",d.t0);
    }catch(SeisppError& serr)
    {
        cerr << base_error <<"Error parsing header data."<<endl
            << "Header parsing loop threw this message:"<<endl;
        serr.log_error();
        cerr<<"Fatal Error: aborting"<<endl;
        exit(-1);
    }
    /* Now load the sample data - requires a float to double
       conversion */
    int i;
    for(i=0;i<tr.ns;++i) 
        d.s.push_back((double)tr.data[i]);
    return d;
}


