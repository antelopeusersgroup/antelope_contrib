#include <fstream>
#include "perf.h"
#include "seispp.h"
#include "slowness.h"
#include "HFArray.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
HFArray::HFArray(string fname,bool geocoords)
{
    string base_error("HFArray text file constructor:  ");;
    FILE *fp;
    fp=fopen(fname.c_str(),"r");
    if(fp==NULL)
        throw 
            SeisppError(base_error + "Open failed for input file "
                    + fname);
    char tag[32];
    double olat,olon,oelev;
    fscanf(fp,"%s%lf%lf%lf",tag,&olon,&olat,&oelev);
    if(string(tag)!="ORIGIN")
        throw SeisppError(base_error + "format error\n"
                + "First line must contain tag ORIGIN followed by lon,lat,elev");
    olat=rad(olat);
    olon=rad(olon);
    double oradius=r0_ellipse(olat)+oelev;
    /* The RegionalCoordinates object has the concept of allowing
       a rotation of the coordinates around the vertical axis 
       by a specified angle.   Her we always set that zero. */
    coords=RegionalCoordinates(olat,olon,oradius,0.0);
    vector<double> xs;
    /* We do this to allow setting components */
    int i;
    for(i=0;i<3;++i) 
        xs.push_back(0.0);
    string sta;
    char csta[20];
    double x,y,z;
    while(fscanf(fp,"%s%lf%lf%lf",csta,&x,&y,&z)==4)
    {
        sta=string(csta);
        if(geocoords)
        {
            /* When true we assume the order is lon,lat,elev(km) */
            double r=r0_ellipse(rad(y))+z;
            Cartesian_point cp=this->coords.cartesian(rad(y),rad(x),r);
            xs[0]=cp.x1;
            xs[1]=cp.x2;
            xs[2]=cp.x3;
        }
        else
        {
            xs[0]=x;
            xs[1]=y;
            xs[2]=z;
        }
        stations.insert(pair<string,vector<double> >(sta,xs));
    }
}
HFArray::HFArray(DatascopeHandle& dbh,string net)
{
    vector<double> xs;
    /* We do this to allow setting components */
    int i;
    for(i=0;i<3;++i) 
        xs.push_back(0.0);
    try{
        dbh.lookup("site");
        if(net.length()>0)
        {
            dbh.natural_join("snetsta");
            string sstr("snet=~/");
            sstr=sstr+net+"/";
            dbh.subset(sstr);
        }
        dbh.rewind();
        int n=dbh.number_tuples();
        for(i=0;i<n;++i,++dbh)
        {
            string sta,refsta;
            sta=dbh.get_string("sta");
            refsta=dbh.get_string("refsta");
            xs[0]=dbh.get_double("deast");
            xs[1]=dbh.get_double("dnorth");
            xs[2]=dbh.get_double("elev");
            stations.insert(pair<string,vector<double> >(sta,xs));
            if(sta==refsta)
            {
                double olat,olon;
                olat=dbh.get_double("lat");
                olon=dbh.get_double("lon");
                olat=rad(olat);
                olon=rad(olon);
                /* not using this for now - force
                   reference elevation to sea level
                double oelev;
                oelev=dbh.get_double("elev");
                double oradius=r0_ellipse(olat)+oelev;
                */
                coords=RegionalCoordinates(olat,olon,r0_ellipse(olat),0.0);
            }
        }
    }catch(...){throw;};
}



vector<double> HFArray::x(string sta)
{
    vector<double> result;
    result.reserve(3);
    map<string,vector<double> >::iterator it;
    it=stations.find(sta);
    if(it==stations.end()) 
        throw SeisppError("HFArray x method:  "
                + sta + " location not defined");
    int i;
    for(i=0;i<3;++i)
        result.push_back((*it).second[i]);
    return result;
}
double HFArray::moveout(string sta, SlownessVector u,double v0)
{
    try{
        vector<double> xr=this->x(sta);
        double ul2n=u.mag();
        double uz;
        double u0=1.0/v0;
        /* Set uz zero if the mode is evanescent at this surface velocity */
        if(u0<ul2n)
            uz=0;
        else
            uz=sqrt(u0*u0-ul2n*ul2n);
        double ut[3];
        ut[0]=u.ux;
        ut[1]=u.uy;
        ut[2]=uz;
        double mo=ddot(3,ut,1,&(xr[0]),1);
        return(mo);
    }catch(...){throw;};
}
Geographic_point HFArray::origin()
{
    return(coords.origin());
}
Geographic_point HFArray::geographic_location(string sta)
{
    try{
        vector<double> xr=this->x(sta);
        return(coords.geographic(&(xr[0])));
    }catch(...){throw;};
}

HFArray::HFArray(const HFArray& parent)
    : coords(parent.coords)
{
    stations=parent.stations;
}
HFArray& HFArray::operator=(const HFArray& parent)
{
    if(this!=&parent)
    {
        coords=parent.coords;
        stations=parent.stations;
    }
    return(*this);
}

