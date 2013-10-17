#include "coords.h"
#include "HypocenterCSS30.h"
namespace SEISPP {

using namespace SEISPP;

HypocenterCSS30::HypocenterCSS30(Dbptr db,Hypocenter& h) : Hypocenter(h)
{
    long iret;
    iret=dbgetv(db,0,"orid",&orid,
            "evid",&evid,
            "jdate",&jdate,
            "nass",&nass,
            "ndef",&ndef,
            "ndp",&ndp,
            "grn",&grn,
            "srn",&srn,
            "commid",&commid,
            "mb",&mb,
            "ms",&ms,
            "ml",&ml,
            "mbid",&mbid,
            "msid",&msid,
            "mlid",&mlid,
            "etype",etype,
            "dtype",dtype,
            "review",review,
            "algorithm",algorithm,
            "auth",auth,NULL);
    if(iret==dbINVALID) 
        throw SeisppError(string("HypocenterCSS30 constructor:  dbgetv error."));
    dbthis=db;
}
HypocenterCSS30::HypocenterCSS30(const HypocenterCSS30& parent) 
    : Hypocenter(parent)
{
    orid=parent.orid;
    evid=parent.evid;
    jdate=parent.jdate;
    nass=parent.nass;
    ndef=parent.ndef;
    ndp=parent.ndp;
    grn=parent.grn;
    srn=parent.srn;
    commid=parent.commid;
    mb=parent.mb;
    ms=parent.ms;
    ml=parent.ml;
    mbid=parent.mbid;
    msid=parent.msid;
    mlid=parent.mlid;
    strcpy(etype,parent.etype);
    strcpy(dtype,parent.dtype);
    strcpy(review,parent.review);
    strcpy(algorithm,parent.algorithm);
    strcpy(auth,parent.auth);
    dbthis=parent.dbthis;
}
HypocenterCSS30& HypocenterCSS30::operator=(const HypocenterCSS30& parent)
{
    if(this!=&parent)
    {
        /* Tricky way to call operator= for base class */
        Hypocenter::operator=(parent);
        orid=parent.orid;
        evid=parent.evid;
        jdate=parent.jdate;
        nass=parent.nass;
        ndef=parent.ndef;
        ndp=parent.ndp;
        grn=parent.grn;
        srn=parent.srn;
        commid=parent.commid;
        mb=parent.mb;
        ms=parent.ms;
        ml=parent.ml;
        mbid=parent.mbid;
        msid=parent.msid;
        mlid=parent.mlid;
        strcpy(etype,parent.etype);
        strcpy(dtype,parent.dtype);
        strcpy(review,parent.review);
        strcpy(algorithm,parent.algorithm);
        strcpy(auth,parent.auth);
        dbthis=parent.dbthis;
    }
    return (*this);
}
long HypocenterCSS30::dbsave(Dbptr db)
{
    long iret;
    iret=dbaddv(db,0,"orid",orid,
            "evid",evid,
            "lat",deg(lat),
            "lon",deg(lon),
            "depth",z,
            "time",time,
            "jdate",jdate,
            "nass",nass,
            "ndef",ndef,
            "ndp",ndp,
            "grn",grn,
            "srn",srn,
            "commid",commid,
            "mb",mb,
            "ms",ms,
            "ml",ml,
            "mbid",mbid,
            "msid",msid,
            "mlid",mlid,
            "etype",etype,
            "dtype",dtype,
            "review",review,
            "algorithm",algorithm,
            "auth",auth,NULL);
    if(iret==dbINVALID) 
        throw SeisppError(string("HypocenterCSS30::dbsave:  dbaddv error."));
    else
        return(iret);
}
}

