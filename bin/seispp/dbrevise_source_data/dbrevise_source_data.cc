#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "Hypocenter.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "dbrevise_source_data db < in > out [-tol xx -v --help -text -pf pffile]"
        <<endl
        << "Updates source data in an input ThreeComponentEnsemble "<<endl
        << "with revised source coordinates defined in Datascope database db"
        <<endl
        << "Input db is assumed be event->origin subsetted to orid==prefor"
        <<endl
        << "Normally assumes the input data have source coordinates defined in ensemble"
        << " header (Metadata)"<<endl
        << "Keys for source coordinate data are defined in pf file"<<endl
        << "pf allows an option to load the source coordinates from "
        << "first ensemble member"<<endl
        <<  "-tol - set space-time tolerance for match (units of km)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}

/*! \brief Extended Hypocenter object containing all attributes in CSS3.0
  origin table.

  Sometimes it is useful to have not just the core Hypocenter object concept
  but an extended version containing all the attributes of the css3.0 
  origin table.  This object is effectively an abstraction of one row
  of an origin table. 

  This code is a copy from two files that are part of the source code
  for the catalog_cleanup (antelope contrib) program.  
  */

class HypocenterCSS30 : public Hypocenter
{
public:
    /*! See Antelope CSS3.0 documentation for defintion of these attributes.*/
    long orid,evid,jdate;
    long nass,ndef,ndp;
    long grn, srn;
    char etype[3];
    char dtype[2];
    char review[5];
    char algorithm[16];
    char auth[16];
    long commid;
    double mb,ms,ml;
    long mbid,msid,mlid;
    Dbptr dbthis;  /* Holds Dbptr from which this was generated */
    HypocenterCSS30(Dbptr db,Hypocenter& h);
    HypocenterCSS30(const HypocenterCSS30& parent);
    HypocenterCSS30& operator=(const HypocenterCSS30& parent);
    long dbsave(Dbptr db);
};

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
Hypocenter load_hypo(Dbptr db)
{
	string method("tttaup");
	string model("iasp91");
	double lat,lon,depth,otime;
	dbgetv(db,0,"lat",&lat,
		"lon",&lon,
		"depth",&depth,
		"time",&otime,NULL);
	return(Hypocenter(rad(lat),rad(lon),depth,otime,method,model));
}

double space_time_difference(Hypocenter& h1, Hypocenter& h2)
{
	const double velocity(6.0);
	const double Re(6371.0);
	double dist=h1.distance(h2.lat,h2.lon);
	dist*=Re;
	double sqrdist=dist*dist;
	double dz,dt;
	dz=fabs(h1.z-h2.z);
	dt=fabs(h1.time-h2.time);
	dt*=velocity;
	sqrdist += dz*dz + dt*dt;
	return(sqrt(sqrdist));
}


HypocenterCSS30 find_new_hypo(list<HypocenterCSS30>& eventlist,
    Hypocenter oldhypo,double tolerance)
{
  try{
    list<HypocenterCSS30>::iterator eptr;
    for(eptr=eventlist.begin();eptr!=eventlist.end();++eptr)
    {
      double dr=space_time_difference(*eptr,oldhypo);
      if(dr<tolerance)
        return (*eptr);
    }
    throw SeisppError(string("find_new_hypo: ")
       + " no match was found for event with origin time="
        + strtime(oldhypo.time));
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc<=1) usage();
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string dbname(argv[1]);
    string pffile("dbrevise_source_data.pf");
    double tolerance(0.0);;
    bool need_tolerance(true);
    bool binary_data(true);

    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-tol")
        {
            ++i;
            if(i>=argc)usage();
            tolerance=atof(argv[i]);
            need_tolerance=false;
        }
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    Pf *pf;
    char *pfstr=strdup(pffile.c_str());

    if(pfread(pfstr,&pf))
    {
      cerr << "pfread failed on parameter file="<<pffile<<endl;
      usage();
    }
    try{
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        Metadata control(pf);
        /* If true, source coordinates are loaded from first member
           seismogram - otherwise fetch from ensemble metadata*/
        bool load_from_members=control.get_bool("load_from_members");
        /* These are the keys used to fetch hypocenter data from 
           ThreeComponentEnsemble or members */
        string latkey=control.get_string("latitude_key");
        string lonkey=control.get_string("longitude_key");
        string depthkey=control.get_string("source_depth_key");
        string otimekey=control.get_string("origin_time_key");
        /* load the tolerance from pf it was not defined by the arg list*/
        if(need_tolerance) tolerance=control.get_double("space_time_tolerance");
        /* We now load the entire catalog defined in db.   This is a large
           memory algorithm and a very dumb algorithm for a large catalog
           because of the linear search used to match hypocenters.   
           This could be a procedure, but done inline here to avoid 
           need to pass the potentially large object back as a pointer*/
        DatascopeHandle dbh(dbname,true);
        dbh.lookup("event");
        dbh.subset(string("orid==prefor"));
        dbh.natural_join("origin");
        list<HypocenterCSS30> eventlist;
        long nrows=dbh.number_tuples();
        if(SEISPP_verbose) 
          cerr << "dbrevise_source_data:  loading "<<nrows
            << " hypocenters from input database "<<dbname<<endl;
        dbh.rewind();
        for(long n=0;n<nrows;++dbh,++n)
        {
          Hypocenter h(load_hypo(dbh.db));
          HypocenterCSS30 hc3(dbh.db,h);
          eventlist.push_back(hc3);
        }
        ThreeComponentEnsemble d;
        int ensemble_number(0);
        if(SEISPP_verbose)
          cerr << "Old hypocenter data (lat,lon,depth,otime) - new hypocenter"
            <<endl;
        while(inp->good())
        {
            d=inp->read();
            /* Probably unnecessary paranoia */
            if(d.member.size()<=0) continue;
            /* Trick to simplify code for loading data hypocenter */
            Metadata dmd;
            if(load_from_members)
              dmd=Metadata(dynamic_cast<Metadata&>(d.member[0]));
            else
              dmd=Metadata(dynamic_cast<Metadata&>(d));
            double slat,slon,sdepth,otime;
            try {
              slat=dmd.get<double>(latkey);
              slat=rad(slat);
              slon=dmd.get<double>(lonkey);
              slon=rad(slon);
              sdepth=dmd.get<double>(depthkey);
              sdepth=rad(sdepth);
              otime=dmd.get<double>(otimekey);
              Hypocenter dhypo(slat,slon,sdepth,otime,string("tttaup"),
                  string("iasp91"));  // frozen method and model - baggage here;
              /* This will throw an exception if no match is found for dhypo.
                 That was done assuming that should not happen in the 
                 context of the intent of this program */
              HypocenterCSS30 new_hypo(find_new_hypo(eventlist,
                    dhypo,tolerance));
              d.put(latkey,deg(new_hypo.lat));
              d.put(lonkey,deg(new_hypo.lon));
              d.put(depthkey,new_hypo.z);
              d.put(otimekey,new_hypo.time);
              /* For now these are frozen */
              d.put("evid",new_hypo.evid);
              d.put("orid",new_hypo.orid);
              if(SEISPP_verbose)
              {
                cerr << deg(dhypo.lat)<<" "
                  << deg(dhypo.lon)<<" "
                  << dhypo.z<<" "
                  << strtime(dhypo.time)<<" ";
                cerr << deg(new_hypo.lat)<<" "
                  << deg(new_hypo.lon)<<" "
                  << new_hypo.z<<" "
                  << strtime(new_hypo.time)<<endl;
              }
            }catch(SeisppError& serr)
            {
              cerr << "dbrevise_source_data:  failure to load hypocenter data"
                <<" for ensemble number "<<ensemble_number<<endl
                <<"Error message thrown in processsing:"<<endl;
              serr.log_error();
              cerr  << "Hypocenter data for this ensemble was not changed"
                <<endl;
            }
            out->write(d);
            ++ensemble_number;
        }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

