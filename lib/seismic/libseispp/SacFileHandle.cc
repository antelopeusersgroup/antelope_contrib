#include "SacFileHandle.h"
using namespace std;
using namespace SEISPP;
/* These allowed values of the IZTYPE variable in SAC headers*/
const int IUNKN(69);
const int IB(9);
const int IDAY(10);
const int IO(11);
const int IA(12);
/* Documentation allows ITn with n=0,...,9 but won't mess with this
   unless it proves necessary */
/* This assumes several things:  requires that orderkeys and 
   ensemblelist std list containers are allowed to be empty 
   in the GenericFileHandle.  This happens because Sac has no 
   concept of an ensemble.  Further it assumes the cross reference
   map defines the internal names for ns_key and dt_key used
   in this call below.  Finally, for this implementation we
   alway open the file read only.
   */
SacFileHandle::SacFileHandle(string filename,AttributeCrossReference& namemap,
                        list<string> mdlist,list<string>okey,
                        list<string>ensmdl)
   : GenericFileHandle(filename,string("SAC"),namemap,okey,ensmdl,
                       mdlist,true,string("nsamp"),
                       string("dt"),1.0,true,true)
{
    /*
    list<string>orderkeys;
    list<string>ensmdlist;
    */

    if(SEISPP_verbose) cerr << "Created SacFileHandle for file="
        << filename<<endl;
}
/* In this constructor the handle is always created in read only mode.  */
SacFileHandle::SacFileHandle(string filename)
    : GenericFileHandle(filename,string("SAC"),true)
{
    const string base_error("SacFileHandle::file name constructor:  ");
    /* Sac has no concept of ensembles so these two lists are simply
       created as empty containers */
    list<string> orderkeys,ensmdlist;
    /* We assume we can find AttributeMap definitions in this pf file.*/
    Pf *pf;
    if(pfread(const_cast<char *>("SacFileHandle"),&pf))
        throw SeisppError(base_error + "pfread failed for SacFileHeader.pf");
    try {
        string inxref=pftbl2string(pf,"SAC_metadata_cross_reference");
        AttributeCrossReference sacxref(inxref);
        /* Also require the Tbl SAC_metadata_list */
        list<string> sacmdlist=pftbl2list(pf,"SAC_metadata_list");
        this->set_required(sacxref,orderkeys,ensmdlist,sacmdlist,
                string("nsamp"), string("dt"),1.0,true,true);
    } catch(...){
        pffree(pf);
        throw;
    }
    pffree(pf);
}
TimeSeries SacFileHandle::GetNextSeismogram()
{
    TimeSeries result
        =dynamic_cast<GenericFileHandle*>(this)->GetNextSeismogram();
    /* SAC keeps time in a set of date ints  with a complicated logic 
       that allows multiple ways to define the absolute time of the
       first sample.  The following handles this to the best of my 
       knowledge of all the common permutation of this obnoxious
       complication of this format.  Note this assumes these names
       are defined in the cross reference map.  
       internal names in the cross reference map that are lower
       case versions of the standard sac names for the date attributes*/
    //DEBUG
    //cout << dynamic_cast<Metadata&>(result);
    int iztype;
    try {
        iztype=result.get_int("iztype");
    }catch(SeisppError& serr)
    {
        cerr << "Warning:   iztype variable not loaded.  Set to unknown"<<endl
            << "Output assumes first sample time is 0 and relative time"<<endl;
        iztype=IUNKN;
    }
    int yr,day,hr,min,nzsec,msec;
    double b,fsec;
    char tstr[64];
    switch (iztype)
    {
    case IUNKN:
        result.t0=0.0;
        break;
    case IB:
    case IDAY:
    case IO:
    case IA:
        try {
            yr=result.get_int("year");
            day=result.get_int("jday");
            hr=result.get_int("hour");
            min=result.get_int("min");
            /* an evil inconsistency because of a type clash. 
               Serious maitenance warning */
            nzsec=result.get_int("nzsec");
            msec=result.get_int("msec");
            b=result.get_double("b");
        } catch(...){throw;};
        fsec=((double)(nzsec*1000+msec))/1000.0;
        sprintf(tstr,"%04d%03d:%02d:%02d:%06.3lf",yr,day,hr,min,fsec);
        result.t0=str2epoch(tstr)+b;
        break;
    default:
        throw SeisppError(string("SacFileHandle::GetNextSeismogram:  ")
                + "iztype header set to unsupported value");
    }
    return(result);
}
