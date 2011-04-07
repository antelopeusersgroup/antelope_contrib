#include <float.h>
#include <sys/stat.h>
#include "SeisppError.h"
#include "TimeSeries.h"
#include "ensemble.h"
#include "GenericFileHandle.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{

GenericFileHandle::GenericFileHandle(string filename,
      string tracetype, AttributeCrossReference& namemap,
      list<string> okeys, list<string> ensmdlist, list<string> tmdlist, 
      bool read_only, string nskey, string dt_key, double dtscl, 
      bool using_sample_interval,bool nowrit) 
            : xref(namemap), dbuffer(tracetype), fname(filename)
{
    const string base_error("GenericFileHandle constructor:  ");
    no_write_dead=nowrit;
    retry_limit=50;
    sleep_interval=1;
    /* We must require the orderkeys to be loaded for each trace
       as changes in keys is the signal to mark a new ensemble.
       Note that is not as general as it could be (e.g. it doesn't 
       allow for interval tests) but is sufficient for now.  This 
       is research code after all. */
    list<string>::iterator okptr;
    for(okptr=okeys.begin();okptr!=okeys.end();++okptr)
    {
        try{
            string exttest=xref.external(*okptr);
        }catch(SeisppError& serr) {
            throw SeisppError(base_error
                    + "ensemble metdata key="
                    + *okptr
                    + " is not defind in cross reference map\nAdd to definitions");
        }
    }
    orderkeys=okeys;
    ensemble_mdlist=ensmdlist;
    trace_mdlist=tmdlist;
    readmode=read_only;
    nsamp_keyword=nskey;
    dt_keyword=dt_key;
    dtscale=dtscl;
    key_is_dt=using_sample_interval;
    // Cache external names for these keywords for efficiency
    // and to avoid later error traps
    try {
        dtkey_ext=xref.external(dt_keyword);
    } catch (...)
    {
        throw SeisppError(base_error
                + "Required metadata keyword ="
                + dt_keyword
                + " does not have a cross reference for file format "
                +tracetype);
    }
    try {
        nskey_ext=xref.external(nsamp_keyword);
    } catch (...)
    {
        throw SeisppError(base_error
                + "Required metadata keyword ="
                + nsamp_keyword
                + " does not have a cross reference for file format "
                + tracetype);
    }

    if(readmode)
    {
        fp=fopen(filename.c_str(),"r");
        if(fp==NULL) throw SeisppError(base_error
                + "Cannot open file "
                + filename
                + " for reading");
        fsize=this->filesize();
        current_file_position=0;
    }
    else
    {
        outstrm.sync_with_stdio();
        outstrm.open(filename.c_str(),ios::out 
                | ios::app | ios::binary);
        if(outstrm.fail()) throw SeisppError(base_error
                + "Cannot open file "
                + filename
                + " for output in append mode");
        current_file_position=outstrm.tellp();
        fp=NULL;
    }
}
GenericFileHandle::GenericFileHandle(const GenericFileHandle& parent)
{
    throw SeisppError(string("GenericFileHandle: coding error\n")
      + "Copying a GenericFileHandle is not allowed in this implementation.  Try using a shared_pointer\n");
}
GenericFileHandle::~GenericFileHandle()
{
    if(fp!=NULL)
        fclose(fp);
    // Needs similar test to check outstrm
    if(outstrm.is_open())
    {
        this->unlock();
        outstrm.close();
    }
}

long GenericFileHandle::filesize()
{
    long result;
    if(readmode)
    {
        if(fp==NULL) 
            return 0L;
        else
        {
            long current=ftell(fp);
            fseek(fp,0L,SEEK_END);
            result=ftell(fp);
            fseek(fp,current,SEEK_SET);
        }
    }
    else
    {
        try {
            long current=outstrm.tellp();
            outstrm.seekp(0L,ios_base::end);
            result=outstrm.tellp();
            outstrm.seekp(current,ios_base::beg);
        } catch(exception& excpt)
        {
            throw SeisppError(string("GenericFileHandle::filesize:  ")
             + string("IO error trying to determine file size.\nSystem Message: ")
             +excpt.what());
        }
    }

    fsize=result;
    return(result);
}
bool GenericFileHandle::eof()
{
    if(readmode)
    {
        long current=ftell(fp);
        if(current==fsize) 
            return true;
        else
            return false;
    }
    else
        /* Always respond true for writing */
        return true;
}
/* This is common code used in get methods below for loading
   metadata.  Used for both global (ensemble) Metadata and individual
   Metadata.  All involve loading from headers in FixedFormatTrace
   objects.  All should dynamic_cast to a Metadata reference to use
   this private method.  Note carefully that mdlist is a name of 
   internal names to be extracted.   */
void GenericFileHandle::LoadMetadata(FixedFormatTrace& dext,
        Metadata& d,list<string> mdlist)
{
    try {
        string extkey;
        list<string>::iterator mdlptr;
        MDtype keydatatype;
        for(mdlptr=trace_mdlist.begin();mdlptr!=trace_mdlist.end();
                ++mdlptr)
        {
            extkey=xref.external(*mdlptr);
            keydatatype=xref.type(*mdlptr);
            int ival;
            double dval;
            string sval;
            bool bval;
            switch(keydatatype)
            {
                case MDint:
                    ival=dext.get<int>(extkey);
                    d.put(*mdlptr,ival);
                    break;
                case MDreal:
                    dval=dext.get<double>(extkey);
                    d.put(*mdlptr,dval);
                    break;
                case MDboolean:
                    bval=dext.get<bool>(extkey);
                    d.put(*mdlptr,bval);
                    break;
                case MDstring:
                    sval=dext.get_string(extkey);
                    d.put(*mdlptr,sval);
                    break;
                case MDinvalid:
                default:
                    //Silently do nothing if invalid or something else
                    continue;
            }
        }
    }catch(...){throw;};
}
/* This private method standardizes loading of core attributes of 
   the BasicTimeSeries object.  Useful becasue both TimeSeries
   and ThreeComponentSeismogram objects are children of this 
   base.  In a different design this could have been made a 
   constructor, but since this handle is essentially an add on
   this is done this way - common code as a private method.
   d is the current FixedFormatTrace buffer.  The base variable
   should always be received as a dynamic_cat from the higher
   order object (i.e. TimeSeries or ThreeComponentSeismogram). */
void GenericFileHandle::LoadCommonAttributes(FixedFormatTrace& d, 
        BasicTimeSeries& base)
{
    try {
        int nsext=d.get<int>(nskey_ext);
        base.ns=nsext;
        double dtext=d.get<double>(dtkey_ext);
        if(!key_is_dt) dtext=1.0/dtext;
        /* Required because some formats like segy use time in microsecond*/
        dtext *= dtscale;
        base.dt=dtext;
        /* Two required metadata entries.  For no abort if these aren't
           defined.  A bit dogmatic, but for now assume we will always
           be writing these as working files from some other source 
           like a datascope database.*/
        string extkey;
        base.t0=d.tstart();
        base.tref=d.tref;
        /* Always mark data live.  If format allows this is then
           overridden.  */
        base.live=true;
    }catch(...){throw;};
}
TimeSeries GenericFileHandle::GetNextSeismogram()
{
    const string base_error("GenericFileHandle::GetNextSeismogram:  ");
    if(!readmode) throw SeisppError(base_error
            + "Coding error.  Trying to read when handle is in write mode");
    if(dbuffer.data_are_3c) throw SeisppError(base_error
            + "Error trying to read 3C\nData format="
            + dbuffer.format_description()
            + " does not support three component data");
    if(this->eof()) throw SeisppError(base_error
            + "Attempt to read past end of file.  Check caller program logic.");
    try {
        FixedFormatTrace NextSeis(dbuffer,fp,nskey_ext,dtkey_ext,
                key_is_dt);
        TimeSeries result(NextSeis.ns);
        LoadCommonAttributes(NextSeis,
                dynamic_cast<BasicTimeSeries&> (result));
        LoadMetadata(NextSeis,
                dynamic_cast<Metadata&> (result), trace_mdlist);
        result.s=NextSeis.data();
        current_file_position=ftell(fp);
        return(result);
    }catch(...){throw;};
}
/* Private method used by the GetNextEnsemble method below */
bool GenericFileHandle::keys_match(Metadata& d1, Metadata& d2)
{
    list<string>::iterator nmptr;
    for(nmptr=orderkeys.begin();nmptr!=orderkeys.end();++nmptr)
    {
        /* Constructor guaranteeds that orderkeys are in the set of
           metadata loaded with each time series so we do not need to
           trap that possible error condition */
        MDtype thiskeytype=xref.type(*nmptr);
        int ival;
        double dval,dtest;
        bool bval;
        string sval;
        switch(thiskeytype)
        {
            case MDint:
                ival=d1.get_int(*nmptr);
                if(ival!=(d2.get_int(*nmptr))) return(false);
                break;
            case MDreal:
                dval=d1.get_double(*nmptr);
                dtest=(dval-d2.get_double(*nmptr))/dval;
                dtest=fabs(dtest);
                if(dtest>DBL_EPSILON) return(false);
                break;
            case MDboolean:
                // This is kind of irrational as a key, but will code it anyway
                bval=d1.get_bool(*nmptr);
                if(bval || d2.get_bool(*nmptr)) return(false);
                break;
            case MDstring:
                sval=d1.get_string(*nmptr);
                if(sval!=(d2.get_string(*nmptr))) return(false);
                break;
            case MDinvalid:
            default:
                throw SeisppError(
                        string("GenericFileHandle::keys_match private method:")
                        + "Ensemble key = "
                        + *nmptr
                        + "used to define ensemble boundary has invalid type");
        }
    }
    return(true);
}

auto_ptr<TimeSeriesEnsemble> GenericFileHandle::GetNextEnsemble()
{
    const string base_error("GenericFileHandle::GetNextEnsemble:  ");
    if(!readmode) throw SeisppError(base_error
            + "Coding error.  Trying to read when handle is in write mode");
    if(this->eof()) throw SeisppError(base_error
            + "Attempt to read past end of file.  Fix calling program logic.");
    if(dbuffer.data_are_3c) throw SeisppError(base_error
            + "Error trying to read 3C\nData format="
            + dbuffer.format_description()
            + " does not support three component data");
    /* When reading an ensemble we assume all the traces have the same 
       value for ensemble attributes.  Thus we assume we can read them 
       from the first trace in each ensemble */
    try {
        // Save this to insulate against changes in behaviour of
        // FixedFormatTrace object
        long last_offset=current_file_position;
        auto_ptr<TimeSeriesEnsemble> result(new TimeSeriesEnsemble());
        FixedFormatTrace NextSeis(dbuffer,fp,nskey_ext,dtkey_ext,
                key_is_dt);
        LoadMetadata(NextSeis,dynamic_cast<Metadata&>(*result),
                ensemble_mdlist);
        /* Minor inefficiency but we back up to read the last trace
           again.  This is necessary so we can just call the 
           GetNextSeismogram method to load the whole thing. */
        fseek(fp,last_offset,SEEK_SET);
        /* Now we load data until the key attributes change or we 
           hit eof */
        TimeSeries d,dlast;
        int tracecount(0);
        do {
            last_offset=current_file_position;
            d=this->GetNextSeismogram();
            if(tracecount==0) dlast=d;
            if(this->keys_match(d,dlast))
            {
                result->member.push_back(d);
                current_file_position=ftell(fp);
                ++tracecount;
            }
            else
            {
                /*If keys do not match we already read the next seismogram 
                  so we have to then back the file pointer so 
                  the next read will be from the right position.*/
                fseek(fp,last_offset,SEEK_SET);
                current_file_position=last_offset;
                // this is the loop break except at eof
                break;
            }
            dlast=d;
        }while(!this->eof());
        return(result);
    }catch(...){throw;};
}
/* This code was modeled closely after GetNextSeismogram (scalar data) */
ThreeComponentSeismogram GenericFileHandle::GetNext3CSeismogram()
{
    const string base_error("GenericFileHandle::GetNext3CSeismogram:  ");
    if(!readmode) throw SeisppError(base_error
            + "Coding error.  Trying to read when handle is in write mode");
    if(!dbuffer.data_are_3c) throw SeisppError(base_error
            + "Error trying to read 3C\nData format="
            + dbuffer.format_description()
            + " does not support three component data");
    if(this->eof()) throw SeisppError(base_error
            + "Attempt to read past end of file.  Check caller program logic.");
    try {
        FixedFormatTrace NextSeis(dbuffer,fp,nskey_ext,dtkey_ext,
                key_is_dt);
        ThreeComponentSeismogram result(NextSeis.ns);
        LoadCommonAttributes(NextSeis,
                dynamic_cast<BasicTimeSeries&> (result));
        LoadMetadata(NextSeis,
                dynamic_cast<Metadata&> (result), trace_mdlist);
        /* For 3c data we assume the data are stored in a single 
           large vector.  The data method of FixedFormatTrace returns
           this as a stl vector container */
        vector<double> rawsamples=NextSeis.data();
        /* Constructor above is assumed to have allocated data matrix u*/
        int nsamp=result.ns;
        if(NextSeis.channel_order)
            dcopy(3*nsamp,&(rawsamples[0]),1,result.u.get_address(0,0),1);
        else
            for(int k=0;k<3;++k) dcopy(nsamp,&(rawsamples[k*nsamp]),1,
                    result.u.get_address(k,0),3);
        current_file_position=ftell(fp);
        return(result);
    }catch(...){throw;};
}
/* This code was derived from GetNextEnsemble (scalar data version) above*/
auto_ptr<ThreeComponentEnsemble> GenericFileHandle::GetNext3CEnsemble()
{
    const string base_error("GenericFileHandle::GetNextEnsemble:  ");
    if(!readmode) throw SeisppError(base_error
            + "Coding error.  Trying to read when handle is in write mode");
    if(this->eof()) throw SeisppError(base_error
            + "Attempt to read past end of file.  Fix calling program logic.");
    /* When reading an ensemble we assume all the traces have the same 
       value for ensemble attributes.  Thus we assume we can read them 
       from the first trace in each ensemble */
    try {
        // Save this to insulate against changes in behaviour of
        // FixedFormatTrace object
        long last_offset=current_file_position;
        auto_ptr<ThreeComponentEnsemble> result(new ThreeComponentEnsemble());
        FixedFormatTrace NextSeis(dbuffer,fp,nskey_ext,dtkey_ext,
                key_is_dt);
        LoadMetadata(NextSeis,dynamic_cast<Metadata&>(*result),
                ensemble_mdlist);
        /* Minor inefficiency but we back up to read the last trace
           again.  This is necessary so we can just call the 
           GetNextSeismogram method to load the whole thing. */
        fseek(fp,last_offset,SEEK_SET);
        /* Now we load data until the key attributes change or we 
           hit eof */
        ThreeComponentSeismogram d,dlast;
        int tracecount(0);
        do {
            last_offset=current_file_position;
            d=this->GetNext3CSeismogram();
            if(tracecount==0) dlast=d;
            if(this->keys_match(d,dlast))
            {
                result->member.push_back(d);
                current_file_position=ftell(fp);
                ++tracecount;
            }
            else
            {
                /*If keys do not match we already read the next seismogram 
                  so we have to then back the file pointer so 
                  the next read will be from the right position.*/
                fseek(fp,last_offset,SEEK_SET);
                // Not really required, but best made explicit
                current_file_position=last_offset; 
                // this is the loop break except at eof
                break;
            }
            dlast=d;
        }while(!this->eof());
        return(result);
    }catch(...){throw;};
}
void GenericFileHandle::put_metadata_to_dbuffer(Metadata& d,
        list<string>& metanm)
{
    list<string>::iterator mdlptr;
    try{
        for(mdlptr=metanm.begin();mdlptr!=metanm.end();++mdlptr)
        {
            int ival;
            double dval;
            bool bval;
            string sval;
            string extkey;
            MDtype thiskey=xref.type(*mdlptr);
            switch(thiskey)
            {
                case MDint:
                    ival=d.get_int(*mdlptr);
                    extkey=xref.external(*mdlptr);
                    dbuffer.put<int>(extkey,ival);
                    break;
                case MDreal:
                    dval=d.get_double(*mdlptr);
                    extkey=xref.external(*mdlptr);
                    dbuffer.put<double>(extkey,dval);
                    break;
                case MDboolean:
                    bval=d.get_bool(*mdlptr);
                    extkey=xref.external(*mdlptr);
                    dbuffer.put<bool>(extkey,bval);
                    break;
                case MDstring:
                    sval=d.get_string(*mdlptr);
                    extkey=xref.external(*mdlptr);
                    dbuffer.put_string(extkey,sval);
                    break;
                default:
                    cerr << "Warning:  undefined attribute "
                        << *mdlptr << " found in list of output metadata"
                        <<endl
                        << "Output will not be useful if this attribute"
                        << " is essential"<<endl;
            }
        }
    }catch(...){throw;};
}

int GenericFileHandle::put(TimeSeries& d)
{
    /* To be stateless one might want to always force a seek to eof,
       but I do not do that on purpose for efficiency. Instead 
       all we really need to do is copy the required metadata to
       the FixedFormatTrace object and then call operator >>.
       So first we have a loop to post all the required metadata.*/
    try {
        if(no_write_dead && (!d.live)) return(0);
        // Assume constructor already defined dbuffer 
        // Ensemble metadata will be written to every output 
        // trace along with the individual trace attributes.  
        // allow every trace to have a different sample size
        dbuffer.resize(d.ns);
        dbuffer.ns=d.ns;
        dbuffer.put(0,d.s);
        this->put_metadata_to_dbuffer(d,ensemble_mdlist);
        this->put_metadata_to_dbuffer(d,trace_mdlist);
        this->lock();
        dbuffer.write(outstrm);
        this->unlock();
    } catch(...){throw;};
    return(1);
}
int GenericFileHandle::put(TimeSeriesEnsemble& d)
{
    int tracecount;
    vector<TimeSeries>::iterator dptr;
    try {
        /* This loop could use the put(TimeSeries method but we 
           repeat the code there to avoid repeated locks. */
        this->lock();
        for(dptr=d.member.begin(),tracecount=0;dptr!=d.member.end();
                ++dptr,++tracecount) 
        {
            if(no_write_dead && !(dptr->live)) continue;
            // Allow each trace to have a different number of samples
            dbuffer.resize(dptr->ns);
            dbuffer.ns=dptr->ns;
            dbuffer.put(0,dptr->s);
            this->put_metadata_to_dbuffer(*dptr,trace_mdlist);
            /* write these second so if attributes are duplicated in
               trace and ensemble lists the ensemble versions override.
               This uses the model that ensemble metadata are common
               to all members*/
            this->put_metadata_to_dbuffer(d,ensemble_mdlist);
            dbuffer.write(outstrm);
        }
        this->unlock();
    } catch(...){
        //Make sure we unlock the file if it got locked and we had a failure
        this->unlock();
        throw;
    };
    return(tracecount);
}
/* Now similar code for ThreeComponentSeismogram and ensemble */

int GenericFileHandle::put(ThreeComponentSeismogram& d)
{
    const string base_error("GenericFileHandle::put method for 3c seismogram:  ");
    /* To be stateless one might want to always force a seek to eof,
       but I do not do that on purpose for efficiency. Instead 
       all we really need to do is copy the required metadata to
       the FixedFormatTrace object and then call operator >>.
       So first we have a loop to post all the required metadata.*/
    try {
        if(no_write_dead && (!d.live)) return(0);
        /* The put methods below will cause a seg fault if we didn't 
           test for this condition so an exception is essential */
        if(!dbuffer.data_are_3c) throw SeisppError(base_error
                + "trying to put 3c data to requested format is illegal\n"
                + "Definition does not defines this as a 3c format.");

        /* Assume constructor already defined dbuffer.  As for
           TimeSeries we write ensemble metadata to each trace and
           trace headers for each 3c block. */
        dbuffer.resize(d.ns);
        dbuffer.ns=d.ns;
        int ntotal=3*dbuffer.ns;
        if(dbuffer.channel_order)
        {
            // channel order is the transpose of the u matrix
            dmatrix ut=tr(d.u);
            dbuffer.put(0,ntotal,ut.get_address(0,0));
        }
        else
        {
            dbuffer.put(0,ntotal,d.u.get_address(0,0));
        }
        this->put_metadata_to_dbuffer(d,ensemble_mdlist);
        this->put_metadata_to_dbuffer(d,trace_mdlist);
        this->lock();
        dbuffer.write(outstrm);
        this->unlock();
    } catch(...){throw;};
    return(1);
}
int GenericFileHandle::put(ThreeComponentEnsemble& d)
{
    const string base_error("GenericFileHandle::put method for 3c ensemble:  ");
    if(!dbuffer.data_are_3c) throw SeisppError(base_error
            + "3c ensemble put method called for a format that is not 3c\n"
            + "Check format selected and/or definition of this format");
    int tracecount;
    vector<ThreeComponentSeismogram>::iterator dptr;
    try {
        /* This loop could use the put(ThreeComponentSeismogram method but we 
           repeat the code there to avoid repeated locks. */
        this->lock();
        for(dptr=d.member.begin(),tracecount=0;dptr!=d.member.end();
                ++dptr,++tracecount) 
        {
            if(no_write_dead && !(dptr->live)) continue;
            // Allow each trace to have a different number of samples
            dbuffer.resize(dptr->ns);
            dbuffer.ns=dptr->ns;
            int ntotal=3*dbuffer.ns;
            if(dbuffer.channel_order)
            {
                // Channel order is the transpose of the data matrix
                dmatrix ut=tr(dptr->u);
                dbuffer.put(0,ntotal,ut.get_address(0,0));
            }
            else
            {
                dbuffer.put(0,ntotal,dptr->u.get_address(0,0));
            }
            this->put_metadata_to_dbuffer(*dptr,trace_mdlist);
            /* write these second so if attributes are duplicated in
               trace and ensemble lists the ensemble versions override.
               This uses the model that ensemble metadata are common
               to all members*/
            this->put_metadata_to_dbuffer(d,ensemble_mdlist);
            dbuffer.write(outstrm);
        }
        this->unlock();
    } catch(...){
        //Make sure we unlock the file if it got locked and we had a failure
        this->unlock();
        throw;
    };
    return(tracecount);
}

/* Copied from the web from url http://www.techbytes.ca/techbyte103.html */
bool FileExists(string strFilename) {
  struct stat stFileInfo;
  bool blnReturn;
  int intStat;

  // Attempt to get the file attributes
  intStat = stat(strFilename.c_str(),&stFileInfo);
  if(intStat == 0) {
    // We were able to get the file attributes
    // so the file obviously exists.
    blnReturn = true;
  } else {
    // We were not able to get the file attributes.
    // This may mean that we don't have permission to
    // access the folder which contains this file. If you
    // need to do that level of checking, lookup the
    // return values of stat which will give you
    // more details on why stat failed.
    blnReturn = false;
  }
  
  return(blnReturn);
}
string lockfilename(string base)
{
    return(base + ".lock");
}
void GenericFileHandle::lock()
{
        string lockfname=lockfilename(fname);
        int i;
        for(i=0;i<retry_limit;++i)
        {
            if(FileExists(lockfname)) 
                sleep(sleep_interval);
            else
            {
                FILE *fp;
                fp=fopen(lockfname.c_str(),"w");
                fclose(fp);
                return;
            }
        }
        throw SeisppError(string("GenericFileHandle::lock  ")
                + "Cannot obtain a lock for output file "+fname);
}
void GenericFileHandle::unlock()
{
    /* Intentionally do nothing if lock file does not exist.
       This allows destructor to clear a lock in the event of an error. */
    string lockfname=lockfilename(fname);
    if(FileExists(lockfname))
    {
        if(remove(lockfname.c_str()))
        {
            throw SeisppError(string("GenericFileHandle::unlock:  ")
                    + string("Could not remove lock file=")
                    + lockfname
                    +"\nRemove manually and beware deadlocks if caller blunders on");
        }
    }
}
void GenericFileHandle::rewind()
{
    if(readmode)
    {
        std::rewind(this->fp);
    }
    else
    {
        outstrm.seekp(0L);
        if(SEISPP_verbose) cerr << "WARNING:  "
            "GenericFileHandle.rewind called on an output file"<<endl;
    }
    current_file_position=0;

}


} // end SEISPP namespace encapsulation
