#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include "stock.h"
#include "seispp.h"
#include "ensemble.h"
#include "AttributeCrossReference.h"
#include "dbpp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "dbloadhdr db [-t type -o logfile -pf pffile --help -binary] < infile > outfile"
        <<endl
        << "Load attributes to seismogram headers from Datascope database db"<<endl
        << "-t Change object type from default (ThreeComponentSeismogram)"<<endl
        << "    (allowed options=ThreeComponentEnsemble, TimeSeries, TimeSeriesEnsemble)"
        <<endl
        << "-o - log values set to logfile in an ascii table format (default is no log file)"<<endl
        << " -pf - use alternative parameter file instead of default dbloadhdr"
        << endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"<<endl
        << "Error log file is defined in pf (default dbloadhdr_error.log)"
        <<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE, PMTS, TS, TSE};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else if(otype=="ParticleMotionTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
AttributeCrossReference pfload_ACR(Pf *pf,string tag)
{
  try{
    string acrstr=pftbl2string(pf,tag.c_str());
    return (AttributeCrossReference(acrstr));
  }catch(...){throw;};
}
/* this is a helper for the template below */
Metadata load_find_key(Metadata& d, set<string>& dbmkeys,
  AttributeCrossReference& matchkeys)
{
  try{
    Metadata mdkey;
    set<string>::iterator mkptr;
    for(mkptr=dbmkeys.begin();mkptr!=dbmkeys.end();++mkptr)
    {
      int ival;
      double dval;
      string sval;
      /* We don't worry about exceptions here because the main program
      validates integrity of match keys - warning if you try to extract
      this template code.  The type method requires the internal
      key so we to fetch it - tha tis the potentially dangerous issue */
      string ik=matchkeys.internal(*mkptr);
      MDtype mkey_type=matchkeys.type(ik);
      switch (mkey_type) {
        case MDint:
          ival=d.get_int(ik);
          mdkey.put(*mkptr,ival);
          break;
        case MDreal:
          dval=d.get_double(*mkptr);
          mdkey.put(*mkptr,dval);
          break;
        case MDstring:
          sval=d.get_string(*mkptr);
          mdkey.put(*mkptr,sval);
          break;
        case MDboolean:
        case MDinvalid:
        default:
          throw SeisppError(string("load_find_key:  illegal match key specified"));
      }
    }
    return mdkey;
  }catch(...){throw;};
}
/* This is a generic routine to handle all supported types. Works
only on children of Metadata.  Returns error count */
template <class Datatype>
  int loadhdr(DatascopeMatchHandle& dbh,
     AttributeCrossReference& attributes_to_copy,
     AttributeCrossReference& matchkeys,
     string logfile, string error_file, bool binary_mode)
{
  int error_count(0);  // We return the count of failures
  /* We use this repeatedly below changing only the record number for
  matches so we set it immediately */
  Dbptr db=dbh.db;
  try{
    const string base_error("loadhr generic procedure:  ");
    ofstream errlog;
    /* Open the error file in append mode to avoid overwriting error
    log data that is commonly essential.*/
    errlog.open(error_file.c_str(),ios::app);
    if(errlog.bad())
    {
      throw SeisppError(base_error+"Cannot open error log file="
           +error_file);
    }
    /* Open error file and log file if desired.  log file will be
    enabled only if the logfile variable is not an empty string.*/
    bool save_logfile(false);
    ofstream logstrm;
    if(logfile.length()>0)
    {
      /* set this up to append if the file already exists*/
      logstrm.open(logfile.c_str(),ios::app);
      if(logstrm.bad())
      {
        errlog.close();
        throw SeisppError(base_error+"Cannot open log file="+logfile);
      }
    }
    /* First we fetch the set of internal names we want to load  */
    set<string> hdrnames=attributes_to_copy.internal_names();
    set<string>::iterator hptr;
    /* Similarly we cache the match key names, but here we need the external_names
    names */
    set<string> dbmkeys=matchkeys.external_names();
    /* This is used to save logdata.  Values read and written are posted to
    this list, one for each attribute.  Bad values are defined by values here */
    list<string> logtuple;
    const string bad_int("-999999999");  // nine 9s
    const string bad_real("-999999999E-99");  // nine 9s with and exponent
    const string bad_string("UNDEFINED");

    char form('t');
    if(binary_mode)form='b';
    /* Build handle to serialization using stdin and stdout */
    StreamObjectReader<Datatype> inp(form);
    StreamObjectWriter<Datatype> outp(form);
    int dcount(0);
    while(inp.good())
    {
      Datatype d(inp.read());
      logtuple.clear();
      /* We use this to build an error log for this object.  We write it
      to the error log file only if the count is nonzero */
      stringstream ss;
      ss<<"/////Errors for data at file position="<<dcount<<"/////"<<endl;
      /* Now the first step is to try to find the rows in the db that match
      keys*/
      Metadata mdkey;
      try {
        mdkey=load_find_key(d,dbmkeys,matchkeys);
      }catch(SeisppError& serr)
      {
        ss << "One of the required keys for database row match is not in this object"
           <<endl
           <<"This is the error message thrown by one of the Metadata get methods:"
           <<endl
           <<serr.message<<endl
           << "Not attributes will be set for this object"<<endl;
        errlog<<ss.str();
        error_count += hdrnames.size();
        /* An intentional mismatch here - might want to write a tuple of
        all bad values in this case, but for the way I expect to use a
        verbose log file that would be in the way.    */
        continue;   // Process next event
      }
      list<long> match_records=dbh.find(mdkey);
      /* We skip similar to above if there are not matches and issue
      a warning if there are multiple matches keeping the last */
      int nmatch=match_records.size();
      if(nmatch<1)
      {
        ss<<"No match was found in the database for one or more of the match keys"
        <<endl<<"Run dbverify and dbe to debug this database problem"<<endl;
        errlog<<ss.str();
        error_count += hdrnames.size();
        continue;
      }
      if(nmatch>1)
      {
        ss <<"WARNING:  "
           <<nmatch
           <<" matching records were found for the defined match keys for this object"
           <<endl
           << "Will use the last record found but this may be an error."<<endl
           << "Check database and be sure this is what you want"
           <<endl;
        errlog<<ss.str();
      }
      list<long>::iterator mrptr=match_records.end();
      db.record = (*mrptr);  // use last record

      for(hptr=hdrnames.begin();hptr!=hdrnames.end();++hptr)
      {
        double dval;
        long int ival;
        string sval;
        /* We assume this pair of methods cannot throw an exception. Must
        be true here because we fetched hdrnames from the object*/
        string name_in_db=attributes_to_copy.external(*hptr);
        MDtype mdt=attributes_to_copy.type(*hptr);
        /* We use the C interface for getting values for efficiency and
        because is it is just simpler in this case because the procedural
        return code approach as opposed to a catch/throw error handler */
        int ierr;
        switch(mdt)
        {
          case MDint:
            ierr=dbgetv(db,0,name_in_db.c_str(),&ival,NULL);
            if(ierr==dbINVALID)
            {
              ss<<"dbgetv failed trying to fetch attributre="<<name_in_db<<endl;
              errlog<<ss.str();
              if(save_logfile) logstrm<<bad_int<<" ";
              ++error_count;
            }
            else
            {
              d.put(*hptr,ival);
              if(save_logfile) logstrm<<ival<<" ";
            }
            break;
          case MDreal:
            ierr=dbgetv(db,0,name_in_db.c_str(),&dval,NULL);
            if(ierr==dbINVALID)
            {
              ss<<"dbgetv failed trying to fetch attributre="<<name_in_db<<endl;
              errlog<<ss.str();
              if(save_logfile) logstrm<<bad_real<<" ";
              ++error_count;
            }
            else
            {
              d.put(*hptr,dval);
              if(save_logfile) logstrm<<dval<<" ";
            }
            break;
          case MDstring:
            ierr=dbgetv(db,0,name_in_db.c_str(),&sval,NULL);
            if(ierr==dbINVALID)
            {
              ss<<"dbgetv failed trying to fetch attributre="<<name_in_db<<endl;
              errlog<<ss.str();
              if(save_logfile) logstrm<<bad_int<<" ";
              ++error_count;
            }
            else
            {
              d.put(*hptr,sval);
              if(save_logfile) logstrm<<sval<<" ";
            }
            break;
          case MDboolean:
          case MDinvalid:
          default:
            throw SeisppError(base_error
              +"Illegal type specified for attribute="+name_in_db);
        };
      }
      outp.write(d);
      if(save_logfile) logstrm<<endl;
      ++dcount;
    }
    errlog.close();
    if(save_logfile) logstrm.close();
    return dcount;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    if(argc<2) usage();
    string dbname(argv[1]);
    if(dbname=="--help") usage();
    string pffile("dbloadhdr");
    bool binary_mode(false);
    string otype("ThreeComponentSeismogram");
    bool write_logfile(false);
    string logfile("");
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-binary")
        {
            binary_mode=true;
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
        }
        else if(sarg=="-t")
        {
          ++i;
          if(i>=argc)usage();
          otype=string(argv[i]);
        }
        else if(sarg=="-o")
        {
          ++i;
          if(i>=argc)usage();
          write_logfile=true;
          logfile=string(argv[i]);
        }
        else
            usage();
    }
    Pf *pf;
    if(pfread(const_cast<char*>(pffile.c_str()),&pf))
    {
      cerr << "pfread failed for parameter file="<<pffile<<endl;
      usage();
    }
    try{
      const string dbprocesstag("dbprocess_commands");
      const string mdltag("attributes_to_load");
      const string keys_pftag("match_keys");
      AllowedObjects dtype=get_object_type(otype);

      /* The AttributeCrossReference was designed for a somewhat different purpose
      but it will work well here.   It allows us to map the css3.0 attribute
      names to anything we want in this context.  In a more coherent system
      this should be globally available.   Worth considering as a future
      development*/
      AttributeCrossReference matchkeys=pfload_ACR(pf,keys_pftag);
      AttributeCrossReference attributes_to_copy=pfload_ACR(pf,mdltag);
      AttributeMap am(string("css3.0"));
      DatascopeHandle dbh0(dbname,true);
      DatascopeHandle dbh(dbh0,pf,dbprocesstag);
      /* We require only the names for a match in the database so we
      need to build this little list container for the DatascopeMatchHandle.
      There is a potential disconnect that we need to watch out for here
      with a type mismatch between the definition in matchkeys and the
      AttributeMap.   We check that and exit if that is a problem.*/
      list<string> mkeys;
      set<string> mtmp=matchkeys.external_names();
      set<string>::iterator sptr;
      for(sptr=mtmp.begin();sptr!=mtmp.end();++sptr)
      {
        string internalkey=matchkeys.internal(*sptr);
        MDtype internal_type=matchkeys.type(internalkey);
        /* this is more than a little ugly - this interface needs to be
        improved as this is super low level. I badly violated a basic rule
        of C++ programming in this interface where this detail should not
        be accessible*/
        map<string,AttributeProperties>::iterator amptr;
        amptr=am.attributes.find(*sptr);
        if(amptr==am.attributes.end())
        {  cerr << "dbloadhdr:  Invalid database match key name="
            << (*sptr)<<" was given in parameter file"<<endl
            << "Attribute name is not defined in AttributeMap for css3.0"<<endl
            << "See contrib parameter file=seispp_attribute_maps.pf"<<endl
            << "Fix match key name specification in parameter file"<<endl;
          exit(-1);
        }
        AttributeProperties aptest=am.attributes[*sptr];
        if(aptest.mdt != internal_type)
        {
          cerr <<"dbloadhdr:  Type mismatch in match key name="<<(*sptr)<<endl
            << "Type defined for this key must match type defined in AttributeMap for css3.0"
            << endl
            << "Defined in contrib parameter file=seispp_attribute_maps.pf"<<endl;
          exit(-1);
        }
        mkeys.push_back(*sptr);
      }

      DatascopeMatchHandle dbhm(dbh,string(""),mkeys,am);
      /* Converting pf to it's C++ form is a bit inefficient, but
      less error prone in coding */
      Metadata control(pf);
      string error_log_file=control.get_string("error_log_file");
      int nerr;
      switch(dtype)
      {
        case TS:
          nerr=loadhdr<TimeSeries>(dbhm,attributes_to_copy,matchkeys,
              logfile,error_log_file,binary_mode);
          break;
        case TSE:
          nerr=loadhdr<TimeSeriesEnsemble>(dbhm,attributes_to_copy,matchkeys,
              logfile,error_log_file,binary_mode);
          break;
        case TCE:
          nerr=loadhdr<ThreeComponentEnsemble>(dbhm,attributes_to_copy,matchkeys,
              logfile,error_log_file,binary_mode);
          break;
        case TCS:
          nerr=loadhdr<ThreeComponentSeismogram>(dbhm,attributes_to_copy,matchkeys,
              logfile,error_log_file,binary_mode);
          break;
        case PMTS:
        default:
          cerr << "dbloadhdr:  ParticleMotionTimeSeries not currently supported"
            << endl;
      };
      if(nerr>0)cerr <<"dbloadhdr:  "<<nerr
         <<" load failures - check error log file="<<error_log_file<<endl;
    }
    catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
