#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <map>
#include <list>
#include "seispp.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "export_to_mspass < in  [-o outbase -t object_type -pf pffile -v --help -text]"
        <<endl
        << "Writes data to a special exchange format for the open source package mspass"<<endl
        << "The mspass format splits header data (saved in a yaml format) from sample"<<endl
        << "data stored as a binary fwrite of the raw sample data to a different file"<<endl
        << "Positions are stored keyed with an attribute foff used for a calls to seek in reads"<<endl
        << "The Metadata are stored with a base name with a .yaml extension"<<endl
        << "Sample (binary) data are stored with the same root with extension .dat"<<endl
        << " -o Defines the base file name for output (default is MsPASSExport) "<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=TimeSeries or Seismogram.  Others will cause an abort with an error message."<<endl
        << "Default is ThreeComponentSeismogram.)"<<endl
        << "Default is ThreeComponentSeismogram"<<endl
        << " -pf use pffile instead of default export_to_mspass.pf"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* Attributes to be saved are driven by this map container. The key is the
name search in Metadata.  pair return has to save for output (an be the same
as input) and the type as second. */
typedef map<string,pair<string,MDtype>> NameMapper;
/* This procedure parses an input string (normally from argv)
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCS, TCE,  TS, TSE, PMTS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
    {
      cerr << "Cannot handle ThreeComponentEnsemble data directly"<<endl
          << "Run through dismember to produce ThreeComponentSeismograms "
          << "that are atomic in MsPASS"<<endl;
      exit(-1);
    }
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
    {
      cerr << "Cannot handle TimeSeriesEnsemble data directly"<<endl
        << "Run through dismember to produce TimeSeries objects "
        << "that are atomic in MsPASS"<<endl;
        exit(-1);
    }
    else if(otype=="PMTimeSeries")
    {
      cerr << "Cannot currently handle PMTimeSeries data"<<endl;
      exit(-1);
    }
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
NameMapper build_NameMapper(PfStyleMetadata& pf,string tag)
{
  try{
    NameMapper mapper;
    list<string> t=pf.get_tbl(tag);
    if(t.size()<=0)
    {
      cerr << "Error constructing NameMapper for tag="<<tag<<endl
        << "List return has zero length"<<endl;
      exit(-1);
    }
    list<string>::iterator tptr;
    for(tptr=t.begin();tptr!=t.end();++tptr)
    {
      stringstream ss(*tptr);
      string inkey,outkey,typ;
      ss>>inkey;
      ss>>outkey;
      ss>>typ;
      MDtype mdt;
      if( (typ=="real") || (typ=="Real") || (typ=="float") || (typ=="double"))
      {
        mdt=MDreal;
      }
      else if( (typ=="int") || (typ=="integer") || (typ=="INTEGER"))
      {
        mdt=MDint;
      }
      else if( (typ=="bool") || (typ=="Bool") || (typ=="boolean") || (typ=="BOOLEAN"))
      {
        mdt=MDboolean;
      }
      else if( (typ=="string") || (typ=="STRING") || (typ=="String"))
      {
        mdt=MDstring;
      }
      else
      {
        cerr << "build_NameMapper:  error parsing tag="<<tag<<endl
          << "Don't understand type name="<<typ<<endl;
        exit(-1);
      }
      mapper[inkey]=pair<string,MDtype>(outkey,mdt);
    }
    return mapper;
  }catch(SeisppError& serr)
  {
    serr.log_error();
    exit(-1);
  }
  catch(...){throw;};
}
/* We use this to know type to fetch for Metadata after
renaming */
typedef map<string,MDtype> MetadataIndex;

pair<Metadata,MetadataIndex> ExtractAndRename(Metadata& md,
  NameMapper& xref)
{
  Metadata mdo;
  MetadataIndex mdoi;
  NameMapper::iterator xptr;
  for(xptr=xref.begin();xptr!=xref.end();++xptr)
  {
    try{
      /* Totally bizarre syntax in this construct but how we get a pair from a map index*/
      MDtype mdt=xptr->second.second;
      string inkey=xptr->first;
      string okey=xptr->second.first;
      double dval;
      long int ival;
      string sval;
      bool bval;
      switch(mdt)
      {
        case MDint:
          ival=md.get<long>(inkey);
          mdo.put(okey,ival);
          mdoi[okey]=mdt;
          break;
        case MDreal:
          dval=md.get<double>(inkey);
          mdo.put(okey,dval);
          mdoi[okey]=mdt;
          break;
        case MDstring:
          sval=md.get<string>(inkey);
          mdo.put(okey,sval);
          mdoi[okey]=mdt;
          break;
        case MDboolean:
          bval=md.get<bool>(inkey);
          mdo.put(okey,bval);
          mdoi[okey]=mdt;
          break;
        case MDinvalid:
          cerr << "ExtractAndRename - MDinvalid set for key="<<inkey<<endl
              << "This shouldn't happen and indicates a bug - fatal error"<<endl;
          exit(-1);
      };
    }catch(SeisppError& serr)
    {
      serr.log_error();
    }
  }
  return pair<Metadata,MetadataIndex>(mdo,mdoi);
}
void save_metadata(Metadata& d, ofstream& ofs,int count)
{
  try {
    ofs<<"- Object_number: "<<count<<endl;
    MetadataList mdl;
    mdl=d.keys();
    MetadataList::iterator mptr;
    for(mptr=mdl.begin();mptr!=mdl.end();++mptr)
    {
      double dval;
      long ival;
      string sval;
      bool bval;
      Metadata_typedef mtdef=(*mptr);
      switch(mtdef.mdt)
      {
        case MDint:
          ival=d.get<long>(mtdef.tag);
          ofs<<"  "<<mtdef.tag<<": "<<ival<<endl;
          break;
        case MDreal:
          dval=d.get<double>(mtdef.tag);
          ofs<<"  "<<mtdef.tag<<": "<<dval<<endl;
          break;
        case MDboolean:
          bval=d.get<bool>(mtdef.tag);
          ofs<<"  "<<mtdef.tag<<": "<<bval<<endl;
          break;
        case MDstring:
        default:
          sval=d.get<string>(mtdef.tag);
          ofs<<"  "<<mtdef.tag<<": "<<sval<<endl;
      };
    }
  } catch(SeisppError& serr)
  {
    cerr << "save_metadata (Fatal):  Unexpected Metadata get error"<<endl
       << "Coding error that requires a fix - this is the error posted:"<<endl;
    serr.log_error();
    exit(-1);
  }
}

pair<double *,int> get_write_info(TimeSeries& d)
{
  double *ptr=&(d.s[0]);
  return(pair<double*,int>(ptr,d.ns));
}
pair<double*,int> get_write_info(ThreeComponentSeismogram& d)
{
  double *ptr;
  ptr=d.u.get_address(0,0);
  int ntotal=3*d.ns;
  return(pair<double*,int>(ptr,ntotal));
}
template <typename T> int save_sample_data(T& d, ofstream& dout)
{
  try{
    /* We fetch and return the offset position prior to writing - written to
    header data as foff attribute*/
    long foff=dout.tellp();
    pair<double *,int> winfo;
    winfo=get_write_info(d);
    dout.write(reinterpret_cast<char *>(winfo.first),sizeof(double)*winfo.second);
    return foff;
  }catch(...)
  {
    cerr << "save_sample_data (Fatal):   Something threw an unexpected exception"<<endl;
    exit(-1);
  }
}

template <typename DataType> pair<int,int> export_to_mspass
  (ofstream& hdroutput,ofstream& dout,
  NameMapper& require, NameMapper& optional, bool binary_data)
{
    try{
        /* We need this many times below so we save it here */
        int nreq=require.size();
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        int count(0),nread(0);
        DataType d;
        while(inp.good())
        {
          try{
            d=inp.read();
            ++nread;
            if(!d.live) continue;   // Skip data marked dead
            pair<Metadata,MetadataIndex> extmp;
            extmp=ExtractAndRename(d,require);
            Metadata mdr(extmp.first);
            /* We use the size of the returned keys method to test if all the
            required attributes were loaded */
            MetadataList mdrk=mdr.keys();
            if(mdrk.size()!=nreq)
            {
              cerr << "Data from object number "<<nread
                 <<" is missing required Metadata and will be dropped"<<endl;
              continue;
            }
            MetadataIndex mdri(extmp.second);
            extmp=ExtractAndRename(d,optional);
            Metadata mdo(extmp.first);
            MetadataIndex mdoi(extmp.second);
            Metadata md=mdr+mdo;
            /* This is merging the two map containers - weird map container
            syntax I know.*/
            MetadataIndex mdi(mdri);
            MetadataIndex::iterator miptr;
            for(miptr=mdoi.begin();miptr!=mdoi.end();++miptr)
            {
              mdi[miptr->first]=miptr->second;
            }
            /* Add the BasicTimeSeries hard wired attributes with fixed names */
            md.put("ns",d.ns);
            mdi["ns"]=MDint;
            md.put("delta",d.dt);
            mdi["delta"]=MDreal;
            md.put("starttime",d.t0);
            mdi["starttime"]=MDreal;
            long foff;
            foff=save_sample_data<DataType>(d,dout);
            md.put("foff",foff);
            mdi["foff"]=MDint;  // Redundant but neglect for simplicity
            save_metadata(md,hdroutput,count);
            ++count;
          }catch(SeisppError& serr)
          {
            serr.log_error();
          }
        }
        return pair<int,int>(nread,count);
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string otype("ThreeComponentSeismogram");
    string pffile("export_to_mspass.pf");
    string outbase("MsPASSExport");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
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
          outbase=string(argv[i]);
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
        }
        else
            usage();
    }
    try{
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        PfStyleMetadata pf(pffile);
        NameMapper require=build_NameMapper(pf,string("required"));
        NameMapper optional=build_NameMapper(pf,string("optional"));
        /* Open the hdr file immediately, but open the sample data file alter to
        allow a change in file extension for 3c versus scalar data.*/
        string fname;
        fname=outbase+".yaml";  // yaml may be required for reading
        ofstream hdrostrm;
        hdrostrm.open(fname,std::ofstream::out);
        /* We need to do this immediately to be sure epoch times are not 
         * truncated */
        hdrostrm<<std::setprecision(15);
        ofstream dout;
        pair<int,int> count;
        /* dtype can have more valid values than this, but assum get_object_type
        aborts if they aren't supportd */
        switch (dtype)
        {
            case TCS:
                fname=outbase+".d3C";
                dout.open(fname,std::ofstream::out | std::ios::binary);
                count=export_to_mspass<ThreeComponentSeismogram>(hdrostrm,dout,
                  require,optional,binary_data);
                break;
            case TS:
                fname=outbase+".dat";
                dout.open(fname,std::ofstream::out | std::ios::binary);
                count=export_to_mspass<TimeSeries>(hdrostrm,dout,
                  require,optional,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "export_to_mspass:  processed "<<count.first
            <<" and converted "<<count.second<<" objects"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
