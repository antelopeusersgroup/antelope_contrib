#ifndef _STREAM_OBJECT_INDEX_H_
#define _STREAM_OBJECT_INDEX_H_
#include <fstream>
#include <vector>
#include "seispp_io.h"
#include "Metadata.h"
#include "StreamObjectReader.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
const string IndexFileExtension("idx");
const string FileOffsetKey("foff");

template <typename Tdata> class StreamObjectFileIndex
{
public:
  vector<Metadata> index;
  vector<long> foff;
  StreamObjectFileIndex();
  StreamObjectFileIndex(string dfile,MetadataList mdl);
  StreamObjectFileIndex(const StreamObjectFileIndex& parent);
  /*! Save the index as the root file name with a fixed extension.

    Common practice is to link related files with an extension, meaning
    a set of characters after a period in the file name.   This constructor
    takes the base file name and adds an extension defined in this include
    file (idx).  
    */
  int writeindex();
  int writeindex(const string fname);
  int writeindex(ofstream& ofs);
  int index_size()
  {
    return ndata;
  };
  StreamObjectFileIndex& operator=(const StreamObjectFileIndex& parent);
private:
  int ndata;
  string dfilename;
};
template <typename Tdata>
   StreamObjectFileIndex<Tdata>::StreamObjectFileIndex()
{
    ndata=0;
    dfilename=string("");
    index.reserve(0);
    foff.reserve(0);
}
template <typename Tdata>
   StreamObjectFileIndex<Tdata>::StreamObjectFileIndex(string dfile,
     MetadataList mdl) : dfilename(dfile)
{
  try{
    StreamObjectReader<Tdata> dfh(dfile,'b');
    Tdata d;
    Metadata mdtmp;
    long foffnow;
    foffnow=dfh.foff();
    foff.push_back(foffnow);
    while(dfh.good())
    {
      d=dfh.read();
      foffnow=dfh.foff();
      foff.push_back(foffnow);
      copy_selected_metadata(dynamic_cast<Metadata&>(d),mdtmp,mdl);
      index.push_back(mdtmp);
    }
    foff.pop_back();   //needed because last entry is eof
    ndata=foff.size();
  }catch(...){throw;};
}
template <typename Tdata>StreamObjectFileIndex<Tdata>::StreamObjectFileIndex
   (const StreamObjectFileIndex& parent) : index(parent.index),foff(parent.foff),
                      dfilename(parent.dfilename)
{
  ndata=parent.ndata;
}
template <typename Tdata>
   int StreamObjectFileIndex<Tdata>::writeindex()
{
  try{
    string indxfname;
    std::size_t pos=dfilename.rfind('.');
    /* if period is not found just set the result to dfilename.
       Otherwise we use substr to extract what we need */
    if(pos == std::string::npos)
      indxfname=dfilename+IndexFileExtension;
    else
      // +1 because of start at 0 in C
      indxfname=dfilename.substr(0,pos+1) + IndexFileExtension;
    int count;
    count=this->writeindex(indxfname);
    return count;
  }catch(...){throw;};
}
template <typename Tdata>
   int StreamObjectFileIndex<Tdata>::writeindex(const string fname)
{
  try{
    ofstream ofs;
    ofs.open(fname.c_str(),ios::out);
    if(ofs.fail())
    {
      throw SeisppError(string("StreamObjectFileIndex writeindex method:  ")
          +"open filed on output index file="+fname);
    }
    int count;
    count=this->writeindex(ofs);
    ofs.close();
    return count;
  }catch(...){throw;};
}

template <typename Tdata>
   int StreamObjectFileIndex<Tdata>::writeindex(ofstream& ofs)
{
  try{
    string tname(typeid(Tdata).name());
    ofs<<foff.size()<<endl;
    ofs<<dfilename<<endl;
    ofs<<tname<<endl;
    int i;
    /* a sanity check */
    if(foff.size()!=ndata)
    {
      throw SeisppError(string("StreamObjectFileIndex::writeindex method:")
           + "Coding error.\nInternally stored number of objects does not match actual vector size");
    }
    boost::archive::text_oarchive ar(ofs);
    for(i=0;i<ndata;++i)
    {
      Metadata md(index[i]);
      md.put(FileOffsetKey,foff[i]);
      ar<<md;
    }
    return ndata;
  }catch(...){throw;};
}
} // End SEISPP namespace
#endif
