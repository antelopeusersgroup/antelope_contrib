#ifndef _STREAM_OBJECT_INDEXER_H_
#define _STREAM_OBJECT_INDEXER_H_
#include <fstream>
#include <vector>
#include "seispp_io.h"
#include "Metadata.h"
#include "StreamObjectReader.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
template <typename Tdata> class StreamObjectFileIndexer
{
public:
  StreamObjectFileIndexer(string dfile,MetadataList mdl);
  int writeindex(const string fname,const string dfile);
private:
  vector<Metadata> index;
  vector<long> foff;
  string dfilename;
};
template <typename Tdata>
   StreamObjectFileIndexer<Tdata>::StreamObjectFileIndexer(string dfile,
     MetadataList mdl)
{
  try{
    StreamObjectReader<Tdata> dfh(dfile,'b');
    Tdata d;
    Metadata mdtmp;
    while(dfh.good())
    {
      d=dfh.read();
      long foffnow=dfh.foff();
      foff.push_back(foffnow);
      copy_selected_metadata(dynamic_cast<Metadata&>(d),mdtmp,mdl);
      index.push_back(mdtmp);
    }
  }catch(...){throw;};
}
template <typename Tdata> int StreamObjectFileIndexer<Tdata>::writeindex
                                (const string fname, const string dfile)
{
  try{
    string tname(typeid(Tdata).name());
    ofstream ofs;
    ofs.open(fname.c_str(),ios::out);
    if(ofs.fail())
    {
      throw SeisppError(string("StreamObjectFileIndexer writeindex method:  ")
          +"open filed on output index file="+fname);
    }
    boost::archive::text_oarchive ar(ofs);
    ofs<<foff.size()<<endl;
    ofs<<dfile<<endl;
    ofs<<tname<<endl;
    int i;
    for(i=0;i<foff.size();++i)
    {
      Metadata md(index[i]);
      md.put(OffsetKey,foff[i]);
      ar<<md;
    }
    return foff.size();
  }catch(...){throw;};
}
} // End SEISPP namespace
#endif
