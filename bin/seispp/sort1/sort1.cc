#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <map>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "sort1 key [-i -binary --help] < in > out"
        <<endl
        << "  key is the metadata sort key to use"<<endl
        << "  -i to treat key as int (default is string)"<<endl
        << "WARNING:  this is a pure memory sort so do not use on large files"
        <<endl;
    exit(-1);
}
/* We need this typedef here to reduce ugly iterator syntax.  */
typedef vector<ThreeComponentSeismogram> SeisVector;
typedef vector<ThreeComponentSeismogram>::iterator SeisIterator;
typedef list<SeisIterator> SortedDataList;
shared_ptr<SeisVector> load_data
    (shared_ptr<StreamObjectReader<ThreeComponentSeismogram>> ia)
{
  shared_ptr<SeisVector> dptr(new SeisVector);
  int nseis(0);
  ThreeComponentSeismogram d;
  try{
    while(!ia->eof())
    {
      d=ia->read();
      dptr->push_back(d);
      ++nseis;
    }
    return dptr;
  }catch(...){throw;};
}
SortedDataList int_metadata_sort(SeisVector& d,string key)
{
  multimap<int,SeisIterator> xref;
  vector<ThreeComponentSeismogram>::iterator dptr;
  int i;
  for(dptr=d.begin(),i=0;dptr!=d.end();++dptr,++i)
  {
    try{
      int ival;
      ival=dptr->get_int(key);
      xref.insert(pair<int,SeisIterator>(ival,dptr));
    }catch(SeisppError& serr)
    {
      cerr << "Missing required integer metadata for key="<<key<<endl
        << "Error encountered on the "<<i<<"th seismogram of input file"<<endl
        << "This is the corresponding message from the SeisppError object"<<endl;
      serr.log_error();
      cerr << "This is a fatal error - cannot sort unless every seismogram has"
          << " the key "<<" defined"<<endl;
    }

  }
  /* this section could and should be made into a template and used
  for both of these procedures */
  SortedDataList result;
  multimap<int,SeisIterator>::iterator mptr;
  for(mptr=xref.begin();mptr!=xref.end();++mptr)
  {
      result.push_back(mptr->second);
  }
  return result;
}
SortedDataList string_metadata_sort(SeisVector& d,string key)
{
  SortedDataList result;
  multimap<string,SeisIterator> xref;
  vector<ThreeComponentSeismogram>::iterator dptr;
  int i;
  for(dptr=d.begin(),i=0;dptr!=d.end();++dptr,++i)
  {
    try{
      string sval;
      sval=dptr->get_string(key);
      xref.insert(pair<string,SeisIterator>(sval,dptr));
    }catch(SeisppError& serr)
    {
      cerr << "Missing required string metadata for key="<<key<<endl
        << "Error encountered on the "<<i<<"th seismogram of input file"<<endl
        << "This is the corresponding message from the SeisppError object"<<endl;
      serr.log_error();
      cerr << "This is a fatal error - cannot sort unless every seismogram has"
          << " the key "<<" defined"<<endl;
    }
  }
  multimap<string,SeisIterator>::iterator mptr;
  for(mptr=xref.begin();mptr!=xref.end();++mptr)
  {
      result.push_back(mptr->second);
  }
  return result;
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    if(argc<2) usage();
    string key(argv[1]);
    bool key_is_int(false);
    bool binary_data(false);
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-i")
            key_is_int=true;
        else if(sarg=="-binary")
            binary_data=true;
        else if(sarg=="--help")
            usage();
        else
            usage();
    }
    try{
      shared_ptr<StreamObjectReader<ThreeComponentSeismogram>> ia;
      if(binary_data)
      {
        ia=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
           (new StreamObjectReader<ThreeComponentSeismogram>('b'));
      }
      else
      {
        ia=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
           (new StreamObjectReader<ThreeComponentSeismogram>);
      }
      shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>> oa;
      if(binary_data)
      {
        oa=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
           (new StreamObjectWriter<ThreeComponentSeismogram>('b'));
      }
      else
      {
        oa=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
           (new StreamObjectWriter<ThreeComponentSeismogram>);
      }
        /* The basic algorithm here is to eat up the full file of
        3c objects into a single ensemble object.   The multimap is
        filled by setting the key field to the extracted
        metadata key value and the int field is set to the vector
        index of each member.   The multimap intrinsically sorts to
        weak order so the output can then be produced by using iterators
        on the multimap. */
        shared_ptr<SeisVector> d = load_data(ia);
        /* Each of the two procedures below return this list is the
        sequence of iterators used to build the output */
        SortedDataList outlist;
        if(key_is_int)
          outlist=int_metadata_sort(*d,key);
        else
          outlist=string_metadata_sort(*d,key);
        /* Write the results - this perhaps should be a procedure, but
        was not sure how the list of iterators would work across a
        call*/
        SortedDataList::iterator optr;
        for(optr=outlist.begin();optr!=outlist.end();++optr)
        {
          oa->write(*(*optr));
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
