#ifndef _INDEXED_OBJECT_READER_H_
#define _INDEXED_OBJECT_READER_H_
#include <iostream>
#include <fstream>
#include <memory>
#include "BasicObjectReader.h"
#include "seispp_io.h"
#include "StreamObjectFileIndex.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
/*! \brief Generic reader for a file of objects indexed by a set attributes.

The concept of this object is a generic random access reader of a data set.
The only assumption the reader makes is that an index is available that
defines the order of the data.   That index could be defined by a database
engine or a file scheme.  The implementation uses an index derived from
the Metadata of the parent file of objects.  Further, the parent objects
are assumed to be stored by boost::serialization of the original data.  The
index access individual objects with an foff specification.   

There is a related reader to this one called DataSetReader than can be used
to read multiple files sequentially.   

WARNING:   At the point of this writing at least in Redhat linux there is a
bug in how boost's serialization library interacts with std::ifstream.   
Reads fail if record 0 is read after any other record.  As a workaround
I created a rewind method that closes and recreates the handles for
the serialization when record 0 is requested after any other record.  
Unfortunately, that creates a different problem that we'll have to live
with for now.   Something about the shared pointers and boost's 
constructors for the serialization handle creates a seg fault when the
destructor in this object is called AFTER the rewind method is called.  
It will exit find if the file is read sequentially and it is not necessary
to call rewind.  
constructor to define the index */
template <typename Tdata>
    class IndexedObjectReader : BasicObjectReader<Tdata>
{
public:
  /*! Default constructor.  Sets all attributes null */
  //IndexedObjectReader();
  /*! \brief main constructor.

  This implementation builds the handle from a file containing the index
  data that is assumed to have been built previously.   The format of
  that file can be gleaned from the code and will likely evolve as this
  object is extended.

  \param indexfile is the file containing the index database
  */
  IndexedObjectReader(const string indexfile,const char form='b');
  /*! \brief Standard copy constructor.

  Copying is rational for this object because subset and sort operators
  demand the index be mutable.  */
  IndexedObjectReader(const IndexedObjectReader& parent);
  /*! Assignment operator.   Similar in concept in this case to copy constructor.*/
  IndexedObjectReader<Tdata>& operator=(const IndexedObjectReader<Tdata>& parent);
  /*! Destructor.  Nontrival as it needs to close data file. */
  ~IndexedObjectReader();
  /*! \brief Subset metod.

  A very common need is to reduce the size of a large data set by some criteria.
  This is a generic interface to do that using the C++ concept of function nobjects
  to implement the comparison in a generic way.   The algorithm used is
  the handle returne is reduced to only those elements for which the index
  metadata accessed by key satisfy the true clause of functor.

  \param key - metadata key used for comparison (must define a Tkey value)
  \param testvalue - value used for comparison
  \param - functor is the function object that should be used for comparison test.
     Simple arithmetic equal, gt, or lt are simple examples but it could be
     much more exotic (e.g. an epicentral distance calculator).

  This method leaves the parent unchanged but returns the reduced subsetted
  handle as a new object.   If reducing memory is appropriate one overwrite
  the original in an assignment statement. i.e. A=A.subset(args) should work.  */
  /*   This needs soem study to figure out how to do this syntax for templates.
  IndexedObjectReader<Tdata> 
      subset<class Tkey,class Compare>(string key,Tkey testvalue,Compare functor);
      */
  /*! \brief sort method.

  It is often necessary to sort the data to a new order.   A classic is example
  is a CMP sort from shot gather order in reflection processing.  There might
  be a way to do this interface with variable args to allow an arbitrary
  number of keys, but here we limit this to 3.   The number of sort keys is
  determined by the number of template args passed.   i.e. any key with a
  defined type will be used.

  I think the algorithm to use here is a staged sort on Metadata fields.
  i.e. sequence is:
    foreach key
       sort on key
       foreach unique key
           define range
           sort on range
           recursive call to sort through range
  */
  /*Temporarily disabled for intiial debug.
  void sort<class Tkey1=string,class Tkey2=void,class Tkey3=void,
             class Compare=less(),class Compare=less(),class Compare=less()>
     (string key1, string key2=string(), string key3=string());*/
  bool good()
  {
    return dfs->good();
  };
  long number_available()
  {
    return idx.foff.size();
  };
  bool eof()
  {
    if(last_object_read>=(number_objects-1))
       return true;
    else
       return false;
  };
  void rewind();
  Tdata read();
  Tdata read(int object_number);
  string filename(){return fname;};
private:
  /* For the present this can only be b but left in object for possible 
   * future extensions that add flexibility to the api*/
  char format;  
  /* The archive is constructed from this stream.  We use a shared_ptr to
  allow copying */
  std::shared_ptr<std::ifstream> dfs;
  std::shared_ptr<boost::archive::binary_iarchive> data_arptr;
  StreamObjectFileIndex<Tdata> idx;
  long last_object_read;
  /* this state variable is set on first read.   We need to make it illegal
  to sort after we start reading */
  bool sorting_allowed;
  /* We save the parent file name.   Useful for error messages and
   * perhaps other purposes for near zero cost */
  string fname;
  /* Read from index file - number of objects stored in fname */
  int number_objects;
};

template <typename Tdata>
  IndexedObjectReader<Tdata>::IndexedObjectReader(const string indexfile,const char form)
{
  try{
    const string base_error("IndexedObjectReader constructor:");
    this->format=form;
    /* Initialize these */
    last_object_read=-1;
    sorting_allowed=true;
    ifstream ifs;
    ifs.open(indexfile.c_str(),ios::in);
    if(ifs.fail())
    {
      throw SeisppError(base_error+"cannot open file "+indexfile+" for input");
    }
    /* These entries are in index data file, but we don't actually need to
    store them.*/

    ifs>>number_objects;
    ifs>>fname;
    string tname;
    ifs>>tname;
    if(typeid(Tdata).name() != tname)
    {
      throw SeisppError(base_error+"type mismatch in data file\n"
         + "Expected object type="+typeid(Tdata).name()
         + " but index given is to a file of objects of type="+tname);
    }
    /* Now we read in the index saved as a vector of Metadata objects */
    boost::archive::text_iarchive ia(ifs);
    int i;
    idx.index.reserve(number_objects);
    idx.foff.reserve(number_objects);
    for(i=0;i<number_objects;++i)
    {
      Metadata mdin;
      long thisoffset;
      ia>>mdin;
      idx.index.push_back(mdin);
      try{
        thisoffset=mdin.get<long>("foff");
        idx.foff.push_back(thisoffset);
      }catch(MetadataGetError& mde)
      {
        throw SeisppError(base_error+"Error in index data\nRequired attribute foff missing");
      }
    }
    ifs.close();
    dfs=shared_ptr<std::ifstream>(new std::ifstream);
    /* Note the format determines whether or not the data file is text or
    binary serialized data.   Note an internet source
    http://www.cplusplus.com/forum/beginner/186019/
    claims seekg to arbitrary positions is not supported in text mode for
    an istream.   If that proves true we should throw an error if the
    file demands text mode. */
    switch(format)
    {
      case 't':
        throw SeisppError(base_error
          + "Text format is not allowed for an indexed file in SEISPP\n"
            +"Convert parent file to "+ indexfile+"to binary");
        break;
      case 'b':
        dfs->open(fname.c_str(),ios::in | ios::binary);
        break;
      default:
         throw SeisppError(base_error + "Illegal format="+format
             +" was read from index file="+indexfile);
    };
    data_arptr=shared_ptr<boost::archive::binary_iarchive>
                (new boost::archive::binary_iarchive(*dfs));
  }catch(...){throw;};
}
template <typename Tdata>
  IndexedObjectReader<Tdata>::IndexedObjectReader(const IndexedObjectReader& parent)
{
  format=parent.format;
  dfs=parent.dfs;
  data_arptr=parent.data_arptr;
  idx=parent.idx;
  last_object_read=parent.last_object_read;
  sorting_allowed=parent.sorting_allowed;
  fname=parent.fname;
  number_objects=parent.number_objects;
}
/* This destructor will seg fault at present if the rewind method 
 * is called.  Something about shared_ptr implementation I do not 
 * get */
template <typename Tdata>
  IndexedObjectReader<Tdata>::~IndexedObjectReader()
{
    if(dfs.use_count()<=1) dfs->close();
}
template <typename Tdata>
  IndexedObjectReader<Tdata>& IndexedObjectReader<Tdata>::operator=(const IndexedObjectReader& parent)
{
  if(this!=(&parent))
  {
    format=parent.format;
    dfs=parent.dfs;
    data_arptr=parent.data_arptr;
    idx=parent.idx;
    last_object_read=parent.last_object_read;
    sorting_allowed=parent.sorting_allowed;
    fname=parent.fname;
    number_objects=parent.number_objects;
  }
  return *this;
}
template <typename Tdata>
        Tdata IndexedObjectReader<Tdata>::read()
{
  const string base_error("IndexedObjectReader read method:  ");
  Tdata d;
  try{
    if(last_object_read<0)
    {
      last_object_read=0;
    }
    else
    {
      ++last_object_read;
    }
    if(last_object_read >= this->idx.foff.size())
    {
      throw SeisppError(base_error + "Attempt to read past end of data set");
    }
    long offset=this->idx.foff[last_object_read];
    dfs->seekg(offset,ios::beg);
    if(dfs->bad())
    {
      throw SeisppError(base_error+"seekg failure");
    }
    //DEBUG
    //cerr << "read() method - current file position="<<dfs->tellg()<<endl;
    *(data_arptr)>>d;
    return d;
  }catch(...){throw;};
}

template <typename Tdata>
        Tdata IndexedObjectReader<Tdata>::read(int onum)
{
  const string base_error("IndexedObjectReader read for specified object number:  ");
  Tdata d;
  try{
    if(onum<0)
    {
      throw SeisppError(base_error + "object number passed is negative - likely coding error");
    }
    if(onum >= this->idx.foff.size())
    {
      throw SeisppError(base_error + "Requested object number past end of data set");
    }
    /* This conditional is necessary to handle a bug in how boost serialization
     * currently interacts with istream.   This conditionis a subset of 
     * the concept of rewind so it is handled with rewind that deals with
     * the bug.   This section can probably be left when the bug is resolved. 
     */
    if( (last_object_read>0) && (onum==0) )
    {
      this->rewind();
    }
    else
    {
      long offset=this->idx.foff[onum];
      dfs->seekg(offset,ios::beg);
      if(!dfs->good())
      {
        throw SeisppError(base_error+"seekg failure");
      }
    }
    //DEBUG
    //cerr << "File position for object number "<<onum<<" is "<<dfs->tellg()<<endl;
    *(data_arptr)>>d;
    last_object_read=onum;
    return d;
  }catch(...){throw;};
}
/* This is a workaround for a mysterious bug in boost serialization
     * I have been unable to figure out.   When accessing the first object
     * after reading others boost throws an alloc error deep in the code.
     * Since this is the concept of rewind it is convenient to put it here.
     * If that bug gets resolved this method could be much simpler */
template<typename Tdata> void IndexedObjectReader<Tdata>::rewind()
{
  try{
    dfs->close();
    dfs->open(fname.c_str(),ios::in | ios::binary);
    boost::archive::binary_iarchive *ptr;
    ptr=new boost::archive::binary_iarchive(*dfs);
    /* This should make the old archive go out of scope and be deleted.
     * Have not checked so this could be a memory leak */
    data_arptr.reset(ptr);
    last_object_read=-1;
  }catch(...){throw;};
}
/*
template <typename Tdata,typename Tkey, typename Compare>
IndexedObjectReader<Tdata> IndexedObjectReader::subset<class Tkey,class Compare>
               (string key,Tkey testvalue,Compare comp)
{
    try{
      int i;
      int n=index.size();
      IndexObjectReader<Tdata> ssreader(*this);
      ssreader.idx.foff.clear():
      ssreader.idx.index.clear();
      for(i=0;i<n;++i)
      {
        Tkey val;
        val=index.get<Tkey>(key);
        if(testvalue,val)
        {
          ssreader.foff.push_back(this->foff[i]);
          ssreader.index.push_back(this->index[i]);
        }
      }
      return ssreader;
    } catch(...){throw;};
}
*/
} // End namespace encapsulation
#endif
