#ifndef _INDEXED_OBJECT_READER_H_
#define _INDEXED_OBJECT_READER_H_
#include "BasicObjectReader.h"
#include "seispp_io.h"
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
index access individual objects with an foff specification.   For now we
assume this is only one file to mesh with modern HPC systems that do not like
any more files than necessary.   The intent is that the interface is Generic
enough to allow alternative implementations - notably with a database
constructor to define the index */
template <typename Tdata>
    class IndexedObjectReader : BasicObjectReader<Tdata>
{
public:
  /*! Default constructor.  Sets all attributes null */
  IndexedObjectReader();
  /*! \brief main constructor.

  This implementation builds the handle from a file containing the index
  data that is assumed to have been built previously.   The format of
  that file can be gleaned from the code and will likely evolve as this
  object is extended.

  \param indexfile is the file containing the index database
  \param clustermode is a placeholder.   One expected extension of this
      concept is to develop a foreman daemon that would tell multiple
      processes which object to read next.   Until implemented if
      set true the constructor will throw an exception. */
  IndexedObjectReader(const string indexfile,bool clustermode=false);
  /*! \brief Standard copy constructor.

  Copying is rational for this object because subset and sort operators
  demand the index be mutable.  */
  IndexedObjectReader(const IndexedObjectReader& parent);
  /*! Assignment operator.   Similar in concept in this case to copy constructor.*/
  IndexedObjectReader<Tdata>& operator=(const IndexedObjectReader& parent);
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
  IndexedObjectReader<Tdata> subset<class Tkey,class Compare>(string key,Tkey testvalue,Compare functor);
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
    return foff.size();
  };
  bool eof()
  {
    if(last_object_read>=foff.size())
       return true;
    else
       return false;
  };
  T read();
  T read(int object_number);
private:
  /* The archive is constructed from this stream.  We use a shared_ptr to
  allow copying */
  shared_ptr<ifstream> dfs;
  shared_ptr<boost::archive::binary_iarchive> data_arptr;
  /* This vector contains the data index and MUST contain long int attribute
  defined by OffsetKey.  */
  vector<Metadata> index;
  /* The foff data is so critical we cache it */
  vector<long> foff;
  bool cluster_mode;
  /* This is made a pointer to reduce overhead when not running
  in cluster mode.   Perhaps should be a shared_ptr.  Actually for initial
  implementation we comment it out and throw an error if we attempt
  construction in cluster mode */
  //Foreman *ClusterController;
  long last_object_read;
  /* this state variable is set on first read.   We need to make it illegal
  to sort after we start reading */
  bool sorting_allowed;
};

template <typename Tdata> IndexedObjectReader<Tdata>::IndexedObjectReader(const string indexfile,
   bool cm=false)
{
  try{
    const string base_error("IndexedObjectReader constructor:");
    this->format=form;
    this->clustermode=cm;
    /* Initialize these */
    last_object_read=-1;
    sorting_allowed=true;
    if(clustermode)
    {
      throw SeisppError(base_error+"clustermode not yet supported");
    }
    ifstream ifs;
    ifs.open(indexfile.c_str(),ios::in);
    if(this->ifs.fail())
    {
      throw SeisppError(base_error+"cannot open file "+fname+" for input");
    }
    /* These entries are in index data file, but we don't actually need to
    store them.*/
    int number_object;
    string object_file_name;
    ifs>>number_objects;
    ifs>>object_file_name;
    string tname;
    ifs>>tname;
    type_info tinfo=typeid(Tdata);
    if(tinfo.name()!=tname)
    {
      throw SeisppError(base_error+"type mismatch in data file\n"
         + "Expected object type="+tinfo.name()
         + " but index given is to a file of objects of type="tname);
    }
    /* Now we read in the index saved as a vector of Metadata objects */
    boost::archive::text_iarchive ia(ifs);
    int i;
    mdin.reserve(number_objects);
    foff.reserve(number_objects);
    for(i=0;i<number_objects;++i)
    {
      Metadata mdin;
      long thisoffset;
      ia>>mdin;
      index.push_back(mdin);
      try{
        thisoffset=dmin.get<long>("foff");
        foff.push_back(thisoffset);
      }catch(MetdataGetError& mde)
      {
        throw SeisppError(base_error+"Error in index data\nRequired attribute foff missing");
      }
    }
    ifs.close();
    dfs=shared_ptr<ifstream&>(new ifstream);
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
        dfs->open(object_file_name.c_str(),ios::in | ios::binary);
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
    if(last_object_read>=foff.size())
    {
      throw SeisppError(base_error + "Attempt to read past end of data set");
    }
    long offset=this->foff(last_object_read);
    dfs->seekg(offset);
    if(dfs->bad())
    {
      throw SeisppError(base_error+"seekg failure");
    }
    switch(format)
    {
      case 't':
        (*txt_ar)>>d;
        break;
      case 'b':
      default:
        (*bin_ar)>>d;
    };
    return d;
  }catch(...){throw;};
}

template <typename Tdata>
        Tdata IndexedObjectReader<Tdata>::read(int onum)
{
  const string base_error("IndexedObjectReader read for specified object number:  ");
  T d;
  try{
    if(onum<0)
    {
      throw SeisppError(base_error + "object number passed is negative - likely coding error");
    }
    if(onum>=foff.size())
    {
      throw SeisppError(base_error + "Requested object number past end of data set");
    }
    last_object_read=onum;
    long offset=this->foff(last_object_read);
    dfs->seekg(offset);
    if(dfs->bad())
    {
      throw SeisppError(base_error+"seekg failure");
    }
    switch(format)
    {
      case 't':
        (*txt_ar)>>d;
        break;
      case 'b':
      default:
        (*bin_ar)>>d;
    };
    return d;
  }catch(...){throw;};
}
template <typename Tdata>
IndexedObjectReader<Tdata> IndexedObjectReader::subset<class Tkey,class Compare>
               (string key,Tkey testvalue,Compare comp)
{
    try{
      int i;
      int n=index.size();
      IndexObjectReader<Tdata> ssreader(*this);
      ssreader.foff.clear():
      ssreader.index.clear();
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
} // End namespace encapsulation
#endif
