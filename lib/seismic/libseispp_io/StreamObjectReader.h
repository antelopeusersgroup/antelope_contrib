#ifndef _STREAM_OBJECT_READER_H_
#define _STREAM_OBJECT_READER_H_
#include "BasicObjectReader.h"
#include "seispp_io.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
/*! \brief Generic object reader to read serialized test data from a sequential file.

This object abstracts reading of objects based on two standard concepts:
(1) access is sequential as in reading from a tape, and (2) data are
serialized and written as ascii text.   The implementation uses the
boost text archive library, but the design of the interface is intended to
insulate the application from this implementation detail. */

template <typename T>
    class StreamObjectReader : public BasicObjectReader<T>
{
  public:
    /*! \brief Default constructor.

      Default constructor uses boost text serialization to stdin*/
    StreamObjectReader(const char format='t');
    /*! \brief Create handle to read from file.

      Creates an input handle to read from a file.
      \param fname - file name opened as ifs

      \exception - throws a SeisppError object if operation fails.  Boost
        constructors may also throw special error object (needs research).
        */
    StreamObjectReader(const string fname,const char format='t');
    /*! Destructor - has to close io channel */
     ~StreamObjectReader();
     /*! Read the next object in file. */
     T read();
    /*! Returns number of objects in the file being read. */
    long number_available();
    /*! \brief Return the number of objects already read.

    In loops going through a sequential file it can be useful to know the
    position.   This effectively returns a count of position as an integer
    of how many objects are read.   Since most objects are not a fixed size
    this is not directly linked to file position.  It is of use in a loop
    of for information on how a job is progressing. */
    long number_already_read(){return n_previously_read;};
    /*! \brief Test if ok to read more.

    This method can be used to drive a while loop.  Returns true as long
    as the count of object read is less than the number in the file */
    bool good(){return more_data_available;};
    /*! Test for end of file condition.  */
    bool eof();
    /*! \brief position to beginning of file.

    Since a serial file is basd on the concept of sequential access it is
    useful to have the concept of rewind as in a tape. */
    void rewind();
  private:
    char format;
    boost::archive::text_iarchive *txt_ar;
    boost::archive::binary_iarchive *bin_ar;
    /* input from stdio is special and is flagged by this boolean */
    bool input_is_stdio;
    /* This string defines the parent filename opened as ifstream ifs*/
    string parent_filename;
    /* stream linked to ar */
    ifstream ifs;
    /* To support multiple objects in one serial file we need to
       cache the number of objects expected and the number already
       read */
    long nobjects;
    long n_previously_read;
    /* When reading from stdin this boolean is set based on the
    tag string written at the end of each object.  Set true as long
    as the tag is not the eof_tag. */
    bool more_data_available;
};
template <typename T>
        T StreamObjectReader<T>::read()
{
  const string base_error("StreamObjectReader read method:  ");
  T d;
  try{
    /* This little test is probably an unnecessary overhead, but the cost is
    tiny */
    if(!input_is_stdio)
      if(n_previously_read>=(nobjects-1)) throw SeisppError(base_error
        + "Trying to read past end of file - code should test for this condition with at_eof method");
    string tag;
    char tagbuf[BINARY_TAG_SIZE+1];
    switch(this->format)
    {
      case 'b':
        (*bin_ar)>>d;
        if(input_is_stdio)
            cin.read(tagbuf,BINARY_TAG_SIZE);
        else
            ifs.read(tagbuf,BINARY_TAG_SIZE);
        tagbuf[BINARY_TAG_SIZE]='\0';
        tag=string(tagbuf);
        break;
      case 't':
      default:
        (*txt_ar)>>d;
        if(input_is_stdio)
          cin>>tag;
        else
          ifs>>tag;
    };
    ++n_previously_read;
    if(tag==more_data_tag)
      more_data_available=true;
    else if(tag==eof_tag)
      more_data_available=false;
    else
    {
      more_data_available=false;
      cerr << "StreamObjectReader read method (WARNING): invalid end of data tag="
        << tag<<endl
        << "Read may be truncated"<<endl
        << "Number of objects read so far="<<n_previously_read<<endl;
    }
    return d;
  }catch(...)
  {
    throw SeisppError(base_error
      + "boost text serialization read failed\nCheck that input is a valid boost text serialization file");
  }
}
template <typename T>
      StreamObjectReader<T>::StreamObjectReader(const char form)
{
  /* Note when this constructor is called ifs is not initialized and must
  not be touched. */
  format=form;
  input_is_stdio=true;
  nobjects=0;
  parent_filename="STDIN";
  n_previously_read=0;
  switch(format)
  {
    case 'b':
      bin_ar=new boost::archive::binary_iarchive(std::cin);
      txt_ar=NULL;
      break;
    case 't':
    default:
      bin_ar=NULL;
      txt_ar=new boost::archive::text_iarchive(std::cin);
  };
  more_data_available=true;
}
template <typename T> 
   StreamObjectReader<T>::StreamObjectReader(string fname,const char form)
{
  try{
    const string base_error("StreamObjectReader file constructor:  ");
    format=form;
    switch(format)
    {
      case 'b':
        ifs.open(fname.c_str(),ios::in | ios::binary);
        break;
      case 't':
      default:
        ifs.open(fname.c_str(),ios::in);
    };
    if(ifs.fail())
    {
      throw SeisppError(base_error+"cannot open file "+fname+" for input");
    }
    parent_filename=fname;
    input_is_stdio=false;
    n_previously_read=0;
    string magic_test;
    char tagbuf[BINARY_TAG_SIZE];
    switch(format)
    {
      case 'b':
        ifs.seekg(ifs.end-BinaryIOStreamEOFOffset);
        ifs.read(tagbuf,BINARY_TAG_SIZE);
        magic_test=string(tagbuf);
        ifs.read((char*)(&(this->nobjects)),sizeof(long));
        break;
      case 't':
      default:
        ifs.seekg(ifs.end-TextIOStreamEOFOffset);
        ifs >> magic_test;
        ifs >> nobjects;
    };
    if(magic_test!=eof_tag) throw SeisppError(base_error + "File "
        + fname + " does not appear to be a valid seispp boost serialization file");
    this->rewind();
    switch(format)
    {
      case 'b':
        bin_ar=new boost::archive::binary_iarchive(ifs);
        txt_ar=NULL;
        break;
      case 't':
      default:
        bin_ar=NULL;
        txt_ar=new boost::archive::text_iarchive(ifs);
    };
  }catch(...){throw;};
}
template <typename T>
   StreamObjectReader<T>::~StreamObjectReader()
{
  switch(format)
  {
    case 'b':
      delete bin_ar;
      break;
    case 't':
    default:
      delete txt_ar;
  };
  if(!input_is_stdio) ifs.close();
}
template <typename T>
   long StreamObjectReader<T>::number_available()
{
    if(input_is_stdio)
        throw SeisppError(string("StreamObjectReader::number_available method:")
               + "  input is stdio - cannot determine file size from stdin");
    else
        return nobjects;
}
template <typename T>
     bool StreamObjectReader<T>::eof()
{
  if(input_is_stdio)
  {
    if(more_data_available)
      return false;
    else
      return true;
  }
  else
  {
    /* We could use the same test as for stdin, but is a good integrity test.
    Makes the reader less robust, but more reliable in retrieving valid data*/
    if(n_previously_read>=nobjects)
      return true;
    else
      return false;
  }
}
template <typename T>
    void StreamObjectReader<T>::rewind()
{
  if(input_is_stdio)
  {
    throw SeisppError(string("StreamObjectReader rewind method:  ")
      + "input is tied to stdin - rewind is not possible for stdin");
  }
  else
    ifs.seekg(0,ios_base::beg);
}

}
template <typename T>
StreamObjectReader<T>* BuildReadHandle(string fname, bool binary_mode, bool use_stdin)
{
    try{
        StreamObjectReader<T> *handle;
        if(use_stdin)
        {
            if(binary_mode)
            {
                handle=new StreamObjectReader<T>('b');
            }
            else
            {
                handle=new StreamObjectReader<T>('t');
            }
        }
        else
        {
            if(binary_mode)
            {
                handle=new StreamObjectReader<T>(fname.c_str(),'b');
            }
            else
            {
                handle=new StreamObjectReader<T>(fname.c_str(),'t');
            }
        }
        return handle;
    }catch(...){throw;}
}
#endif
