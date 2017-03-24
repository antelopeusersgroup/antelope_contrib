#include <string>
#include <iostream>
#include <fstream>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#ifndef _SEISPP_IO_H_
#define _SEISPP_IO_H_
namespace SEISPP{
using namespace std;
using namespace SEISPP;
/*! \brief Abstract base class for generic object reader.

This is the base class for a collection of C++ classes designed to
abstract and simplify the process of reading and writing of seispp data
objects.   The purpose of this base class is to provide the foundation for
a set of polymorphic objects for reading data in various generic forms.
*/
class BasicObjectReader
{
  public:
    /*! Pure virtual method used to allow an abstract base class.*/
    virtual bool good()=0;
    virtual long number_available()=0;
};
/*! \brief Abstract base class for generic object writer.

This is the base class for a collection of C++ classes designed to
abstract and simplify the process of reading and writing of seispp data
objects.   The purpose of this base class is to provide the foundation for
a set of polymorphic objects for saving data in varous forms.
*/
class BasicObjectWriter
{
  public:
    virtual long number_already_written()=0;
};
/*! We write the number of objects in set of concatenated serial objects
   at the end of the file.  We seek back this many bytes to read the
   number of objects written */
const int TextIOStreamEOFOffset(64);
/*! This string is written after each object when more data follows..*/
const string more_data_tag("MORE_DATA_FOLLOW");
/*! This is written at the end of the file immediately before text conversion
of nobject count */
const string eof_tag("END_OF_FILE");
/*! \brief Generic object reader to read serialized test data from a sequential file.

This object abstracts reading of objects based on two standard concepts:
(1) access is sequential as in reading from a tape, and (2) data are
serialized and written as ascii text.   The implementation uses the
boost text archive library, but the design of the interface is intended to
insulate the application from this implementation detail. */
class TextIOStreamReader : BasicObjectReader
{
  public:
    /*! \brief Default constructor.

      Default constructor uses boost text serialization to stdin*/
    TextIOStreamReader();
    /*! \brief Create handle to read from file.

      Creates an input handle to read from a file.
      \param fname - file name opened as ifs

      \exception - throws a SeisppError object if operation fails.  Boost
        constructors may also throw special error object (needs research).
        */
    TextIOStreamReader(string fname);
    /*! Standard copy constructor.  Seems impossible with boost text_iarchive*/
    //TextIOStreamReader(const TextIOStreamReader& parent);
    /*! Destructor - has to close io channel */
     ~TextIOStreamReader();
     /*! Standard assignment operator. Appears impossible for boost serialization*/
    //TextIOStreamReader& operator=(const TextIOStreamReader& parent);
    template <class T> T read();
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
    boost::archive::text_iarchive *ar;
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
/*! \brief Generic object writer saving data in a sequential text file.

This object abstracts reading of objects based on two standard concepts:
(1) access is sequential as in reading from a tape, and (2) data are
serialized and written as ascii text.   The implementation uses the
boost text archive library, but the design of the interface is intended to
insulate the application from this implementation detail. */
class TextIOStreamWriter : BasicObjectWriter
{
  public:
    /*! \brief Default constructor.

      Default constructor uses boost text serialization to stdout*/
    TextIOStreamWriter();
    /*! \brief Create handle to write to file.

      Creates an input handle to write to a file.
      \param file - file to open for output

      \exception - throws a SeisppError object if operation fails.  Boost
        constructors may also throw special error object (needs research).
        */
    TextIOStreamWriter(string fname);
    /*! Destructor - has to close io channel */
     ~TextIOStreamWriter();
    /*! \brief write one object.

    This is the primary method of this object.  Writes a single object to
    the output.  Assumes d has a serialization defined.

    \param d - object to be written
    */
    template <class T> void write(T& d);
    long number_already_written(){return nobjects;};
  private:
    boost::archive::text_oarchive *ar;
    /* input from stdio is special and is flagged by this boolean */
    bool output_is_stdio;
    /* This is the name of the file linked to ofs.*/
    string parent_filename;
    /* stream linked to ar */
    ofstream ofs;
    /* To support multiple objects in one serial file we need to
       count the number of objects written.   This number is written to
       the end of the file.   It is incremted by each write. */
    long nobjects;
};
template <class InputObject> InputObject TextIOStreamReader::read()
{
  const string base_error("TextIOStreamReader read method:  ");
  InputObject d;
  try{
    /* This little test is probably an unnecessary overhead, but the cost is
    tiny */
    if(!input_is_stdio)
      if(n_previously_read>=(nobjects-1)) throw SeisppError(base_error
        + "Trying to read past end of file - code should test for this condition with at_eof method");
    (*ar)>>d;
    ++n_previously_read;
    string tag;
    if(input_is_stdio)
      cin>>tag;
    else
      ifs>>tag;
    if(tag==more_data_tag)
      more_data_available=true;
    else if(tag==eof_tag)
      more_data_available=false;
    else
    {
      more_data_available=false;
      cerr << "TextIOStreamReader read method (WARNING): invalid end of data tag="
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
template <class OutputObject> void TextIOStreamWriter::write(OutputObject& d)
{
    try {
      if(nobjects>0)
      {
        if(output_is_stdio)
          cout<<more_data_tag<<endl;
        else
          ofs<<more_data_tag<<endl;
      }
      (*ar) << d;
      ++nobjects;
    }catch(...)
    {
        throw SeisppError(string("TextIOStreamWriter write method failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
}
/***** Should be able to add: (1) read/write binary sequential, (2) read/write
xml sequential, (3) forms of indexed files with keys generated seperately, and
(4) database access through a nonsql db */

/*! Legacy writer for archive connected to stdout - original seispp_filters */
template <class OutputObject> void write_object(OutputObject& d,
        boost::archive::text_oarchive& oa)
{
    try {
        oa << d;
    }catch(...)
    {
        throw SeisppError(string("write_object failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
}
/*! Legacy reader for archive connected to stdin- original seispp_filters */

template <class InputObject> InputObject
    read_object(boost::archive::text_iarchive& ia)
{
    InputObject d;
    try{
        ia>>d;
    }catch(...)
    {
        /* This template ignores errors thrown by boost
           and converts to a standard error for the seispp
           library - perhaps not ideal but what is done here. */
        throw SeisppError(string("read_object failed:  ")
                    + "Check that input file is a boost text archive file");
    }
    return d;
}
} // End namespace SEISPP declaration 
#endif
