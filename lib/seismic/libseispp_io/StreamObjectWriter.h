#ifndef _STREAM_OBJECT_WRITER_H_
#define _STREAM_OBJECT_WRITER_H_
#include "BasicObjectWriter.h"
#include "seispp_io.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
/*! \brief Generic object writer saving data in a sequential text file.

This object abstracts reading of objects based on two standard concepts:
(1) access is sequential as in reading from a tape, and (2) data are
serialized and written as ascii text.   The implementation uses the
boost text archive library, but the design of the interface is intended to
insulate the application from this implementation detail. */
template <class T> class StreamObjectWriter : public BasicObjectWriter<T>
{
  public:
    /*! \brief Default constructor.

      Default constructor uses boost text serialization to stdout*/
    StreamObjectWriter(const char format='t');
    /*! \brief Create handle to write to file.

      Creates an input handle to write to a file.
      \param file - file to open for output

      \exception - throws a SeisppError object if operation fails.  Boost
        constructors may also throw special error object (needs research).
        */
    StreamObjectWriter(string fname,const char format='t');
    /*! Destructor - has to close io channel */
     ~StreamObjectWriter();
    /*! \brief write one object.

    This is the primary method of this object.  Writes a single object to
    the output.  Assumes d has a serialization defined.

    \param d - object to be written
    */
    void write(T& d);
    long number_already_written(){return nobjects;};
  private:
    char format;
    boost::archive::text_oarchive *txt_ar;
    boost::archive::binary_oarchive *bin_ar;
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

template <class T> void StreamObjectWriter<T>::write(T& d)
{
    try {
      if(nobjects>0)
      {
        switch(this->format)
        {
          case 'b':
            if(output_is_stdio)
                cout.write(more_data_tag.c_str(),BINARY_TAG_SIZE);
            else
                ofs.write(more_data_tag.c_str(),BINARY_TAG_SIZE);
          case 't':
          default:
            if(output_is_stdio)
              cout<<more_data_tag<<endl;
            else
              ofs<<more_data_tag<<endl;
        };
      }
      switch(this->format)
      {
        case 'b':
          (*bin_ar)<<d;
          break;
        case 't':
        default:
          (*txt_ar)<<d;
      };
      ++nobjects;
    }catch(...)
    {
        throw SeisppError(string("StreamObjectWriter write method failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
}
template <class T> StreamObjectWriter<T>::StreamObjectWriter(const char form)
{
  /* Note ofs is left invalid in this condition - stdin cannot seek
  which creates a disconnect */
  format=form;
  output_is_stdio=true;
  nobjects=0;
  parent_filename="STDOUT";
  /* This may not be necessary according to the documentation */
  ios::sync_with_stdio();
  switch(format)
  {
    case 'b':
      bin_ar=new boost::archive::binary_oarchive(std::cout);
      txt_ar=NULL;
      break;
    case 't':
    default:
      bin_ar=NULL;
      txt_ar=new boost::archive::text_oarchive(std::cout);
  };
}
template <class T> StreamObjectWriter<T>::StreamObjectWriter(string fname,const char form)
{
  try{
    const string base_error("StreamObjectWriter file constructor:  ");
    format=form;
    switch(format)
    {
      case 'b':
        ofs.open(fname.c_str(),ios::out | ios::trunc);
        break;
      case 't':
      default:
        ofs.open(fname.c_str(),ios::out | ios::binary | ios::trunc);
    };
    if(ofs.fail())
    {
      throw SeisppError(base_error+"open failed on file "+fname+" for output");
    }
    parent_filename=fname;
    output_is_stdio=false;
    nobjects=0;
    ios::sync_with_stdio();
    switch(format)
    {
      case 'b':
        bin_ar=new boost::archive::binary_oarchive(ofs);
        txt_ar=NULL;
        break;
      case 't':
      default:
        bin_ar=NULL;
        txt_ar=new boost::archive::text_oarchive(ofs);
    };
  }catch(...){throw;};
}
template <class T> StreamObjectWriter<T>::~StreamObjectWriter()
{
  int i;
  char *buf;
  buf=new char [TextIOStreamEOFOffset];
  /* Initialize the buffer to all blanks */
  for(i=0;i<TextIOStreamEOFOffset;++i) buf[i]=' ';
  switch(format)
  {
    case 'b':
      if(output_is_stdio)
      {
        cout.write(eof_tag.c_str(),BINARY_TAG_SIZE);
        cout.write((char *)(&nobjects),sizeof(long));
      }
      else
      {
        ofs.write(eof_tag.c_str(),BINARY_TAG_SIZE);
        ofs.write((char *)(&nobjects),sizeof(long));
        ofs.close();
      }
      delete bin_ar;
      break;
    case 't':
    default:
      buf=new char [TextIOStreamEOFOffset];
      /* Initialize the buffer to all blanks */
      for(i=0;i<TextIOStreamEOFOffset;++i) buf[i]=' ';
      sprintf(buf,"%s %ld\n",eof_tag.c_str(),nobjects);
      if(output_is_stdio)
        for(i=0;i<TextIOStreamEOFOffset;++i) cout<<buf[i];
      else
      {
        for(i=0;i<TextIOStreamEOFOffset;++i) ofs<<buf[i];
      }
      ofs.close();
      delete txt_ar;
      delete [] buf;
  };
}
/*! \brief Generic algorithm to dismember an ensemble into components.
 *
 * Ensembles are defined in the seispp library as vectors of objects with
 * the reserved name "members".   Ensembles also have Metadata parameters
 * that are global.   This generic algorithm copes the global metadata to 
 * each member and writes the members to a stream output defined by 
 * the handle out.   Tens is the type of the ensemble and Tmem defines
 * the type of the members of that ensemble.
 *
 * \param d - input ensemble.
 * \param out - output handle (StreamObjectWriter template class)
 * */
template <class Tens,class Tmem> int write_ensemble(Tens& d,
        shared_ptr<StreamObjectWriter<Tmem>> out)
{
    try {
      int i;
      Metadata ensmd(dynamic_cast<Metadata&>(d));
      MetadataList keylist=ensmd.keys();
      for(i=0;i<d.member.size();++i)
      {
        Tmem dmem(d.member[i]);
        copy_selected_metadata(ensmd,dynamic_cast<Metadata&>(dmem),keylist);
        out->write(dmem);
      }
      return i;
    }catch(...){throw;}
}
}
#endif
