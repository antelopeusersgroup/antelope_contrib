#include <string>
#include <iostream>
#include <fstream>
#include "seispp.h"
#include "seispp_io.h"
using namespace std;
namespace SEISPP {
StreamObjectReader::StreamObjectReader(const char format)
{
  /* Note when this constructor is called ifs is not initialized and must
  not be touched. */
  input_is_stdio=true;
  nobjects=0;
  parent_filename="STDIN";
  n_previously_read=0;
  switch(format)
  {
    case 'b':
      bin_ar=ar=new boost::archive::binary_iarchive(std::cin);
      txt_ar=NULL;
      break;
    case 't':
    default:
      bin_ar=NULL;
      txt_ar=new boost::archive::text_iarchive(std::cin);
  };
}
StreamObjectReader::StreamObjectReader(string fname,const char format)
{
  try{
    const string base_error("StreamObjectReader file constructor:  ");
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
        ifs.read(&(this->nobjects),sizeof(long));
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
        bin_ar=ar=new boost::archive::binary_iarchive(ifs);
        txt_ar=NULL;
        break;
      case 't':
      default:
        bin_ar=NULL;
        txt_ar=new boost::archive::text_iarchive(ifs);
    };
  }catch(...){throw;};
}
StreamObjectReader::~StreamObjectReader()
{
  switch(format)
  {
    case 'b':
      delete bin_ar;
    case 't':
      delete txt_ar;
    default:
  };
  if(!input_is_stdio) ifs.close();
}
long StreamObjectReader::number_available()
{
    if(input_is_stdio)
        throw SeisppError(string("StreamObjectReader::number_available method:")
               + "  input is stdio - cannot determine file size from stdin");
    else
        return nobjects;
}
bool StreamObjectReader::eof()
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
void StreamObjectReader::rewind()
{
  if(input_is_stdio)
  {
    throw SeisppError(string("StreamObjectReader rewind method:  ")
      + "input is tied to stdin - rewind is not possible for stdin");
  }
  else
    ifs.seekg(0,ios_base::beg);
}
StreamObjectWriter::StreamObjectWriter(const char format)
{
  /* Note ofs is left invalid in this condition - stdin cannot seek
  which creates a disconnect */
  output_is_stdio=true;
  nobjects=0;
  parent_filename="STDOUT";
  /* This may not be necessary according to the documentation */
  ios::sync_with_stdio();
  switch(format)
  {
    case 'b':
      bin_ar=ar=new boost::archive::binary_oarchive(std::cout);
      txt_ar=NULL;
      break;
    case 't':
    default:
      bin_ar=NULL;
      txt_ar=new boost::archive::text_oarchive(std::cout);
  };
}
StreamObjectWriter::StreamObjectWriter(string fname,const char format)
{
  try{
    const string base_error("StreamObjectWriter file constructor:  ");
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
        bin_ar=ar=new boost::archive::binary_oarchive(ifs);
        txt_ar=NULL;
        break;
      case 't':
      default:
        bin_ar=NULL;
        txt_ar=new boost::archive::text_oarchive(ifs);
    };
  }catch(...){throw;};
}
StreamObjectWriter::~StreamObjectWriter()
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
        cout.write((char *)()&nobjects),sizeof(long));
      }
      else
      {
        ofs.write(eof_tag.c_str(),BINARY_TAG_SIZE);
        ofs.write((char *)()&nobjects),sizeof(long));
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
      for(i=0;i<TextIOStreamEOFOffset;++i)
      {
        if(output_is_stdio)
          cout<<buf[i];
        else
        {
          ofs<<buf[i];
          ofs.close();
          delete txt_ar;
        }
      }
      delete [] buf;
  };
}
} // End SEISPP namespace encapsulation
