#include <string>
#include <iostream>
#include <fstream>
#include "seispp.h"
#include "seispp_io.h"
using namespace std;
namespace SEISPP {
TextIOStreamReader::TextIOStreamReader()
{
  /* Note when this constructor is called ifs is not initialized and must
  not be touched. */
  input_is_stdio=true;
  /* Only one object is allowed for stdin*/
  nobjects=1;
  parent_filename="STDIN";
  n_previously_read=0;
  ar=new boost::archive::text_iarchive(std::cin);
}
TextIOStreamReader::TextIOStreamReader(string fname)
{
  try{
    const string base_error("TextIOStreamReader file constructor:  ");
    ifs.open(fname.c_str(),ios::in);
    if(ifs.fail())
    {
      throw SeisppError(base_error+"cannot open file "+fname+" for input");
    }
    parent_filename=fname;
    input_is_stdio=false;
    n_previously_read=0;
    ifs.seekg(ifs.end-TextIOStreamEOFOffset);
    string magic_test;
    ifs >> magic_test;
    if(magic_test!=eof_tag) throw SeisppError(base_error + "File "
        + fname + " does not appear to be a valid seispp boost serialization file");
    ifs >> nobjects;
    this->rewind();
    ar=new boost::archive::text_iarchive(ifs);
  }catch(...){throw;};
}
TextIOStreamReader::~TextIOStreamReader()
{
  delete ar;
  if(!input_is_stdio) ifs.close();
}
long TextIOStreamReader::number_available()
{
    if(input_is_stdio)
        throw SeisppError(string("TextIOStreamReader::number_available method:")
               + "  input is stdio - cannot determine file size from stdin");
    else
        return nobjects;
}
bool TextIOStreamReader::eof()
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
void TextIOStreamReader::rewind()
{
  if(input_is_stdio)
  {
    throw SeisppError(string("TextIOStreamReader rewind method:  ")
      + "input is tied to stdin - rewind is not possible for stdin");
  }
  else
    ifs.seekg(0,ios_base::beg);
}
TextIOStreamWriter::TextIOStreamWriter()
{
  /* Note ofs is left invalid in this condition - stdin cannot seek
  which creates a disconnect */
  output_is_stdio=true;
  nobjects=0;
  parent_filename="STDIN";
  /* This may not be necessary according to the documentation */
  ios::sync_with_stdio();
  ar=new boost::archive::text_oarchive(std::cout);
}
TextIOStreamWriter::TextIOStreamWriter(string fname)
{
  try{
    const string base_error("TextIOStreamWriter file constructor:  ");
    ofs.open(fname.c_str(),ios::out | ios::trunc);
    if(ofs.fail())
    {
      throw SeisppError(base_error+"open failed on file "+fname+" for output");
    }
    parent_filename=fname;
    output_is_stdio=false;
    nobjects=0;
    ios::sync_with_stdio();
    ar=new boost::archive::text_oarchive(ofs);
  }catch(...){throw;};
}
TextIOStreamWriter::~TextIOStreamWriter()
{
  int i;
  char *buf=new char [TextIOStreamEOFOffset];
  /* Initialize the buffer to all blanks */
  for(i=0;i<TextIOStreamEOFOffset;++i) buf[i]=' ';
  sprintf(buf,"%s %ld\n",eof_tag.c_str(),nobjects);
  for(i=0;i<TextIOStreamEOFOffset;++i)
  {
    if(output_is_stdio)
      cout<<buf[i];
    else
      ofs<<buf[i];
  }
  delete [] buf;
  ofs.close();
  delete ar;
}
BinaryIOStreamReader::BinaryIOStreamReader()
{
  /* Note when this constructor is called ifs is not initialized and must
  not be touched. */
  input_is_stdio=true;
  /* Only one object is allowed for stdin*/
  nobjects=1;
  parent_filename="STDIN";
  n_previously_read=0;
  ar=new boost::archive::binary_iarchive(std::cin);
}
BinaryIOStreamReader::BinaryIOStreamReader(string fname)
{
  try{
    const string base_error("BinaryIOStreamReader file constructor:  ");
    ifs.open(fname.c_str(),ios::in|ios::binary);
    if(ifs.fail())
    {
      throw SeisppError(base_error+"cannot open file "+fname+" for input");
    }
    parent_filename=fname;
    input_is_stdio=false;
    n_previously_read=0;
    ifs.seekg(ifs.end-BinaryIOStreamEOFOffset);
    char magic_test[BINARY_TAG_SIZE];
    ifs.read(magic_test,BINARY_TAG_SIZE);
    if(!strncmp(magic_test,binary_eof_tag,BINARY_TAG_SIZE))
       throw SeisppError(base_error + "File "
        + fname + " does not appear to be a valid seispp boost serialization file");
    ifs.read((char*)(&nobjects),sizeof(long));
    this->rewind();
    ar=new boost::archive::binary_iarchive(ifs);
  }catch(...){throw;};
}
BinaryIOStreamReader::~BinaryIOStreamReader()
{
  delete ar;
  if(!input_is_stdio) ifs.close();
}
long BinaryIOStreamReader::number_available()
{
    if(input_is_stdio)
        throw SeisppError(string("BinaryIOStreamReader::number_available method:")
               + "  input is stdio - cannot determine file size from stdin");
    else
        return nobjects;
}
bool BinaryIOStreamReader::eof()
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
void BinaryIOStreamReader::rewind()
{
  if(input_is_stdio)
  {
    throw SeisppError(string("BinaryIOStreamReader rewind method:  ")
      + "input is tied to stdin - rewind is not possible for stdin");
  }
  else
    ifs.seekg(0,ios_base::beg);
}
BinaryIOStreamWriter::BinaryIOStreamWriter()
{
  /* Note ofs is left invalid in this condition - stdin cannot seek
  which creates a disconnect */
  output_is_stdio=true;
  nobjects=0;
  parent_filename="STDIN";
  /* This may not be necessary according to the documentation */
  ios::sync_with_stdio();
  ar=new boost::archive::binary_oarchive(std::cout);
}
BinaryIOStreamWriter::BinaryIOStreamWriter(string fname)
{
  try{
    const string base_error("BinaryIOStreamWriter file constructor:  ");
    ofs.open(fname.c_str(),ios::out | ios::trunc | ios::binary);
    if(ofs.fail())
    {
      throw SeisppError(base_error+"open failed on file "+fname+" for output");
    }
    parent_filename=fname;
    output_is_stdio=false;
    nobjects=0;
    ios::sync_with_stdio();
    ar=new boost::archive::binary_oarchive(ofs);
  }catch(...){throw;};
}
BinaryIOStreamWriter::~BinaryIOStreamWriter()
{
  int i;
  ofs.write(binary_eof_tag,BINARY_TAG_SIZE);
  ofs.write((char *)(&nobjects),sizeof(long));
  delete ar;
}

} // End SEISPP namespace encapsulation 
