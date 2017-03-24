#include <string>
#include <iostream>
#include <fstream>
#include "seispp.h"
#include "seispp_io.h"
using namespace std;
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
  char *buf=new char [TextIOStreamEOFOffset];
  sprintf(buf,"%s %ld\n",eof_tag.c_str(),nobjects);
  int i;
  for(i=0;i<TextIOStreamEOFOffset;++i)
  {
    if(output_is_stdio)
      cout<<buf[i];
    else
      ofs<<buf[i];
  }
  ofs.close();
  delete ar;
}
/* Binary versions go next - silly to do them until we at least get
the painfully similar text versions above to at least compile */
