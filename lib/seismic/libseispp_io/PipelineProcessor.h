#ifndef _STREAM_OBJECT_WRITER_H_
#define _STREAM_OBJECT_WRITER_H_
#include <ext/stdio_filebuf.h>
#include <iostream>
#include <fstream>
#include <list>
#include "BasicObjectWriter.h"
#include "seispp_io.h"
#include "PfStyleMetadata.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
/*! \brief Engine to execute a unix pipeline from a calling C++ program.

This is a lower level object used for implementing parallel processing
in the foreman/worker model.   This engine is used to generate a pipeline
of unix commands.   This is used in parallel processing as the downstream
exec from a splitter.   That is a stream of data of common objects is
split into parallel steams.   This object can be used to execute a pipeline
on each of the parallel streams.   

The implementation has some complexities in handling the output of the 
pipeline.   See the description of the primary constructor for details.
Note like most io handles there is no copy constructor or operator= for
this beast.   That is especially irrational for this beast since an 
implict fork is created using the popen function.  
*/

template <class T> class PipelineProcessor : public BasicObjectWriter<T>
{
  public:
    /*! Default constructor:  if used will only throw an error.*/
    PipeLineProcessor()
    {
      throw SeisppError(string("PipelineProcessor - coding error\n")
              + "Constructor with no arguments is undefined");
    };
    /*! \brief Construct from mix of parameter file data and arguments.

    This is the only constructor defined in this api for this object.
    It uses a parameter file to define the processing chain.   Lines
    that define each command of the pipeline are expected to
    are expected to appear in a newline separated list in an antelope
    Tbl& section headed by the keyword pipeline_commands.   Note 
    that the lines must NOT contain the special pipeline symbol ("|"}
    or input/output redirection symbols used by the unix shell.  
    Furthermore there is currently no mechanism to implement shell
    variable subsitution in any of the pipeline command lines.   

    Output of the pipeline is handled by a (perhaps unnecessarily 
    complicated) algorithm.  The outfile argument is a switch with 
    two special options.  If outfile is set to UsePf (the default) 
    the output file name will be extracted from the parameter file
    with the key "pipeline_output_file".   If set to "none" 
    the pipeline output will go to stdout.   Note that output to 
    stdout is nearly guaranteed to create chaos in a parallel job
    as there is high probability the output will get garbled or the 
    job may just fail.   Hence, think of none as useful mainly for testing
    with a scalar job.  

    \param pf - PfStyleMetadata object parsed from an antelope pf file. 
      See above for required contents. 
    \param outfile - name of the file to contain stdout written by the 
      executed pipeline.   (see above for special keywords that influence
      behavior. Default is UsePf.
    \param - format switch for stream data.  Must be 't' for text data or
      'b' for binary data (the default).

    \throw SeisppError for various condition that cause failure.  
      */

    
    PipelineProcessor(PfStyleMetadata& pf,const string outfile="UsePf",
            const char format='b') throw SeisppError(serr&);
    ~PipelineProcessor();
    void write(T& d);
    long number_already_written(){return nobjects;};
    /*! \brief Return the command to be processed by the pipeline.

      This is a convenience method that returns the command line
      of the process pipeline.
     */
    string commands();
  private:
    char format;
    long nobjects;
    string pipeline_commands;
    FILE *pipe;
    int fdesc;  // Integer file descriptor of pipe
    /* These four need to be pointers becauase I see no way
    to copy them to create a simpler constructor.   Makes sense for
    on io operator, but still awkward as it ads complexity to the
    destructor.   I also worry these may create dulicate frees in
    the destructor because filebuf is used these define a chain
    of objects built on a common buffer.   If any of the intermediaties
    delete teh buffer we are hosed.
    */
    __gnu_cxx::stdio_filebuf<char> *filebuf;
    ostream *os_pipe;
    boost::archive::binary_oarchive *bin_ar;
    boost::archive::text_oarchive *txt_ar;
    bool ready;
};
template <class T>
  PipelineProcessor<T>::PipelineProcessor(PfStyleMetadata& pf, 
          string outfile,char form)
{
  const string base_error("PipelineProcessor pf contructor:  ");
  ios::sync_with_stdio();
  format=form;
  nobjects=0;
  list<string> command_lines;
  try{
    command_lines=pf.get_tbl("pipeline_commands");
    pipeline_commands=string("");
    list<string>::iterator sptr;
    int ncommands=command_lines.size();
    int i(1);  // not initialization to 1 to make conditional in loop simpler
    for(sptr=command_lines.begin();sptr!=command_lines.end();++sptr)
    {
      pipeline_commands+=(*sptr);
      if(i!=ncommands) pipeline_commands += " | ";
      ++i;
    }
    string fname;
    if(outfile="UsePf")
      fname=pf.get_string("pipeline_output_file");
    else
      fname=outfile;
    if(fname!="none")
    {
        pipeline_commands += " > ";
        pipeline_commands += outfile;
    }
    pipe=popen(pipeline_commands.c_str(),"w");
    if(pipe==NULL) throw SeisppError(base_error
        + "popen failed\nCommand lines passed to popen:\n"
        + pipeline_commands);
    fdesc=fileno(pipe);
    /* This is a nonstandard way to associate a C file descriptor with
    a C++ stream.   It depends on a gnu extension that states clearly this
    only works on unix systems. */
    filebuf = new __gnu_cxx::stdio_filebuf<char>(fdesc,ios::out);
    os_pipe = new ostream(filebuf);
    switch(format)
    {
      case 't':
        txt_ar = new boost::archive::text_oarchive(*os_pipe);
        bin_ar=NULL;
        break;
      case 'b':
      default:
        bin_ar = new boost::archive::binary_oarchive(*os_pipe);
        txt_ar=NULL;
      };
    ready=true;
  }catch(...){throw;};
}
template <class T> PipelineProcessor<T>::~PipelineProcessor()
{
  /* This complexity is needed to add the eof information for a seispp
  stream file.  This is modified from the StreamObjectWriter destructor */

  int i;
  char *buf;
  buf=new char [TextIOStreamEOFOffset];
  /* Initialize the buffer to all blanks */
  for(i=0;i<TextIOStreamEOFOffset;++i) buf[i]=' ';
  switch(format)
  {
    case 't':
      buf=new char [TextIOStreamEOFOffset];
      /* Initialize the buffer to all blanks */
      for(i=0;i<TextIOStreamEOFOffset;++i) buf[i]=' ';
      sprintf(buf,"%s %ld\n",eof_tag.c_str(),nobjects);
      for(i=0;i<TextIOStreamEOFOffset;++i) (*os_pipe)<<buf[i];
      delete [] buf;
      break;
    case 'b':
    default:
      (*os_pipe).write(eof_tag.c_str(),BINARY_TAG_SIZE);
      (*os_pipe).write((char *)(&nobjects),sizeof(long));
  };
   /* delete io elements in reverse order they were created.  Hopefully
   that will prevent duplicate frees */
  if(bin_ar!=NULL) delete bin_ar;
  if(txt_ar!=NULL) delete txt_ar;
  delete os_pipe;
  /* This delete causes the calling program to abort.   Most likely 
   * creates a duplicate free situation.   Possible memory leak but 
   * don't expect this to be called many times and the destructor 
   * would almost always be expected to be called on exit. */
  //delete filebuf;
  int ret;
  ret=pclose(pipe);
  if(ret!=0) throw SeisppError(string("PipelineProcessor destructor:  ")
         + "pclose failed - output is probably incomplete");
}
/* How to do this efficiently is not clear.   There is a known collision
at present between low level C io funtions accessible through FILE and
C++ streams.   There is a gnu trick for unix that is probably best, but
does not seem to be present in stock install for Macs.  Maybe
for linux.   In any case, a problem to solve.   this url has excellent
advice:  https://gcc.gnu.org/onlinedocs/libstdc++/manual/fstreams.html
Seems clear the solutionr equires streambuf

Here is the full refernce for the gnu implentation I think i can use:
https://gcc.gnu.org/onlinedocs/libstdc++/manual/ext_io.html */
template <class T> void PipelineProcessor<T>::write(T& d)
{
  string base_error("PipelineProcessor::write method:  ");
  try{
    switch(format)
    {
      case 't':
        if(nobjects>0)
        {
          (*os_pipe)<<more_data_tag<<endl;
        }
        (*txt_ar) << d;
        break;
      case 'b':
      default:
        if(nobjects>0)
        {
          os_pipe->write(more_data_tag.c_str(),BINARY_TAG_SIZE);
        }
        (*bin_ar) << d;
    };
    ++nobjects;
  }catch(...){throw;};
}

} //End namespace SEISPP declaration
#endif
