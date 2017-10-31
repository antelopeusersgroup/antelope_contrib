#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <list>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP; 
void usage()
{
    cerr << "cat file1 file2 ... filen [-text --help]"
        <<endl
        << "Concatenate a set of seispp files into a single larger file"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"
        << "(Note:  will exit with an error if the count of files is only one)"
        <<endl;
    exit(-1);
}
/* This was copied from listhdr - should probably become a compiled
   procedure */
enum AllowedObjects {TCS, TCE, PMTS, TS, TSE};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename T> 
     void cat_seispp_generic(list<string>& files,bool binary_data)
{
  char dform;
  if(binary_data)
    dform='b';
  else
    dform='t';
  try{
    /* Output is always to stdout */
    StreamObjectWriter<T> ofs;
    list<string>::iterator fptr;
    for(fptr=files.begin();fptr!=files.end();++fptr)
    {
      StreamObjectReader<T> ifs(fptr->c_str(),dform);
      T d;
      while(ifs.good())
      {
        d=ifs.read();
        ofs.write(d);
      }
    }
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    bool binary_data(true);
    list<string> FileList;
    AllowedObjects otype;
    if(argc<=2) usage();

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-t")
        {
          ++i;
          if(i>argc) usage();
          otype=get_object_type(string(argv[i]));
        }
        else
        {
          FileList.push_back(sarg);
        }
    }
    if(FileList.size()<2) usage();
    try{
      switch(otype)
      {
        case TCS:
          cat_seispp_generic<ThreeComponentSeismogram>(FileList,binary_data);
          break;
        case TS:
          cat_seispp_generic<TimeSeries>(FileList,binary_data);
          break;
        case TSE:
          cat_seispp_generic<TimeSeriesEnsemble>(FileList,binary_data);
          break;
        case PMTS:
          cat_seispp_generic<PMTimeSeries>(FileList,binary_data);
          break;
        case TCE:
        default:
          cat_seispp_generic<ThreeComponentEnsemble>(FileList,binary_data);
      };
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

