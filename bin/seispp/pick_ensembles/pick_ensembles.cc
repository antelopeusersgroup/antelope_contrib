#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ThreeCEnsembleTimePicker.h"

using namespace std;
using namespace SEISPP;

void usage()
{
    cerr << "pick_ensembles infile outfile [-pf pffile -v --help -text]"
        <<endl
        << "Pick a file of ThreeComponentEnsembles to align all members to zero time."<<endl
        << "The program is agnostic about what alignment should be, but typically alignment is seismic phase"<<endl
        << "The program is a mix of GUI and command line type in responses through stdin and stdout.  "<<endl
        << "Because of the stdin/stdout driven responses program reads from infile"<<endl
        << "and writes aligned data to outfile"<<endl
        << " -pf read from alternative pf file pffile (default is pick_ensemble.pf)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    if(argc<3) usage();
    string infile(argv[1]);
    string outfile(argv[2]);
    bool binary_data(true);
    string pffile("pick_ensembles.pf");
    for(i=3;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc) usage();
          pffile=string(argv[i]);
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
      PfStyleMetadata control=pfread(pffile);
      shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> inp;
      if(binary_data)
      {
        inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>(infile,'b'));
      }
      else
      {
        inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>(infile,'t'));
      }
      shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> out;
      if(binary_data)
      {
        out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>(outfile,'b'));
      }
      else
      {
        out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>(outfile,'t'));
      }
      ThreeCEnsembleTimePicker win(control);
      ThreeComponentEnsemble d;
      int pass;
      while(!inp->eof())
      {
        d=inp->read();
        pass=1;
        string ques;
        do{
          if(pass==1)
          {
            win.plot(d);
          }
          win.pick();
          win.align();
          cout << "Are picks ok?  Enter:"<<endl
            << "y - to accept these picks and save results"<<endl
            << "c - to clear all picks and start over"<<endl
            << "r - to refine the picks"<<endl;
          cin >> ques;
          if(ques=="c")
            win.reset();
          else
            ++pass;
        }while(ques!="y");
        out->write(d);
      }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
