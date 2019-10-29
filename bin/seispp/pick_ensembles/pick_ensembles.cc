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
    cerr << "pick_ensembles infile outfile [-noalign -pf pffile -v --help -text]"
        <<endl
        << "Pick a file of ThreeComponentEnsembles to align all members to zero time."<<endl
        << "The program is agnostic about what alignment should be, but typically alignment is seismic phase"<<endl
        << "The program is a mix of GUI and command line type in responses through stdin and stdout.  "<<endl
        << "Because of the stdin/stdout driven responses program reads from infile"<<endl
        << "and writes aligned data to outfile"<<endl
        << " -noalign - when this flag appears in arg list the display will"<<endl
        << "   show time shifts but output will not be shifted with picks only defined by header field pickedtime"<<endl
        << "   (Default will align the output with picks so pick times define 0 relative time"
        <<endl
        << " -pf read from alternative pf file pffile (default is pick_ensemble.pf)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* Takes output of picker defined with the map container picks 
 * and loads the time shifts into the headers (metadata) of matching
 * seismograms.   Since not all seismograms have to be picked we 
 * return the number of picks actually set. If the boolean align is
 * true the seismograms are shifted by the picked lag to match the
 * align method.  Warning - that will permanently alter t0 of each 
 * seismogram */
int load_picks(ThreeComponentEnsemble& d, IndexTimes& picks, bool align)
{
    try{
        int nset(0);
        /* Now set only picks that are defined */
        IndexTimes::iterator pptr;
        for(nset=0,pptr=picks.begin();pptr!=picks.end();++pptr,++nset)
        {
            int imem=pptr->first;
            double atime=pptr->second;
            /* The picker always converts pick times to absolute 
             * times.   To compute shift we need to compute the lag
             * this way. */
            d.member[imem].put(TCPICKKEY,atime);
            double t0s=d.member[imem].time_reference();
            double lag=atime-t0s;
            d.member[imem].put("picked_lag",lag);
            if(align) d.member[imem].shift(lag);
        }
        return nset;
    }catch(...){throw;};
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
    bool align_output(true);
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
        else if(sarg=="-noalign")
        {
            align_output=false;
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
        /* first clear any older picks and make sure all seismograms
         * have the key field here defined.  This is fixes an
         * inadequacy (bug) in the picker object. */
        vector<ThreeComponentSeismogram>::iterator dptr;
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
            dptr->put(TCPICKKEY,0.0);
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
        IndexTimes picks;
        picks=win.get_member_times();
        int npicks;
        npicks=load_picks(d,picks,align_output);
        cout << "Writing data after setting "<<npicks<<" picks"<<endl;
        if(align_output) cout << "Note:  the output data were shifted by pick times"
                            <<endl;
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
