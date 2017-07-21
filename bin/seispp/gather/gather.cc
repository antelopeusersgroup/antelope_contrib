#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <list>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
/* You should always include a usage procedure like this to trap
   command line parsing problems. */
void usage()
{
    cerr << "gather -i key1 key2 ... -s key1 key2 ... [-binary] < in > out"
        <<endl
        << "Build gathers using list of integer and string keys"<<endl
        << "Follow -i with list of integer keys that define a match"<<endl
        << "Follow -s with a list of string keys that define a match"<<endl
        << "Gather grouping is defined by exactly matching all key values"<<endl
        << "group keys are posted to ensemble metadata"<<endl
        << "Use -binary flag to switch to binary data input and output"
        <<endl;
    exit(-1);
}
bool keys_match(ThreeComponentSeismogram& d,list<string>& sk, list<string>& ik,
    list<string>stest, list<int> itest)
{
  try{
    list<string>::iterator skptr,svptr;
    for(skptr=sk.begin(),svptr=stest.begin();skptr!=sk.end();++skptr,++svptr)
    {
      string svnow=d.get_string(*skptr);
      if(svnow != (*svptr)) return false;
    }
    list<string>::iterator ikptr;
    list<int>::iterator ivptr;
    for(ikptr=ik.begin(),ivptr=itest.begin();ikptr!=ik.end();++ikptr,++ivptr)
    {
      int intvnow=d.get_int(*ikptr);
      if(intvnow != (*ivptr)) return false;
    }
    return true;
  }catch(...){throw;};
}
void reset_ensemble(ThreeComponentSeismogram& d,list<string> skeys,
  list<string> ikeys,ThreeComponentEnsemble& dout,
    list<string>& svaltest,list<int>& ivaltest)
{
  try{
    list<string>::iterator sptr;
    list<string>::iterator iptr;
    dout.member.clear();
    svaltest.clear();
    ivaltest.clear();
    for(sptr=skeys.begin();sptr!=skeys.end();++sptr)
    {
      string sval=d.get_string(*sptr);
      svaltest.push_back(sval);
      dout.put(*sptr,sval);
    }
    for(iptr=ikeys.begin();iptr!=ikeys.end();++iptr)
    {
      int ival=d.get_int(*iptr);
      ivaltest.push_back(ival);
      dout.put(*iptr,ival);
    }
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    if(argc<3)usage();
    list<string> ikeys,skeys;
    bool binary_data(false);
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-i")
        {
            while(i<(argc-1))
            {
              ++i;
              if(i>=argc)usage();
              sarg=string(argv[i]);
              if(sarg=="-binary") 
              {
                binary_data=true;
                break;
              }
              if(sarg=="-s")
              {
                --i;
                break;
              }
              ikeys.push_back(sarg);
            }
        }
        else if(sarg=="-s")
        {
            while(i<(argc-1))
            {
              ++i;
              if(i>=argc)usage();
              sarg=string(argv[i]);
              if(sarg=="-binary") 
              {
                binary_data=true;
                break;
              }
              if(sarg=="-i")
              {
                --i;
                break;
              }
              skeys.push_back(sarg);
            }
        }
        else if(sarg=="-binary")
          binary_data=true;
        else
          usage();
    }
    try{
        list<string> svaltest;
        list<int> ivaltest;
        shared_ptr<StreamObjectReader<ThreeComponentSeismogram>> ia;
        if(binary_data)
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>('b'));
        }
        else
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> oa;
        if(binary_data)
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentSeismogram d;
        ThreeComponentEnsemble dout;
        list<string>::iterator sptr,iptr;
        int nseis,ngather;
        nseis=0;   ngather=0;
        while(!ia->eof())
        {
            d=ia->read();
            if(nseis==0)
            {
              for(sptr=skeys.begin();sptr!=skeys.end();++sptr)
              {
                string stmp=d.get_string(*sptr);
                svaltest.push_back(stmp);
                dout.put(*sptr,stmp);
              }
              for(iptr=ikeys.begin();iptr!=ikeys.end();++iptr)
              {
                int itmp=d.get_int(*iptr);
                ivaltest.push_back(itmp);
                dout.put(*iptr,itmp);
              }
              dout.member.push_back(d);
            }
            else
            {
              if(keys_match(d,skeys,ikeys,svaltest,ivaltest))
              {
                dout.member.push_back(d);
              }
              else
              {
                ++ngather;
                oa->write(dout);
                /* This clears dout and then initializes ensemble metadata
                and sets svaltest and ivaltest with values from d.*/
                reset_ensemble(d,skeys,ikeys,dout, svaltest,ivaltest);
                dout.member.push_back(d);
              }
            }
            ++nseis;
        }
        /* Save the last ensemble */
        if(dout.member.size() > 0) 
        {
            oa->write(dout);
            ++ngather;
        }
        cerr << "Gather processed "<<nseis<<" 3c seismograms"<<endl
                << "Assembled "<<ngather<<" ensembles"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
        exit(-1);
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
        exit(-1);
    }
}
