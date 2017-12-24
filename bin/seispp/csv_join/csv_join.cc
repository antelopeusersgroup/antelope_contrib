#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <string>
#include <map>
#include "minicsv.h"
using namespace std;   
void usage()
{
    cerr << "csv_join master joinfile [-keycolm n -keycolin n -valcol n --help]"
        <<endl
        << "Special program for multiwavelet processing to merge mwpmavg csv output"<<endl
        << "with the output of listhdr.   Could, however, have other uses."<<endl
        << "Algorithm assumes master is reduced subset of attributes exctracted"
        << endl
        << "from a seispp data set and written as a csv file.   This program"
        << endl
        << "acts like a database join using a string key BUT assuming the "
        << endl
        << "left table (master) is a subset of the right table (joinfile)."
        <<endl
        <<"  The result is a table "
        << "of the same length as master matching row by row.  "<<endl
        << "This allows easy plotting with things like matlab or python."
        <<endl
        << "Output table is a csv file with the key in column 1 and desired data field in column 2"
        <<endl
        << "Output is to stdout"<<endl
        << "-keycolm and -keycolin are used to specify the column where the "
        << "string match key is expected for the master and input files respectively"<<endl
        << "(Default is 1 (first column) for both"<<endl
        << "-valcol is used to specify which column from the input file is written to output"
        <<endl
        << "Reads serialized ThreeComponentEnsemble objects from stdin"<<endl
        << "and writes a copy to stdout"<<endl
        << " --help - prints this message"<<endl;
    exit(-1);
}
map<string,int> load_master(csv::ifstream& ifs,int col)
{
  map<string,int> result;
  int i,j;
  string key;
  j=0;
  while(ifs.read_line())
  {
    /* skip to col.  col assumed to start from 1 not zero  */
    string junk;
    for(i=1;i<col;++i) ifs>>junk;
    ifs>>key;
    result.insert(pair<string,int>(key,j));
    ++j;
  }
  return result;
}
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    if(argc<3) usage();
    string masterfile(argv[1]);
    string joinfile(argv[2]);
    int keycolm(1),keycolin(1),valcol(2);
    for(i=3;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-keycolm")
        {
            ++i;
            if(i>=argc)usage();
            keycolm=atoi(argv[i]);
        }
        else if(sarg=="-keycolin")
        {
            ++i;
            if(i>=argc)usage();
            keycolin=atoi(argv[i]);
        }
        else if(sarg=="-valcol")
        {
            ++i;
            if(i>=argc)usage();
            valcol=atoi(argv[i]);
        }
        else
            usage();
    }
    try{
      csv::ifstream leftin(masterfile.c_str());
      if(!leftin.is_open())
      {
        cerr << "Cannot open master (left table) file ="<<masterfile<<endl;
        usage();
      }
      csv::ifstream rightin(joinfile.c_str());
      if(!rightin.is_open())
      {
        cerr << "Cannot open jointfile (right table) file ="<<joinfile<<endl;
        usage();
      }
      map<string,int> master(load_master(leftin,keycolm));
      /* Create vectors for output of length set by size of master */
      int nrows=master.size();
      string *okeys,*ovals;
      okeys=new string[nrows];
      ovals=new string[nrows];
      int maxcol=max(keycolin,valcol);
      int nset(0);
      while(rightin.read_line())
      {
        map<string,int>::iterator mptr;
        string key,val,stmp;
        int iout;
        for(i=1;i<=maxcol;++i)
        {
          rightin>>stmp;
          if(i==keycolin) 
            key=stmp;
          if(i==valcol)
            val=stmp;
        }
        mptr=master.find(key);
        if(mptr!=master.end())
        {
            iout=master[key];
            if(iout>=nrows)
            {
              cerr << "Master index value ="<<iout<<" exceeds expected size="
                << nrows<<endl
                << "This should not happen - programming bug likely"
                <<endl
                << "Blundering on but output is is suspect"<<endl;
            }
            else
            {
              okeys[iout]=key;
              ovals[iout]=val;
              ++nset;
              continue;
            }
          }
      }
      for(i=0;i<nrows;++i)
      {
        cout << okeys[i]<<","<<ovals[i]<<endl;
      }
      if(nrows!=nset)
      {
        cerr << "csv_join(WARNING):  size mismatch in output table"<<endl
          << "Master table has "<<nrows<<endl
          << "Join with right table set only "<<nset<<endl
          << "Expect null rows in output"<<endl;
      }
    }catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

