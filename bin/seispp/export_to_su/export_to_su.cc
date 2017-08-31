#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "stock.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "ensemble.h"
#include "GenericFileHandle.h"
using namespace std;
using namespace SEISPP;

void usage()
{
    cerr << "export_to_su outfile [-text -pf pffile] < seispp_input"
        <<endl
        << "Convert a serialized file of TimeSeries objects to a seismic unix file" <<endl
        << "(Use dismember to convert an enemble for input into this program"
        <<endl
        << "outfile is a file name for seismic unix output"<<endl
        << "-text - assume input is text format (default is binary)"<<endl
        << "-pf - uses alternative pf file from default export_to_su.pf"<<endl;
    exit(-1);
}


bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{

    int i;
    const int narg_required(1);
    if(argc<(narg_required+1)) usage();
    string pffile("export_to_su");
    string outfile(argv[1]);
    if(outfile=="--help") usage();
    bool binary_data(true);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-text")
            binary_data=false;
        else
            usage();
    }
    try{
        Pf *pf;
        if(pfread(const_cast<char *>(pffile.c_str()),&pf))
        {
          cerr << "pfread failed for pffile="<<pffile<<endl;
          usage();
        }
        string xrefstr=pftbl2string(pf,"metadata_cross_reference");
        AttributeCrossReference outxref(xrefstr);
        list<string> tmdlist=pftbl2list(pf,"output_metadata_list");
        /* These are empty lists used to trick the GenericFileHandle
           Borrowed from export_volume */
        list<string> orderkeys,endslist;
        orderkeys.clear();   endslist.clear();
        /* This incantation defines the su writer */
        GenericFileHandle outhandle(outfile,string("SEGYfloat"), outxref,
            orderkeys,endslist,tmdlist,false,string("nsamp"),string("dt"),
            1000000.0,true);
        shared_ptr<StreamObjectReader<TimeSeries>> iaptr;
        if(binary_data)
            iaptr=shared_ptr<StreamObjectReader<TimeSeries>>
                (new StreamObjectReader<TimeSeries>('b'));
        else
            iaptr=shared_ptr<StreamObjectReader<TimeSeries>>
                (new StreamObjectReader<TimeSeries>('t'));
        TimeSeries d;
        int nseis(0);
        while(iaptr->good())
        {
            d=iaptr->read();
            /* This strange header value for su seems necessary */
            d.put("duse",1);
            outhandle.put(d);
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
