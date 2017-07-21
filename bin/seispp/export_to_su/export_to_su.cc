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
    cerr << "export_to_su outfile [-pf pffile] < seispp_input"
        <<endl
        << "Convert a text format serialized TimeSeriesEnsemble object to a seismic unix file" <<endl
        << "outfile is a file name for seismic unix output"<<endl
        << "-pf - uses alternative pf file from default export_to_su.pf"<<endl;
    exit(-1);
}


bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{

    int i;
    const int narg_required(1);
    string pffile("export_to_su");
    string outfile(argv[1]);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
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
            1000.0,true);
        StreamObjectReader<TimeSeriesEnsemble> ia;
        TimeSeriesEnsemble d;
        d=ia.read();
        if(SEISPP_verbose)
        {
          cerr << "export_to_su read ensemble with "<<d.member.size()<<" seismograms"<<endl
            << "Writing seismic unix format data to file="<<outfile<<endl;
        }
        vector<TimeSeries>::iterator dptr;
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
          /* This strange header value for su seems necessary */
          dptr->put("duse",1);
          outhandle.put(*dptr);
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
