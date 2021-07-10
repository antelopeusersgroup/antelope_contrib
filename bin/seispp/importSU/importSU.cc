#include <iostream>
#include "seispp.h"
#include "StreamObjectWriter.h"
/* This could be in an include, but will insert this prototype here
   rather than make an include file with one line.*/
TimeSeries ReadSegyTrace(FILE *,bool load_coordinates);

void usage()
{
    cerr << "importSU [--help -text] < sufile > seisppfile"<<endl
        << "Translates seismic unix file to file of serialized TimeSeries objects"
        <<endl
        << "Use --help to get this usage line"<<endl
        << "Use -text to serialize as a text file (default is binary)"
        <<endl;
    exit(-1);
}
/* This is defined in SU's par.h and seems necessary for this to link.
      Not used in this code, but an annoying extern.   This is 
      an incredibly obscure trick to make this work.  Found by 
     pure hacking
*/
extern "C"{
int xargc;
char **xargv;
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    xargc=argc;
    xargv=argv;
    int i,j;
    bool binary_data(true);
    /* This perhaps should be an option, which is why we define
       it here.  This way it is a trivial change to make it optional */
    bool load_coordinates(true);
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
        else
            usage();
    }
    try{
        char form;
        if(binary_data)
            form='b';
        else
            form='t';
        StreamObjectWriter<TimeSeries> oa(form);
        bool readok(true);
        int nread(0);
        do{
            TimeSeries dread=ReadSegyTrace(stdin,load_coordinates);
            if(dread.ns>0)
            {
                oa.write(dread);
                ++nread;
            }
            else
                readok=false;
        }while(readok);
        cerr << "importSU read and converted "<<nread<<" seismograms"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
}
