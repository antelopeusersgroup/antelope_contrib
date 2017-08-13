#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
//#include "PMTimeSeries.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "listhdr [-i infile -csv format_file -t objecttype -text -showfile -showcount]  >outfile"
        <<endl
        << "List metadata components of a stream of serialized objects"
        <<endl
        << "Default read from stdin use -i option to read from file infile"
        <<endl
        << " -csv - write the output as a csv file using the format defined in format_file"
        <<endl
        << "        (default dumps all with operator <<"<<endl
        << " -t - specify the type of object expected"<<endl
        << "      (Currently accept:  ThreeComponentSeismogram (default), ThreeComponentEnsemble"<<endl
        << "      TimeSeries, TimeSeriesEnsemble, and PMTimeSeries)"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -showfile - prints file name in first column (allowed only with -i option)"
        <<endl
        << "-showcount - print object count in a file with multiple objects"
        <<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE, PMTS, TS, TSE};
/*! Simple class to drive csv outputs in this program. */
class MetadataComponent
{
    public:
        string key; //metadata key to access 
        MDtype mdt;  //type enum (defined in Metadata.h)
        string undefined_value;  // This value is value to write to output if key not found
};
vector<MetadataComponent> parse_csv_format_file(string fname)
{
    ifstream icff;
    icff.open(fname.c_str(),ios::in);
    if(!icff) 
    {
        cerr << "parse_csv_format_file procedure:  open failed on file "
            << fname<<endl;
        exit(-1);
    }
    vector<MetadataComponent> result;
    char inpline[128];
    while(icff.getline(inpline,128))
    {
        stringstream ss(inpline);
        MetadataComponent mc;
        ss>>mc.key;   
        string mdttest;
        ss >> mdttest;
        if( (mdttest=="double") || (mdttest=="MDreal") || (mdttest=="real") || (mdttest=="float") )
            mc.mdt=MDreal;
        else if( (mdttest=="int") || (mdttest=="MDint") || (mdttest=="long") )
            mc.mdt=MDint;
        else if(mdttest=="boolean")
            mc.mdt=MDboolean;
        else if( (mdttest=="string") || (mdttest=="String") || (mdttest=="MDstring") )
            mc.mdt=MDstring;
        else
        {
            cerr << "parse_csv_format_file procedure: Unrecognized type ="<<mdttest
               <<" for entry with key="<<mc.key<<endl
              << "Fatal error - exiting"<<endl;
           exit(-1);
        }
        ss>>mc.undefined_value;
        result.push_back(mc);
    } 
    return result;
}
/* This procedure will write one line to csv file extracting from Metadata d using
   the mdl vector to define the output format and null defaults */
int WriteToCSVFile(Metadata& d,ostream& ofs,vector<MetadataComponent>& mdl)
{
    int nlast=mdl.size()-1;
    int i;
    int nsaved;
    double dval;
    int ival;
    bool bval;
    string sval;
    vector<MetadataComponent>::iterator mdptr;
    /* We do this to be sure epoch times get printed correctly in all cases.   Makes
       sense here as bloated files are unlikely to be an issue with metadata output*/
    ofs<<std::setprecision(13);
    for(i=0,mdptr=mdl.begin();mdptr!=mdl.end();++mdptr,++i)
    {
        try{
            switch (mdptr->mdt)
            {
                case MDreal:
                    dval=d.get_double(mdptr->key);
                    ofs << dval;
                    break;
                case MDint:
                    ival=d.get_int(mdptr->key);
                    ofs << ival;
                    break;
                case MDstring:
                    sval=d.get_string(mdptr->key);
                    ofs << sval;
                    break;
                case MDboolean:
                    bval=d.get_bool(mdptr->key);
                    ofs << bval;
                    break;
                default:
                    cerr << "Illegal mdtype specified"<<endl
                        << "This should not happen unless the program overwrites itself"<<endl
                        << "Fatal error:  exiting"<<endl;
                    exit(-1);
            }
            ++nsaved;
        }catch (SEISPP::MetadataGetError& err)
        {
            ofs << mdptr->undefined_value;
        }
        if(i<nlast) ofs<<",";
    }
    ofs << endl;   // endl does not work for minicsv
    return nsaved;
}

AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template<class Treader> 
  void listhdr_generic(string infile, bool binary_data, bool use_stdin,
          bool csv_output, vector<MetadataComponent>& csv_format_info,
          bool showfile, bool showcount)
{
    StreamObjectReader<Treader> *rptr;
    try {
        rptr=NULL;  // do this in case the next procedure aborts 
        rptr=BuildReadHandle<Treader>(infile,binary_data,use_stdin);
        int i(0);
        while(rptr->good())
        {
            Treader d;
            d=rptr->read();
            Metadata md(dynamic_cast<Metadata&>(d));
            if(csv_output)
            {
              if(showfile) cout<<infile<<",";
              if(showcount) cout <<i<<",";
              WriteToCSVFile(md,cout,csv_format_info);
            }
            else
            {
              if(showfile) 
                  cout<<"Metadata contents for object in file="<<infile<<endl;
              if(showcount)
                  cout << "Metadata at index position="<<i<<endl;
              cout << md;
            }
            ++i;
         }
    }catch(SeisppError& serr)
    {
        if(rptr!=NULL) delete rptr;
        throw serr;
    }
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    string infile("Standard Input");
    bool csv_output(false);
    string otype("ThreeComponentSeismogram");
    string fname_csvo;
    bool use_stdin(true);
    bool binary_data(true);
    bool showfile(false);
    bool showcount(false);
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-csv")
        {
            ++i;
            if(i>=argc)usage();
            csv_output=true;
            fname_csvo=string(argv[i]);
        }
        else if(sarg=="-i")
        {
            ++i;
            if(i>=argc)usage();
            infile=string(argv[i]);
            use_stdin=false;
        }
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else if(sarg=="-text")
            binary_data=false;
        else if(sarg=="-showfile")
            showfile=true;
        else if(sarg=="-showcount")
            showcount=true;
        else
            usage();
    }
    try{
        if(use_stdin && showfile)
        {
            cerr << "Illegal argument combination"<<endl
                << "-showfile options not allowed with input from stdin"
                <<endl;
            usage();
        }
        AllowedObjects dtype=get_object_type(otype);
        vector<MetadataComponent> csv_format_info;
        if(csv_output)
            csv_format_info=parse_csv_format_file(fname_csvo);
        switch (dtype)
        {
            case TCS:
                listhdr_generic<ThreeComponentSeismogram>(infile,
                        binary_data,use_stdin,csv_output,csv_format_info,
                        showfile,showcount);
                break;
            case TCE:
                listhdr_generic<ThreeComponentEnsemble>(infile,
                        binary_data,use_stdin,csv_output,csv_format_info,
                        showfile,showcount);
                break;
            case TS:
                listhdr_generic<TimeSeries>(infile,
                        binary_data,use_stdin,csv_output,csv_format_info,
                        showfile,showcount);
                break;
            case TSE:
                listhdr_generic<TimeSeriesEnsemble>(infile,
                        binary_data,use_stdin,csv_output,csv_format_info,
                        showfile,showcount);
                break;
            case PMTS:
                cerr << "PMTimeSeries not yet supported"
                    <<"Cannot run"<<endl;
                exit(-1);
                //fofflist=build_index<PMTimeSeries>(infile);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
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

