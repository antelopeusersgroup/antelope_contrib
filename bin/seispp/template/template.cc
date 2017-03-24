/* This is a template file for building a unix style filter for 
the seispp library using boost serialization.  This example 
uses serial, plain text format data as output but I plan to evolve the
generic io object to binary and (maybe) the boost xml format.  The
concept here is to have unix style filters applied to sequential files
that contain an arbitrary number of objects of a common type with 
serialization defined.   The implementation is hidden behind an 
interface defined in the seispp_io library.   

This example simply copies a serialized ThreeComponentEnsemble from 
stdin to stdout.   make should build it under the name template. 

See comments below to see how to customize this for a different algorithm.
You can also look at examples that (at least for now) are in the directory
one up from where this file is located. 

You should, of course, strip all the template comments when you 
get your program to work.  Alternatively, start from template_plain.cc.
*/

/* This set of system includes are always required.  Do not remove them.*/
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
/* These are SEISPP includes that are a base requirement 
   to use this template file */
#include "seispp.h"
/* Replace these as needed.  This one is here only to make
   this do nothing template compile for testing configuration */
#include "ensemble.h"
/* This file defines the object that abstracts object io */
#include "seispp_io.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
/* You should always include a usage procedure like this to trap
   command line parsing problems. */
void usage()
{
    cerr << "usage line goes here"
        <<endl
        << "add short explanation of args here if needed"<<endl;
    exit(-1);
}
/* This obnoxious external variable is a necessary evil to deal with 
   error logging in the SEISPP library. Your code will probably not link 
   without it.*/ 
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    /* Common variables for a program common appear here, but 
       C/C++ now allow later declarations that usually make 
       cleaner code.   We need this counter for the arg list below*/
    int i;
    /* As the name implies set this to the number of required 
       args */
    const int narg_required(0);
    /* Here you should crack required args.  For example:
       string dbname(argv[1]);
       */
    /* Follow this by global declarations related to arg 
       list.  These two are generic for arg cracking template
       below.  Note use of initialization - that is good 
       programming practice. */
    double example_real(0.0);
    bool example_boolean(false);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-x")
        {
            ++i;
            if(i>=argc)usage();
            example_real=atof(argv[i]);
        }
        else if(sarg=="-flag")
        {
            /* flags normally set a boolean like this
               true or false */
            example_boolean=true;
        }
        else
            usage();
    }
    /* This template assumes small numbers of control parameters
       that can be readily defined on the command line.  If the
       required parameters are large consider the use of an antelope
       parameter file.   Here is an example of how that would 
       typically be done:

    string pffile("template.pf");
    PfStyleMetadata control=pfread(pffile);
    
       This would typically be combined with a -pf optional argument
       to change the name pffile on the command line. 
    */
    try{
        /* This reader and writer perform the serialization.  
         * The default constructors used in this example connect
         * stdin for input and stdout for output. 
         * Alternatively to read from fname use 
         * TextIOStreamReader inp(fname);
         * and to write to fname use
         * TextIOStreamWriter out(fname);   
         * - In both cases fname should be an std::string for a valid
         *   file name */
        TextIOStreamReader inp;
        TextIOStreamWriter out;
        /* This example uses the ThreeComponentEnsemble object.  
         * Note the same logic can be used for any object that
         * has boost serialization defined. */
        ThreeComponentEnsemble d;
        /* We can loop through a file by one of two methods.   
         * This example uses a while loop.  For reading from a file 
         * instead of stdio one can use a for loop and the number_available
         * method */
        int n(0);
        cerr << "Template seispp unix filter:  copying "
            <<"ThreeComponentSeismogram objects from stdin to stdout"
            <<endl;
        /* Insert your algorithm here.  This example
        simple writes a message for each ensemble it reads */
        while(inp.good())
        {
            d=inp.read<ThreeComponentEnsemble>();
            out.write<ThreeComponentEnsemble>(d);
            ++n;
        }
        cerr << "Total number of ensembles copied ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

