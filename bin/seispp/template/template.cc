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
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
/* These are SEISPP includes that are a base requirement 
   to use this template file */
#include "seispp.h"
/* Replace these as needed.  This one is here only to make
   this do nothing template compile for testing configuration */
#include "ensemble.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
/* You shoudl always include a usage procedure like this to trap
   command line parsing problems. */
void usage()
{
    cerr << "usage line goes here"
        <<endl
        << "add short explanation of args here if needed"<<endl;
    exit(-1);
}
/* This is a generic routine to read a single object from 
   a boost text archive.   The object type is defined by
   the type InputObject as the template argument in standard
   C++ convention.   Note we return the object.   If 
   the object you are reading is huge you should consider 
   modifying this to return a pointer or auto_ptr.*/
template <class InputObject> InputObject 
    read_object(boost::archive::text_iarchive& ia)
{
    InputObject d;
    try{
        ia>>d;
    }catch(...)
    {
        /* This template ignores errors thrown by boost 
           and converts to a standard error for the seispp 
           library - perhaps not ideal but what is done here. */
        throw SeisppError(string("read_object failed:  ")
                    + "Check that input file is a boost text archive file");
    }
    return d;
}
/* This generic function is the inverse of read_object.  It writes
an object of type ObjectType to the boost archive stream.  
The write will fail with an exception if serialization is not 
defined for OutputObject. */
template <class OutputObject> void write_object(OutputObject& d,
        boost::archive::text_oarchive& oa)
{
    try {
        oa << d;
    }catch(...)
    {
        throw SeisppError(string("write_object failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
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
        /* This allows input and output of objects through
           the boost serialization mechanism.   This assumes
           input and output of streams of data.  It has only
           been tested on single objects, but in principle it
           should work for arbitrarily complex formats mixing
           text data with boost archive text data. */
        boost::archive::text_iarchive ia(cin);
        boost::archive::text_oarchive oa(cout);
        /* This uses the generic template above to read a single
           object.  This example uses a ThreeComponentEnsemble 
           but any object with serialization define should work
           with this template. Note use of an auto_ptr for 
           efficiency with large objects*/
        ThreeComponentEnsemble d;
        d=read_object<ThreeComponentEnsemble>(ia);
        /* Insert your algorithm here.  This example
        simple writes this message to cerr */
        cerr << "Template seispp unix filter:  copying stdin to stdout"
            <<endl;
        /*This is the output equivalent of read_object.  This 
         is again only an example and should be changed.*/
        write_object<ThreeComponentEnsemble>(d,oa);
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

