/* This is a template file for building a unix style filter for 
the seispp library using boost serialization.  This example 
allows for serialization with boost's text or binary archives, but 
the interface insulates the application from this detail.  i.e. it is 
a good example of how the interface separates the concept from implementation
details.   boost has an xml serialization, for example, that could be
easily added.

This template illustrates the use of the seispp_io template library that
abstracts the read and write interface.   This is a template that can
be used as the framework for a family of unix style filters to process
data in a seismic unix style.  Modules that uses this concept can 
be found in directories at the same level as this one.   

Thie example centers on the use of ThreeComponentEnsemble objects, but
the template library allows the same concept to be extended to any object
that can be serialized with boost's C++ Archive objects.

This example simply copies a serialized ThreeComponentEnsemble from 
stdin to stdout.   make should build it under the name template. 
This is a useless module, but the idea is to make it easier for 
others to plug into this framework.   Copy this template, add your 
own algorithm, hack the code where required, modify the makefiles, and 
you have your own seismic unix style module without the dependency on
seismic unix format. 

Instuctional comments are embedded in this template.   Once you have read
these the best use of this template is to copy the file template_plain.cc
which has all comments removed. 
*/

/* This set of system includes are always required.  Do not remove them.*/
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
/* This is needed for shared_ptr, which is part of the 2011 standard forward.
 * For older compilers this may not work without using a differnt template
 * library */
#include <memory>
/* These are SEISPP includes that are a base requirement 
   to use this template file */
#include "seispp.h"
/* Replace these as needed.  This one is here only to make
   this do nothing template work for ThreeComponentEnsemble objects */
#include "ensemble.h"
/* These two files define generic readers and writers used in this package*/
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
/* You will get lots of errors without these namespace
   declaration*/
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
/* You should always include a usage procedure like this to trap
   command line parsing problems. Modify as appropriate.*/
void usage()
{
    cerr << "template < in > out [-v --help -text]"
        <<endl
        << "Example, do nothing filter using seismic unix style pipeline"<<endl
        << "Reads serialized ThreeComponentEnsemble objects from stdin"<<endl
        << "and writes a copy to stdout"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* This obnoxious external variable is a necessary evil to deal with 
   error logging in the SEISPP library. Your code will probably not link 
   without it.*/ 
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    /* Common variables for a program common appear here, but 
       C/C++ now allow later declarations that usually make 
       cleaner code.   We need this counter for the arg list below*/
    int i;
    /* As the name implies set this to the number of required 
       args */
    const int narg_required(0);
    /* This is needed to allow prog --help to work correctly
       if if there are no args */
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    /* Here you should crack required args.  For example:
       string dbname(argv[1]);
       */
    /* Follow this by global declarations related to arg 
       list.  This uses some unused generic examples along
       with the options listed in the usage procedure above.
       Note use of C++ style initialization - that is good 
       programming practice. */
    double example_real(0.0);
    bool example_boolean(false);
    bool binary_data(true);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            /* Good practice is to always have this option
             * to print usage line */
            usage();
        }
        else if(sarg=="-x")
        {
            /* this is an example not actually used in this program */
            ++i;
            if(i>=argc)usage();
            example_real=atof(argv[i]);
        }
        else if(sarg=="-flag")
        {
            /* this is an example not actually used in this program.
            A flag normally should to used to set a boolean like this
               true or false */
            example_boolean=true;
        }
        /* This one is actually used */
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
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
        /* This block should be cloned exactly for any filter to 
         * read and write ThreeComponentSeismogram objects.   
         * If only a reader delete the write portion and if only 
         * a writer delete the read portion.   To read and write
         * different objects replaced ThreeComponenEnsemble with
         * another valid type (e.g. TimeSeriesEnsemble). 
         *
         * Both switch input/output with the binary switch.  Readers
         * will throw a SeisppError exception if you attempt to read
         * a text file as binary or vice versa.  
         *
         * I used a shared_ptr as good practice here as a shared_ptr
         * provides automatic memory management.   My example uses
         * a memory managed pointer as copying i/o handles to procedures
         * is problematic, and more cleanly managed with a memory managed
         * pointer (at least in the author's opinion). 
         *
         * Also note this construct uses a constructor that assumes input
         * from stdin and output to stdout.  These generic handles have
         * alternative constructors to read and write to files with a 
         * name argument.  See the doxygen pages for SEISPP for details. */
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
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
            <<"ThreeComponentEnsemble object from stdin to stdout"
            <<endl;
        if(binary_data)
            cerr << "Assuming binary format data"<<endl;
        else
            cerr << "Assuming ascii formatted data"<<endl;
        /* Insert your algorithm here.  This example
        simple writes a message for each ensemble it reads */
        while(inp->good())
        {
            d=inp->read();
            out->write(d);
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

