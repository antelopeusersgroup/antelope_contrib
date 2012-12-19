#include "GenericFileHandle.h"
using namespace std;
using namespace SEISPP;
/* \brief Special GenericFileHandle for SAC files.

   This handle can be used to read a SAC file.  SAC has no
   concept of ensembles so it has only a method to read 
   a single seismogram.   At this time it also intentionally
   has no write method as I refuse to perpetuate such an 
   abomination.  There are also plenty of other tools to 
   write sac files. 
   */

class SacFileHandle : public GenericFileHandle
{
public:
    /* \brief Standard constructor with simple interface. 

       This will construct a handle to a Sac file in read only
       mode using standard mapping defined in a default parameter 
       file.   Most users will want to use this constructor.
       This beast has the overhead of loading all sac header fields.
\param filename is the name of the Sac file to be read.
\exception SeisppError object is thrown if there are any problems.
*/
    SacFileHandle(string filename);
/* \brief Fully parameterized constructor. 

The actual algorithm used to read a sac file with this object
uses a GenericFileHandle with significant complexity used to make
it general.  Ones needed to define a Sac file can be specified
by this constructor if one wants to change the naming convention from
the default behaviour. 
\param filename is the name of the SAC file to be read
\param namemap is an object used to cross reference between internal 
   names and standard SAC header names.  There is a standard seispp
   constructor for this.
\param mdlist is a list of header fields that should be loaded from the file.
\param ensk should be an empty list because SAC has no concept of an ensemble.
\param ensmdl should be an empty list because SAC has no concept of an ensemble.
\exception SeisppError will be thrown if there are any problems.
*/

    SacFileHandle(string filename,AttributeCrossReference& namemap,
                list<string> mdlist,list<string> ensk, list<string>ensmdl);
/*! \brief Reads the sac file and puts contents into a TimeSeries object.

  Because SAC files only contain one seismogram this is the only method
  in this object.  It simply reads the file contents and loads the 
  specified header fields into the Metadata area of the object.  
  Attribute tags are internal names defined by the AttributeCrossReference
  object.  These are defined in the SacFileHandle.pf file. 
  */
    TimeSeries GetNextSeismogram();
};
