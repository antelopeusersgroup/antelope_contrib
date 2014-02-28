#ifndef _PF_STYLE_METADATA_H_
#define _PF_STYLE_METADATA_H_

#include <list>
#include <string>
#include "Metadata.h"
namespace SEISPP{
using namespace SEISPP;
/*! \brief C++ object version of a parameter file.

   This object encapsulates the Antelope concept of a parameter
file in a single wrapper.   The main constructor is actually 
a procedure called pfread, which does little more than call the
actual primary constructor.   
   Internally this object does not use an antelope Pf at all
directly, but it is a child of Metadata.  Simple attributes
(i.e. key-value pairs) are posted to type specific interal 
map containers (int, float, boolean, and string).   Note the 
parser attempts to guess the type of each value given in the 
obvious ways (periods imply real numbers, e or E imply real 
numbers, etc.) but all simple key-value pairs are also ALWAYS 
duplicated in type specific and string containers.  The get
methods for simple types all first try the type-specific get
and if that fails they use the string container as a fallback. 
This has the bad side effect of causing duplicates to appear in
the output for all keys associated with int, float, and boolean 
values in pfwrite output generated from files parsed with 
the SEISPP::pfread procedure.   I judged this perferable to 
failures created by having the parser incorrectly guess the type 
of something.   Note this would be very prevalent if this were
not done.  e.g. in a Pf booleans can be defined by 0 or 1, which
is impossible to tell from an int without an additional hint.  
    An Antelope Tbl in a pf file is converted to an stl list 
container of stl::string's that contain the input lines.  This is
a strong divergence from the tbl interface of Antelope, but one
I judged a reasonable modernization.   Similarly, Arr's are 
converted to what I am here calling a "branch".   Branches 
are map indexed containers with the key pointing to nested 
versions of this sampe object type. This is in keeping with the way
Arr's are used in antelope, but with an object flavor instead of
the pointer style of pfget_arr.   Thus, get_branch returns a 
PfStyleMetadata object instead of a pointer that has to be memory
managed.  
    A final note about this beast is that the entire thing was 
created with a tacit assumption the object itself is not huge. 
i.e. this implementation may not scale well if applied to very 
large (millions) line pf files.  
\author Gary L. Pavlis, Indiana University
*/
class PfStyleMetadata : public Metadata
{
public:
    /*! Default constructor - does nothing.*/
    PfStyleMetadata():Metadata(){};
    /*! \brief Construct from a set of text lines.

      This is currently the only constructor for this 
      object.   It is assumed the input is a pf file
      stripped for comment lines and null lines. 

      \param lines is the modified version of the text pf file.*/
    PfStyleMetadata(list<string> lines);
    /*!  Standard copy constructor. */
    PfStyleMetadata(const PfStyleMetadata& parent);
    /*! \brief get a Tbl component by key.

      Antelope has the idea of a tbl, which is a list of 
      lines that are parsed independently.  This is the
      get method to extract one of these by its key.

      \param key is the key for the Tbl desired. 
      \exception PfStyleMetadataError will be thrown if the key
         is not present. */
    list<string> get_tbl(const string key);
    /*! \brief used for subtrees (nested Arrs in antelope) 

       This method is used for nested Arr constructs. 
       This returns a copy of the branch defined by key 
       attached to this object.   The original from the parent 
       is retained.   

       \param key is the key used to locate this branch.
       \returns a copy of the contents of the branch linked to key.
       
       \exception PfStyleMetadataError will be thrown if the key is not 
            found. */
    PfStyleMetadata get_branch(const string key);
    /*! Return a list of keys for branches (Arrs) in the pf file. */
    list<string> arr_keys();
    /*! Return a list of keys for Tbls in the pf.*/
    list<string> tbl_keys();
    /*! Standard assignment operator, */
    PfStyleMetadata& operator=(const PfStyleMetadata& parent);
    /*! \brief save result in a pf format.

       This is functionally equivalent to the Antelope pfwrite 
       procedure, but is a member of this object.   A feature of 
       the current implementation is that all simply type parameters 
       will usually be listed twice in the output file.   The reason
       is that the constructor attempts to guess type, but to allow
       for mistakes all simple parameters are also treated as string
       variables so get methods are more robust.   

       \param ofs is a std::ostream (e.g. cout) where the result
          will be written in pf style.   Usually should end in ".pf".
          */
    void pfwrite(ostream& ofs);
private:
    map<string,list<string> > pftbls;
    /* This is used for nested Arrs */
    map<string, PfStyleMetadata> pfbranches;
};
/*! \brief Error class for PfStyleMetadata object.

  This error object is similar to that for Metadata but tags
  all errors cleanly as originating from this child of Metadata. 
  Note SeisppError is a child of std::exception, so catch that 
  to most easily fetch messages coming from this beast. 
  */
class PfStyleMetadataError : public SeisppError
{
    public:
        PfStyleMetadataError(){
            message=string("PfStyleMetadataError->undefined error");
        };
        PfStyleMetadataError(string mess)
        {
            message="PfStyleMetadataError object message="+mess;
        };
        PfStyleMetadataError(const char *mess)
        {
            message=string("PfStyleMetadataError object message=")+mess;
        };
        void log_error(){
            cerr << message<<endl;
        };
};
/*! \brief Create a PfStyleMetadata object from a file.

   This procedure exists to simpify the process of creation of one
   of these objects.   It exists to simplify conversion of existing
   code with a Pf to use this interface instead.  That is, in an
   existing code all that is required is to search for a pfread 
   call and replace it with a call to SEISPP::pfread.  (Note the
   signature is different so the SEISPP namespace scope specifier 
   is not essential, but advised for clarity). 
   Note there is an nontrivial overhead in this procedure as 
   the algorithm pulls in the entire file before starting to parse it.
   Further, because it returns a copy of the result multiple copies
   will be flying around during construction.   This is the potential 
   scaling problem referenced in the introduction for this object.  
   Not a performance issue unless the pf gets huge, which
   currently is never the case.
  \param fname is the file to read (must be full name - no ".pf" assumed)

  \exception SeisppError if there is an i/o problem.
  \exception PfStyleMetadataError is thrown if the constructor fails
  */
PfStyleMetadata pfread(string fname);
} // End SEISPP namespace declaration 
#endif


