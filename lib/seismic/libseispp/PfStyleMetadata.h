#ifndef _PF_STYLE_METADATA_H_
#define _PF_STYLE_METADATA_H_

#include <list>
#include <string>
#include "Metadata.h"
namespace SEISPP{
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
    /* \brief used for subtrees (nested Arrs in antelope) 

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
    /* \brief save result in a pf format.

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
   of these objects.   It is a necessary evil because of a limitation
   in the current C++ standard that does not allow one constructor
   to call another for the same object.   I could not figure out a way
   to do this without a lot of duplicate code, so I elected to make this
   a procedure.  Not a performance issue unless the pf gets huge, which
   currently is never the case.
  \param fname is the file to read (must be full name - no ".pf" assumed)

  \exception SeisppError if there is an i/o problem.
  \exception PfStyleMetadataError is thrown if the constructor fails
  */
PfStyleMetadata pfread(string fname);
} // End SEISPP namespace declaration 
#endif


