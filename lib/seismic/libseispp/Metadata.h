#ifndef _METADATA_H_
#define _METADATA_H_
#include <iostream>
#include <sstream>
#include <string>
#include <list>
#include <map>
#include <vector>
#include "stock.h"
#ifndef NO_ANTELOPE
#include "arrays.h"
#include "pf.h"
#include "databasehandle.h"
#include "AttributeMap.h"
#else
/* This is normally defined (probably improperly) in AttributeMap.h.
   In PWMIG namespace this needs to be defined here.  This is a nasty
   potential maintenance issue putting this in two places */
enum MDtype {MDreal, /*!< Attribute is a real (floating point) number */
    MDint, /*!< Attribute is an integer. */
    MDstring, /*!< Attribute is a character string. */
    MDboolean, /*!< Attribute is a boolean (true/false). */
    MDinvalid  /*!< Attribute type is not recognized (an error code). */
};
#endif
#include "SeisppError.h"

namespace SEISPP 
{
using namespace std;
using namespace SEISPP;

//
//This object is used for selective copy
//
/*! 
\brief Used in Metadata to defined type of Metadata associated with
a given tag.
**/
typedef struct Metadata_typedef {
	string tag; /*!< Name attached to this item.*/
	enum MDtype mdt; /*!< Type of this item. */
} Metadata_typedef;

/*!
// Some components of the Metadata object are driven by 
// this STL list.
**/
typedef list<Metadata_typedef> MetadataList;


//
// This follows a method of inherited objects as a way to build
// exception handlers described in books by Stroustrup on C++
//
// This is the base class that is bare bones
//

/*! \brief Base class error object for Metadata.  
**/
class MetadataError : public SeisppError
{
public:
	MetadataError(){message="no detail posted";};
	MetadataError(string mess){message=mess;};
	MetadataError(char *mess){message=mess;};
	void log_error(){
            cerr<<"Metadata error: "<< message<<endl;
        };
};
/*! \brief Error object thrown by get methods.
**/
class MetadataGetError : public SeisppError
{
public:
	string mdtype;
	string name;
	MetadataGetError(const string mtd, const string n, const string mess){
            mdtype=mtd;
            name=n;
            stringstream ess;
            ess << "MetadataGetError: "
               << "Error while attemping to extract attribute with name="
               << n<<" with type="<<mtd<<endl
                << "Detailed message posted:  "<<mess<<endl;
            message=ess.str();
        };
};
/*! \brief Error object thrown if problems are encountered parsing input
in some constructors.
**/
class MetadataParseError : public SeisppError
{
public:
	int error_code;
	MetadataParseError(int ierr,const string mess)
	{
            error_code=ierr;
            stringstream ess;
            ess << "MetadataParseError:  "
                << "pfcompile failed returning error code="
                <<error_code<<endl
                << "Detail message posted:  "
                << mess<<endl;
            message=ess.str();
        };
};

/*! \brief Object to hold auxiliary parameters referenced by a keyword.
*
*  Ancillary data (Metadata) are a common need in data processing.  
*  Most data objects have a set of parameters required to define to all such 
*  objects (e.g. sample rate in a time series).  With real data of
*  any kind there are always ancillary parameters that are required
*  for one algorithm but are not needed by another.  A common solution
*  to this is traditional data processing is a header like that used
*  in seismic data processing.  A Metadata object can be conveniently
*  thought of as a generalized header.  Data can be put into the 
*  Metadata object with constructors and additional data added or replaced by
*  various put methods.  Data is extract with get methods.  
* 
*  This object supports only the standard data types:  integers, 
*  real numbers, booleans, and strings.  The expectation is if one
*  requires a more complicated objects to be associated with another
*  through this mechanism one can readily extend this object by 
*  standard inheritance mechanisms.  It is important to note that
*  data are stored internally in STL map objects indexed by 
*  a keyword.  Reals, ints, and booleans are stored in the machines
*  native form in these maps.  For each of them if a parameter is
*  requested by one of the typed get methods it looks first in the
*  typed container.  If it is not there, it then tries the string 
*  container and throws and exception if that parameter is not their
*  either.  
* 
*  For Antelope users think of a Metadata object as an alternative 
*  interface to a parameter file.  It is, in fact, more or less a 
*  C++ interface to a parameter file.
**/

class Metadata
{
public:
/*!
// Default constructor.  Does nothing but build an empty object.
**/
        Metadata(){};
#ifndef NO_ANTELOPE
/*!
// Construct from an Antelope parameter file.  Note that antelope
// parameter files are stored in a form similar to an STL
// map container of strings keyed by keywords defined by another
// string.  Parameter files used for constructing a Metadata stored
// all the results in the internal string map container.  Requests
// for typed parameters (ints, double, or boolean) placed into the
// Metadata object through this constructor are stored as strings
// internally and converted to the requested type when requested.
//
//\param pf pointer to antelope Pf produced, for example, by something
//  like pfread (see man pf)
**/
	Metadata(Pf* pf);
/*!
// Construct from a tagged component of a larger Antelope parameter file.
//
// It is often useful to associate common sets of names to 
// the same basic object type but with different values.  This
// can be accomplished with this constructor with a nested parameter
// file.  That is, an an antelope parameter file blocks with duplicate
// parameter names can be contained in blocks of the generic form
// tag1 &Arr{ ... parameters...}  tag2 &Arr{ ... parameters ...} etc.
// This allows the contents of only the section between the curly 
// brackets associated with the unique tag (tag1 or tag2 in the 
// above example) to be parsed and stored in the Metadata object.
//\exception MetadataParseError if parsing the parameter file for the 
//   requested tag fails.  
//\param pfin pointer to Antelope Pf to be used to construct this object.
//\param tag keyword of nested &Arr in parameter file to use to parse
//   this Metadata.
**/
	Metadata(Pf *pfin, string tag);
/*!
\brief File and string constructor.

The normal use for this constructor is to construct the object
from a file.   The default is an oddity created by a need for
backward compatibility.   That is, the default (no format specified)
is to construct from a string stored in memory.   The string is
assumed to be an image of an Antelope pf file that is passed through
the pfcompile procedure to construct the object.   Otherwise the
constructor assumes arg 1 is a file name that is read and parsed
with a structure assumption defined by the format name passed as
arg 2.

Note this constructor is depricated and should not be used.  The
intended use has been superceded by child of this class
called PfStyleMetadata.

\exception MeetadataParseError if pfcompile failes.
\param s is one of two things.  If format is string it assumed to 
  be a string that is to be parsed as an antelope pf file image.  Otherwise
  it is assumed to be a file name with the structure defined by format.
\param format is a keyword that defines the format of the file to be read.
  Currently the only recognized name is "pf" for an antelope pf, but this
  is intended to be a general interface.  The default is "string", which 
  is different.  In that case s is assume to be an image of an
  antelope pf file.

*/
	Metadata(string s,const char *format="string") throw(MetadataParseError);
#endif
/*!
//  Restricted build from a string driven by a typed list.  
//  
// Similar to the string constructor, but only Metadata 
// defined by a list (mdl) are copied.  
//
//\param s string to be compiled into Metadata
//\param mdl MetadataList object containing list of items to be copied
//  from string to Metadata contents.  Any contents of the input string
//  s that do not match keys found in this list will be dropped.
**/
	Metadata(string s,MetadataList& mdl);
#ifndef NO_ANTELOPE
/*!
//  Construct from a database.
//
// Constructs a Metadata object from one tuple of an database 
// view.  Attributes to extract are driven by the MetadataList
// with a name mapping through the AttributeMap object.
// 
//\exception MetadataError if there are problems extracting any 
//  requested attribute.
//
//\param dbh generalized database handle assumed to point at a single
//  tuple that is to be parsed.  In the current implementation this is
//  always an Antelope database handle, but the intent here is to 
//  abstract the interface.
//\param mdl is the list of attributes to be extracted.  This list is 
//  pairs of internal names and data types.  
//\param am is an AttributeMap object that defines the name mapping 
//  between database attribute names and internal metadata names
//  to be used inside a given program. For example, wfdisc.time 
//  can be mapped to "t0" or "time" by this mechanism.
**/
	Metadata(DatabaseHandle& dbh,
		MetadataList& mdl,AttributeMap& am) throw(MetadataError);
#endif

/*!
// Standard copy constructor.
//
//\param mdold - parent object to be copied
**/
	Metadata(const Metadata& mdold);

/*! Standard assignment operator.
  \param mdold - parent object to copy 
  */
	Metadata& operator=(const Metadata& mdold);
	// In this implementation destructor can be defaulted.
	// There is thus no need to declare it.
        // ~Metadata();

/*!
// Get a real number from the Metadata object.  
// 
//\exception MetadataGetError if requested parameter is not found.
//\param key keyword associated with requested metadata member.
**/
        double get_double(string key) throw(MetadataGetError);
/*!
// Get an integer from the Metadata object.  
// 
//\exception MetadataGetError if requested parameter is not found.
//\param key keyword associated with requested metadata member.
**/
        int get_int(string key)throw(MetadataGetError);
/*!
// Get a long integer from the Metadata object.  
// 
//\exception MetadataGetError if requested parameter is not found.
//\param key keyword associated with requested metadata member.
**/
        long get_long(string key)throw(MetadataGetError);
/*!
// Get a string from the Metadata object.  
// 
// Note the string in this case can be quite large.  If the string
// was parsed from an Antelope Pf nested Tbl and Arrs can be extracted
// this way and parsed with pf routines.
//
//\exception MetadataGetError if requested parameter is not found.
//\param key keyword associated with requested metadata member.
**/
        string get_string(string key)throw(MetadataGetError);
/*!
// Get a  boolean parameter from the Metadata object.  
// 
// This method never throws an exception assuming that if the
// requested parameter is not found it is false.
//
//\param key keyword associated with requested metadata member.
**/
        bool get_bool(string key);
/*!
// Place a real number into the Metadata object.
// 
// Inserts a new, real number parameter into the Metadata object.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,double val);
/*!
// Place a long integer into the Metadata object.
// 
// Inserts a new, integer parameter into the Metadata object.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,long val);
/*!
// Place an integer into the Metadata object.
// 
// Inserts a new, integer parameter into the Metadata object.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,int val);
/*!
// Place a boolean parameter into the Metadata object.
// 
// Inserts a boolean parameter into the Metadata object.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,bool val);
/*!
// Place a string parameter the Metadata object.
// 
// Inserts a new string parameter into the Metadata object.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,string val); 
/*!
// Place a string parameter into the Metadata object.
// 
// Inserts a new string parameter into the Metadata object.
// Differs from similar method with C++ string in that this passes
// a plan C char *.  The char * is converted to C++ string internally.
// If the parameter was present before it will be replaced.  If not
// it will be inserted.  
//
//\param key keyword to be used to reference this parameter.
//\param val value to load.
**/
        void put(string key,char * val); 
/*! \brief Query to find out if an attribute is set.
//
// It is frequently necessary to ask if an attribute has been set.
// The get routines throw an exception if one tries to fetch an attribute
// that is not defined, which is always trouble.  Rather than depend
// on exception handlers, which is bad form, a program that cannot
// be certain an attribute is defined should call this method instead
// of using an error handler.  It is both faster and better form.
//
// Note the algorithm used is independent of type simply searching 
// the containers that hold each type stored by this object.
//
// \param key attribute to be test.  
*/
	bool is_attribute_set(string key);
/*! \brief Query to find out if an attribute is set.
//
// It is frequently necessary to ask if an attribute has been set.
// The get routines throw an exception if one tries to fetch an attribute
// that is not defined, which is always trouble.  Rather than depend
// on exception handlers, which is bad form, a program that cannot
// be certain an attribute is defined should call this method instead
// of using an error handler.  It is both faster and better form.
// This overloaded form is a convenience for testing using char constants
// (the standard result of a string between double quotes.).
//
// \param key attribute to be test.  
*/
	bool is_attribute_set(char *val);
/*!
// Delete a parameter from the Metadata object.
// 
//\param key keyword tagging parameter to be removed.
**/
	void remove(string key);
/*!
// Appends a string to an existing string value with a separator.
// 
// It is frequently useful to append a new string to an existing
// string variable stored in a Metadata object.  This can be used,
// for example to build up file names.  A real example in this 
// library at the moment is that this is used to accumulate filter
// parameters when multiple filters are cascaded on data in the 
// TimeInvariantFilter object.  
// 
// This could be done by a get and put, but this automates the process.
// Note that if the key passed was not present in the Metadata object
// before this method is called the separator is ignored and only the 
// third argument becomes the value associated with key.
//
//\param key keyword to access string.   
//\param separator string used to separate new string from previous
//   contents.  Note that if the key passed was not present in the Metadata object
//   before this method is called the separator is ignored and only the
//   third argument becomes the value associated with key.
//\param appendage this string is appended to the current contents subject to 
//   special case noted above for separator parameter.
**/
	void append_string(string key, string separator, string appendage);
/*!
// Output function to a standard stream.
//
// Output format is an Antelope parameter file. 
**/
	friend ostream& operator<<(ostream&,Metadata&);
/*!
// Return a list of keys and associated types.
**/
	MetadataList keys(); 
protected:
	// Typed methods use appropriate map first.  If the 
	// key is not found in the typed version they try to
	// fetch from mstring and convert 
	map<string,double> mreal;
	map<string,long> mint;
	map<string,bool> mbool;
	map<string,string> mstring;
};

//
// Helpers
//
/*!
// Copy only selected Metadata from one Metadata object to another.
//
//\param mdin input Metadata (copy from here).
//\param mdout target to which desired Metadata is to be copied.
//\param mdlist object containing a typed list of Metadata components
//  to copy from mdin to mdout.
**/
void copy_selected_metadata(Metadata& mdin, Metadata& mdout, 
	MetadataList& mdlist) throw(MetadataError);
#ifndef NO_ANTELOPE
/*!
// Build a MetadataList from a parameter file.
//
// This is essentially a constructor for the MetadataList structure.
//
//\param pf pointer to Pf to be parsed (normally produced by pfread
//   of a parameter file.
//\param tag key of Tbl in Pf holding the list.  
**/
MetadataList pfget_mdlist(Pf *pf,const string tag);
/*!
// Convert a Metadata to an Antelope Pf.  This is essentially
// an inverse to the Pf constructor.  
//
//\param md Metadata to be converted.
//\return Antelope parameter file Pf pointer.
**/
Pf *Metadata_to_pf(Metadata& md);
/*!  \brief Extract an antelope Pf Tbl into a string.

  Antelope pf files have the concept of a Tbl grouping of stuff
  that is commonly parsed by programs for data that is not a simple
  single value type.   This procedure finds a Tbl with a specified
  tag and extracts the Tbl contents into a string which is returned.

\param pf  is the Antelope Pf pointer 
\param tag is the tag for the Tbl to be extracted

\return string of Tbl contents. 
*/
string pftbl2string(Pf *pf, const char *tag);
/*!  \brief Extract an antelope Pf Tbl into a list of strings.

  Antelope pf files have the concept of a Tbl grouping of stuff
  that is commonly parsed by programs for data that is not a simple
  single value type.   This procedure finds a Tbl with a specified
  tag and extracts the Tbl contents into list container.   Each 
  string in this list is defined by newlines in the original Tbl of
  the pf file.  Said another way the basic algorithm is a gettbl for
  each line in the Tbl followed by a push_back to the STL list.

\param pf  is the Antelope Pf pointer 
\param tag is the tag for the Tbl to be extracted

\return STL list container of std::string objects derived from tbl lines.
*/
list<string> pftbl2list(Pf *pf, const char *tag);
#endif


} // End namespace SEISPP declaration
#endif
