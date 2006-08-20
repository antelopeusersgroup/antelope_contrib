#ifndef _ATTRIBUTE_MAP_H_
#define _ATTRIBUTE_MAP_H_
#include <string>
#include <map>
#include "stock.h"
#include "pf.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*!
* Lists valid types of Attributes that can be stored as Metadata.
*/

enum MDtype {MDreal, /*!< Attribute is a real (floating point) number */
	MDint, /*!< Attribute is an integer. */
	MDstring, /*!< Attribute is a character string. */
	MDboolean, /*!< Attribute is a boolean (true/false). */
	MDinvalid  /*!< Attribute type is not recognized (an error code). */
};


/*! \brief  Defines properties of a database attribute and link to internal names.
*
* This object is used to define the relationship between a parameter stored 
* externally (originally conceived as a database attribute general enough for
* any externally imposed naming convention) with a particular name with some
* internal naming convention.  This object defines the relationship for one
* internal parameter and it's external properties.  Arrays of these objects 
* or (as used in the AttributeMap object in SEISPP) associate arrays can be
* used to define relationships for a complete set of parameter names.  
*/

class AttributeProperties
{
public:
	/*! Name for this parameter as the attribute name in some database table.*/
	string db_attribute_name;
	/*! \brief Database table name from which this attribute was loaded.
	* Relational databases always share attribute names with multiple tables.
	* This is a fundamental axiom in relational database theory.  Because 
	* an attribute of the same name may or may not be related in different tables
	* it is necessary to define the parent table to avoid ambiguity.  
	* Note that in SEISPP a common convention is that a db_attribute_name may 
	* contain the parent table implicity (e.g. wfdisc.sta) but code using this
	* object should not assume this.  User should assume the correct table name
	* is stored here. */
	string db_table_name;
	/*! Internal name used to reference this parameter.*/
	string internal_name;
	/*! Type of this parameter.*/
	MDtype mdt;
	/*! \brief Defines if parameter is a key in it's parent table.
	* Keys in a relational database are special attributes and it is useful
	* to tag a parameter as being a key or not a key.  When true this attribute
	* indicates a parameter is a key for db_table_name. */
	bool is_key;
	/*! Default constructor.  Creates a null object with MTtype set to invalid.
	*  This constructor exists to avoid automatic construction with no parameters,
	*  but it should not be used. */
	AttributeProperties();
	/*! \brief Create using a string description.
	* The standard construction of an associative array uses this constructor driven 
	* by a series of input lines.  The input line that is parsed must contain these
	* member attributes in the following order:
	* -# internal_name
	* -# db_attribute_name
	* -# db_table_name
	* -# mdt
	*
	* The mdt field must be one of "REAL", "real", "STRING", "string", "INT", "int",
	* "BOOL", "BOOLEAN", "bool", or "boolean".
	* Any other field with cause this constructor to throw an exception.  
	* \exception MetadataError is thrown for parsing errors.
	*/
	AttributeProperties(string);
	/*! \brief Standard copy constructor. */
	AttributeProperties(const AttributeProperties&);
	/*! \brief Standard assignment operator. */	
	AttributeProperties& operator=(const AttributeProperties&);
};
/*! \brief Associative array to map internal to external naming conventions. 
* 
* This object is used to link a set of internally defined parameters tagged with 
* a name to an external name convention.  The working model for external names is
* attribute names defined in a relational database schema, but the concept 
* involved is more general.  That is, the intent of this interface is a general
* way to between one set of parameter names and another.  This could be used,
* for example, to map between header variable names in SEGY or SAC and some
* internal name convention.  The relation of the map defined by this object is
* implicitly assumed to be one-to-one because of the use of the STL map to 
* define the relationship.  Because the map is keyed by the internal name 
* lookup is also intended only for finding the external names associated with a
* particular internal parameter.  The primary use of this object in the SEISPP
* library is to define a global mapping operator for a particular database 
* schema.  That is, the most common construct is to build this object 
* early on using a call like:  AttributeMap("css3.0").   
*
*/
class AttributeMap
{
public:
	/*! \brief Maps internal to external names.
	*  This object is little more than a wrapper around this Standard Template
	* library map container.  The map is keyed by the internal name used to 
	* for a particular parameter.  Each internal name known to the object will
	* have an AttributeProperties associated with it through this map 
	* (same as an associative array for perl and tcl geeks).  The user will
	* need to be familiar with the STL map container to deal with this object
	* correctly.  I made an intentional design decision to not hide this 
	* beast behind the interface because the STL has become a standardized 
	* component of C++.  I took the attitude that STL would outlast my interface
	* definition and cost of hiding this was too high in computational burden.
	* The key thing a user must know is the proper way to retrieve an element from
	* a map and handle the possibility that the requested item is not known to 
	* the map.  Consult the web or the source code for libseispp if you don't 
	* know how to do this.  */
	map<string,AttributeProperties> attributes;
	/*! \brief Default constructor.
	*  The default assumes the css3.0 schema and will load the name definitions
	*  defined for that schema.  */
	AttributeMap();  
	/*! \brief Create mapping for a specified namespace tag (usually a schema name). 
	*
	*  This is the normal constructor for this object.  A one word tag is used to 
	* define a particular title to a namespace mapping.  Normally this is a 
	* database schema name like css3.0 or Trace4.0, but the interface allows it
	* to be anything.  For example, although it isn't currently defined one could
	* easily create a "SacHeader" definition that defined mapping between SAC 
	* header fields and an internal name convention.  The interface simply assumes
	* this keyword can be used to establish a mechanism for creating this beast
	* through an unspecified mechanism.  i.e. the interface is blind to the 
	* details and assumes what you want is to know how to map between A and B
	* and someone else worried about the format for doing this already.  In the
	* current implementation we use an Antelope parameter file to create this 
	* object, but this interface does not depend upon that choice. 
	*
	* \param tag name tag used to define this map (usually a schema name).
	*/
	AttributeMap(string tag);
	/*! Standard copy constructor.*/
	AttributeMap(const AttributeMap& am0);
	/*! Standard assignment operator.*/
	AttributeMap& operator=(const AttributeMap& am0);
private:
	/*! Parameter file driven constructor.
	*  Builds this object from an Antelope parameter file.  Parses the
	* parameter for for a Tbl that is processed line by line to calling
	* the basic constructor for an AttributeProperties with one
	* AttributeProperties object created for each entry in the associated
	* Tbl. 
	* \param pf Parameter file containing Tbl to parse.
	* \param name name of Tbl to be parse.
	*/
	AttributeMap(Pf *pf,string name);  
};

} // End namespace SEISPP declaration

#endif
