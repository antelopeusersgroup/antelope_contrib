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
//Probably should be in Metadata.h
enum MDtype {MDreal, MDint, MDstring, MDboolean, MDinvalid};

// This pair of objects link internal parameter names contained 
// (in a Metadata object in this implementation, but it could be
// more general) within a program with external database attribute
// names.  AttributeProperties define the link for a single 
// parameter between the two namespaces.  AttributeMap 
// defines relationship for many parameters through an std map
// container.

class AttributeProperties
{
public:
	string db_attribute_name;
	string db_table_name;
	string internal_name;
	MDtype mdt;
	bool is_key;
	AttributeProperties();
	AttributeProperties(string);// main constructor parses string 
	AttributeProperties(const AttributeProperties&);
	AttributeProperties& operator=(const AttributeProperties&);
};

class AttributeMap
{
public:
	map<string,AttributeProperties> attributes;

	AttributeMap();  
	AttributeMap(Pf *pf,string name);  
	AttributeMap(string);
	AttributeMap(const AttributeMap&);
	AttributeMap& operator=(const AttributeMap&);
};

} // End namespace SEISPP declaration

#endif
