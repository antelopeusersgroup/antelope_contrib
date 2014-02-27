#ifndef _HEADERMAP_H_
#define _HEADERMAP_H_
#include <string>
#include <map>
#include <iostream>
#include <typeinfo>
#include "stock.h"
#include "pf.h"
#include "seispp.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;

/*! \brief Defines properties for attributes stored in fixed length, 
	binary headers.

* Fixed length binary headers are a common method used to store
* data attributes (Metadata) associated with a larger data object
* to which the header relates.  This object encapsulates things
* need to describe such an entity in a general way.  It is 
* intimately linked to the HeaderMap object that uses an STL
* map to link one of these to parameters in a header tagged by
* a unique name. */
class HeaderAttributeProperties
{
public:
	HeaderAttributeProperties();
	HeaderAttributeProperties(string nm, AttributeType dtype, int nb, int o);
	HeaderAttributeProperties(char *line);
	string name;
	AttributeType dtype;
	int nbytes;
	size_t offset;
};
/*! Defines the layout of a fixed length binary header.

* Fixed length binary headers are a common method used to store
* data attributes (Metadata) associated with a larger data object
* to which the header relates.  This object is designed to be
* a general way to define the layout of binary headers and allow
* accessing the contents of these headers through a simple
* API definition parallel to the Metadata object methods in 
* the SEISPP library.  This object is of limited use alone, but
* is most useful as a base class (superclass) to specific
* data objects like a SEGYTrace.  
*/
class HeaderMap
{
public:
	HeaderMap();
	HeaderMap(Pf *pf, string tag);
	HeaderMap(const HeaderMap& parent);
	HeaderMap& operator=(const HeaderMap& parent);
	template <class T> T get(string name,unsigned char *h);
	string get_string(string name, unsigned char *h);
	bool get_bool(string name,unsigned char *h);
	template <class T> void put(string name, T value,unsigned char *h);
	void put_string(string name,string value,unsigned char *h);
	void put_string(string name,char *value,unsigned char *h);
	void put_bool(string name,bool value,unsigned char *h);
	size_t size(){return(headersize);};
	AttributeType dtype(string name);
protected:
	size_t headersize;
	map<string,HeaderAttributeProperties> attributes;
	int boolean_true;
	int boolean_false;
	bool boolean_default;
};
/*! \brief Generic method to extract a binary glob of type T from a header. 


Fetching an attribute from a header is a common need in data processing
with data having a header.  This is a generic api to extract such
a thing by name. */


template <class T> T HeaderMap::get(string name,unsigned char *headerdata)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(string("HeaderMap::get attribute name=")
			+ name
			+ string("is not defined for this header type"));
	int nb=ithm->second.nbytes;
	if(sizeof(T)!=nb)
		throw SeisppError(string("HeaderMap::get attribute name=")
			+ name 
			+ string("type size mismatch") );
	++nb;  // Need to add one to allow for trailing null
	size_t o=ithm->second.offset;
	T *val;
	val=reinterpret_cast<T*>(headerdata+o);
	return(*val);
}
/*! \brief Generic method to place a binary glob of type T to a header. 


Posting an attribute to a fixed format binary header is a common 
need in dealing with external data formats.   This is a generic api to 
post an attribute of type T to a header using a name key.
*/
template <class T> void HeaderMap::put(string name, 
	T value,unsigned char *headerdata)

{
	const string base_error_message("HeadeMap:put attribute name=");
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(base_error_message
			+ name
			+ string(" is not defined for this header type"));
	int nb=ithm->second.nbytes;
	if(sizeof(T)!=nb)
		throw SeisppError(base_error_message
			+ name 
			+ string(" type size mismatch") );
	switch (ithm->second.dtype)
	{
	case STRING:
		throw SeisppError(base_error_message
			+ name
			+ string(" coding error.  Cannot use this template method with string attribute") );
	case HDRINVALID:
        default:
		throw SeisppError(base_error_message
			+ name
			+ string(" Attribute is marked invalid") );
	}
	size_t o=ithm->second.offset;
	T *hptr;
	unsigned char *a;
	a=headerdata+o;
	hptr=reinterpret_cast<T*>(a);
	*hptr=value;
}
} // End SEISPP namespace declaration
#endif
