#ifndef _METADATA_H_
#define _METADATA_H_
#include <iostream>
#include <string>
#include <list>
#include <map>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "dbpp.h"

namespace SEISPP 
{
using namespace std;
using namespace SEISPP;
// Holds global default metadata
//
//This object is used for selective copy
//
enum MDtype {MDreal, MDint, MDstring, MDlist, MDmap, MDboolean, MDinvalid};
typedef struct Metadata_typedef {
	string tag;
	enum MDtype mdt;
} Metadata_typedef;

typedef list<Metadata_typedef> MetadataList;

// Constructors of a Metadata from a database are driven by
// the following pair of objects.  AttributeProperties provides
// the data to map from db namespace to an internal namespace.
// AttributeMap provides maintains the actual map of AttributeProperties
// objects keyed by a string = AttributeProperties::internal_name

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


//
// This follows a method of inherited objects as a way to build
// exception handlers described in books by Stroustrup on C++
//
// This is the base class that is bare bones
//
class MetadataError
{
public:
	string message;
	MetadataError(){message="Metadata error";};
	MetadataError(string mess){message=mess;};
	MetadataError(char *mess){message=mess;};
	virtual void log_error(){cerr<<"Metadata error: "<< message<<endl;}
};
class MetadataGetError : public MetadataError
{
public:
	string mdtype;
	string name;
	MetadataGetError(const string mtd, const string n, const string mess)
		{mdtype=mtd; name=n;  message=mess; };
	virtual void log_error()
	{ cerr<<"Error attempting to extract parameter " << name
		<< " of type " << mdtype << "\n" << message << endl;};
};
class MetadataParseError : public MetadataError
{
public:
	int error_code;
	MetadataParseError(int ierr,const string mess)
	{error_code=ierr;message=mess;};
	virtual void log_error()
	{cerr<<"pfcompile failed with return code="
		<<error_code<<endl;
	cerr<<message<<endl;
	};
};

class Metadata
{
public:
        Metadata();
	Metadata(Pf*);
	Metadata(Pf *pfin, string tag);
	Metadata(char *) throw(MetadataParseError);
	Metadata(string) throw(MetadataParseError);
	Metadata(DatabaseHandle&,
		MetadataList&,AttributeMap&) throw(MetadataError);
	Metadata(const Metadata&)
		throw(MetadataParseError);
	Metadata& operator=(const Metadata& );
        ~Metadata();

        double get_double(string) throw(MetadataGetError);
        int get_int(string)throw(MetadataGetError);
        string get_string(string)throw(MetadataGetError);
        bool get_bool(string)throw(MetadataGetError);
	// The next two are antelope specific
	Tbl *get_list(string)throw(MetadataGetError);
	Arr *get_map(string)throw(MetadataGetError);
	//These put new metadata in
	//These use the same name but depend on overloading
        void put(string,double);
        void put(string,int);
        void put(string,string); // C++ basic string 
        void put(string,char *);  // for plain C strings
        void put(string,bool);
	void put(string,Arr *);  // antelope map
	void put(string,Tbl *);  // antelope list
	void remove(string);
	friend ostream& operator<<(ostream&,Metadata&);
	Pf *extract_all_metadata_to_pf();
protected:
        Pf *pf;  // Antelope's pf handle
};

//
// Helpers
//
void copy_selected_metadata(Metadata& mdin, Metadata& mdout, 
	MetadataList& mdlist) throw(MetadataError);
MetadataList pfget_mdlist(Pf *pf,string tag);

}
#endif
