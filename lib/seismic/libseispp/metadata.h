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
//
//This object is used for selective copy
//
enum MDtype {MDreal, MDint, MDstring, MDlist, MDmap, MDboolean, MDinvalid};
typedef struct Metadata_typedef {
	string tag;
	enum MDtype mdt;
} Metadata_typedef;

typedef list<Metadata_typedef> Metadata_list;

// Constructors of a Metadata from a database are driven by
// the following pair of objects.  Attribute_Properties provides
// the data to map from db namespace to an internal namespace.
// Attribute_Map provides maintains the actual map of Attribute_Properties
// objects keyed by a string = Attribute_Properties::internal_name

class Attribute_Properties
{
public:
	string db_attribute_name;
	string db_table_name;
	string internal_name;
	MDtype mdt;
	bool is_key;
	Attribute_Properties();
	Attribute_Properties(string);// main constructor parses string 
	Attribute_Properties(const Attribute_Properties&);
	Attribute_Properties& operator=(const Attribute_Properties&);
};

class Attribute_Map
{
public:
	map<string,Attribute_Properties> attributes;

	Attribute_Map();  
	Attribute_Map(Pf *pf,string name);  
	Attribute_Map(string);
	Attribute_Map(const Attribute_Map&);
	Attribute_Map& operator=(const Attribute_Map&);
};


//
// This follows a method of inherited objects as a way to build
// exception handlers described in books by Stroustrup on C++
//
// This is the base class that is bare bones
//
class Metadata_error
{
public:
	string message;
	Metadata_error(){message="Metadata error";};
	Metadata_error(string mess){message=mess;};
	Metadata_error(char *mess){message=mess;};
	virtual void log_error(){cerr<<"Metadata error: "<< message<<endl;}
};
class Metadata_get_error : public Metadata_error
{
public:
	string mdtype;
	string name;
	Metadata_get_error(const string mtd, const string n, const string mess)
		{mdtype=mtd; name=n;  message=mess; };
	virtual void log_error()
	{ cerr<<"Error attempting to extract parameter " << name
		<< " of type " << mdtype << "\n" << message << endl;};
};
class Metadata_parse_error : public Metadata_error
{
public:
	int error_code;
	Metadata_parse_error(int ierr,const string mess)
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
	Metadata(char *) throw(Metadata_parse_error);
	Metadata(string) throw(Metadata_parse_error);
	Metadata(Database_Handle&,
		Metadata_list&,Attribute_Map&) throw(Metadata_error);
	Metadata(const Metadata&)
		throw(Metadata_parse_error);
	Metadata& operator=(const Metadata& );
        ~Metadata();

        double get_double(string) throw(Metadata_get_error);
        int get_int(string)throw(Metadata_get_error);
        string get_string(string)throw(Metadata_get_error);
        bool get_bool(string)throw(Metadata_get_error);
	// The next two are antelope specific
	Tbl *get_list(string)throw(Metadata_get_error);
	Arr *get_map(string)throw(Metadata_get_error);
	//These put new metadata in
	//These use the same name but depend on overloading
        void put_metadata(string,double);
        void put_metadata(string,int);
        void put_metadata(string,string); // C++ basic string 
        void put_metadata(string,char *);  // for plain C strings
        void put_metadata(string,bool);
	void put_metadata(string,Arr *);  // antelope map
	void put_metadata(string,Tbl *);  // antelope list
	void remove(string);
	friend ostream& operator<<(ostream&,Metadata&);
	void print_all_metadata();
	Pf *extract_all_metadata_to_pf();
protected:
        Pf *pf;  // Antelope's pf handle
};

//
// Helpers
//
void copy_selected_metadata(Metadata& mdin, Metadata& mdout, 
	Metadata_list& mdlist) throw(Metadata_error);
Metadata_list pfget_mdlist(Pf *pf,string tag);

}
#endif
