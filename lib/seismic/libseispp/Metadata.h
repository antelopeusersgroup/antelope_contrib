#ifndef _METADATA_H_
#define _METADATA_H_
#include <iostream>
#include <string>
#include <list>
#include <map>
#include <vector>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "AttributeMap.h"
#include "databasehandle.h"

namespace SEISPP 
{
using namespace std;
using namespace SEISPP;

//
//This object is used for selective copy
//
typedef struct Metadata_typedef {
	string tag;
	enum MDtype mdt;
} Metadata_typedef;

typedef list<Metadata_typedef> MetadataList;


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
        Metadata(){};
	Metadata(Pf*);
	Metadata(Pf *pfin, string tag);
	// Depricated.  Earlier version had this
	//Metadata(char *) throw(MetadataParseError);
	Metadata(string) throw(MetadataParseError);
	// New constructor.  Loads content of string to typed
	// content using MetadataList.
	Metadata(string,MetadataList&);
	Metadata(DatabaseHandle&,
		MetadataList&,AttributeMap&) throw(MetadataError);
	// Intentionally default copy constructor for now.  
	// Use of stl map makes coding unnecessary
	//Metadata(const Metadata&);
	// Same for operator = .  Let the compiler generate it.
	//Metadata& operator=(const Metadata& );
	// In this implementation destructor can be defaulted.
	// There is thus no need to declare it.
        // ~Metadata();

        double get_double(string) throw(MetadataGetError);
        int get_int(string)throw(MetadataGetError);
        string get_string(string)throw(MetadataGetError);
	// Note this never throws and exception.  If not found, return false.
        bool get_bool(string);
	//These put new metadata in
	//These use the same name but depend on overloading
        void put(string,double);
        void put(string,int);
        void put(string,bool);
        void put(string,string); 
        void put(string,char *); 
	void remove(string);
	friend ostream& operator<<(ostream&,Metadata&);
	// new method returns ordered object containing
	// ordered pairs of keys and types
	MetadataList keys(); 
protected:
	// Typed methods use appropriate map first.  If the 
	// key is not found in the typed version they try to
	// fetch from mstring and convert 
	map<string,double> mreal;
	map<string,int> mint;
	map<string,bool> mbool;
	map<string,string> mstring;
};

//
// Helpers
//
void copy_selected_metadata(Metadata& mdin, Metadata& mdout, 
	MetadataList& mdlist) throw(MetadataError);
MetadataList pfget_mdlist(Pf *pf,string tag);
Pf *Metadata_to_pf(Metadata& md);

} // End namespace SEISPP declaration
#endif
