#ifndef _METADATA_H_
#define _METADATA_H_
#include <iostream>
#include <string>
#include <list>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
// This line can be removed for Antelope 4.5
extern "C" {
Pf *pfdup(Pf *);
}
using namespace std;
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
	virtual void log_error(){cerr<<"Pf error: "<< message<<endl;}
};
class Metadata_get_error : public Metadata_error
{
public:
	string mdtype;
	string name;
	Metadata_get_error(const string mtd, const string n, const string mess)
		{mdtype=mtd; name=n;  message=mess; };
	virtual void log_error()
	{ cerr<<"Error attempting to extract parameter" << name
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
	// This compiles a large string with pfcompile to load a full pf
        void load_metadata(string) throw(Metadata_parse_error);
	void load_metadata(char *) throw(Metadata_parse_error);
	void print_all_metadata();
	Pf *extract_all_metadata_to_pf();
private:
        Pf *pf;  // Antelope's pf handle
};
//
//This object is used for selective copy
//
enum MDtype {MDreal, MDint, MDstring, MDlist, MDmap, MDboolean};
typedef struct Metadata_typedef {
	string tag;
	enum MDtype mdt;
} Metadata_typedef;

//
// Helpers
//
void copy_selected_metadata(Metadata& mdin, Metadata& mdout, 
		list<Metadata_typedef>& mdlist)
			throw(Metadata_error);

#endif
