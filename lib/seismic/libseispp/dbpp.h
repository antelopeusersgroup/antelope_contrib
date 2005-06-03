#ifndef _SEISPP_DATABASE_H
#define _SEISPP_DATABASE_H
#include <string>
#include <list>
#include "db.h"
#include "pf.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
class DatabaseHandle
{
public:
	virtual double get_double(string)=0;
	virtual int get_int(string)=0;
	virtual string get_string(string)=0;
	virtual string get_filename()=0;
	virtual int append()=0;
	virtual void put(string,double)=0;
	virtual void put(string,float)=0;
	virtual void put(string,int)=0;
	virtual void put(string,string)=0;
	virtual void put(string,char *)=0;
	virtual void put(string,const char *)=0;
	virtual int number_tuples()=0;
	virtual int number_attributes()=0;
	virtual void rewind()=0;  // set row pointer to top
	virtual void operator ++()=0;
	virtual void sort(list<string>keys)=0;
	virtual void subset(string)=0;
	virtual void join(string t1, string t2, 
		list<string> keys1,list<string>keys2)=0;
	virtual void group(list<string>group_keys)=0;
};
class DBBundle
{
public:
	int start_record;
	int end_record;
	Dbptr parent;
};
class DatascopeHandle : public DatabaseHandle
{
public:
	DatascopeHandle();
	DatascopeHandle(string dbname,bool readonly);
	DatascopeHandle(string dbname, string pfname, 
			string tag,bool readonly);
	DatascopeHandle(Dbptr db, Pf *pf, string tag);
	DatascopeHandle(DatascopeHandle& dh);
	DatascopeHandle(Dbptr dbi,Dbptr dbip);
	~DatascopeHandle();
	double get_double(string);
	int get_int(string);
	string get_string(string);
	string get_filename();
	void put(string, double);
	void put(string, float);
	void put(string,int);
	void put(string,string);
	void put(string,char *);
	void put(string, const char *);
	int append();
	int current_record(){return(db.record);};
	void set_record(int rec){db.record=rec;};
	void lookup(string tablename);
	// should have an interator function that could also be 
	// implemented as a ++ operator.  For now use this
	void next_record(){++db.record;};
	int number_tuples();
	int number_attributes();
	void rewind(){db.record=0;};
	void sort(list<string>keys);
	void join(string t1, string t2, 
		list<string> keys1,list<string>keys2);
	void natural_join(string t1, string t2);
	void natural_join(string t2);
	void subset(string sstr);
	void group(list<string>group_keys);
	DBBundle get_range();
	DatascopeHandle& operator=(const DatascopeHandle&);
	void operator ++();
	void close();  
	// Could be private, but some processes can be made 
	// faster by using db function calls using this Dbptr
	// also a general escape
	Dbptr db;
	bool is_bundle;
private:
	bool close_on_destruction;
	Dbptr parent_table;
};
}  // end namespace seispp
#endif
