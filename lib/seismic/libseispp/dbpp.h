#ifndef _SEISPP_DATABASE_H_
#define _SEISPP_DATABASE_H_
#include <string>
#include <list>
#include <vector>
#include "stock.h"
#include "db.h"
#include "pf.h"
#include "attribute_map.h"
#include "metadata.h"
#include "databasehandle.h"

namespace SEISPP
{
using namespace std;
using namespace SEISPP;

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
	DatascopeHandle(const DatascopeHandle& dh);
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
class DatascopeMatchHandle : public DatascopeHandle
{
public:
	DatascopeMatchHandle(DatascopeHandle& parent,
			string table, 
			list<string> matchkeys,
			AttributeMap am);
	DatascopeMatchHandle(const DatascopeMatchHandle& parent);
	~DatascopeMatchHandle();
	list<int> find(Metadata& md);
private:
	Dbptr dbscratch_record;
	Dbptr dbt;
	// These are always the same at present, but we'll store both
	// partly to clarify the interface.  storage cost is small
	Tbl *kpattern;
	Tbl *tpattern;
	Hook *hook;
	// This stores the stl equivalents of the kpattern and tpattern
	// antelope tbls.  i.e. matchkeys[i] is an expansion of
	// gettbl(kpattern,i)
	vector<AttributeProperties> matchkeys;
	AttributeMap amap;
};
}  // end namespace seispp
#endif
