#ifndef _DATABASEHANDLE_H_
#define _DATABASEHANDLE_H_

#include <string>
#include <list>

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
}  // end namespace seispp
#endif
