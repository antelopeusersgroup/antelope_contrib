#include <iostream>
#include <string>
#include "db.h"
using namespace std;

class Velocity_Model_1d
{
public:
	int nlayers;
	double *z,*v,*grad;
	Velocity_Model_1d(){z=NULL;v=NULL;grad=NULL;nlayers=0;};
	Velocity_Model_1d(int n){nlayers=n;
		z=new double[nlayers]; 
		v=new double[nlayers];
		grad=new double[nlayers];};
	Velocity_Model_1d(Dbptr *db,string name, string property);
	Velocity_Model_1d(string fname, string form, string property);
	~Velocity_Model_1d()
	{if(z!=NULL)delete[]z; if(v!=NULL)delete[]v; if(grad!=NULL)delete[]grad;};
	double getv(double zin);
};
class Velocity_Model_error
{
public:
	string message;
	virtual void log_error(){cerr<<"Velocity_Model object error"<<endl;};
};
class Velocity_Model_dberror : public Velocity_Model_error
{
public:
	string name;
	Velocity_Model_dberror(string modname,string mess){
		name=modname;  message=mess;};
	virtual void log_error(){
		cerr<<"Database error accessing velocity model mod1d table "<<name<<endl;
		cerr<<message;
	};
};
class Velocity_Model_ioerror : public Velocity_Model_error
{
public:
	string ioerr;
	Velocity_Model_ioerror(string mess, string ioe){
		message = mess; ioerr = ioe;};
	virtual void log_error(){
		cerr<<"Velocity i/o error" << endl
			<< message << endl
			<< ioerr << endl;
	};
};
