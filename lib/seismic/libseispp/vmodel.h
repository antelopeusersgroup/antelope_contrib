#include <iostream>
#include <string>
#include "db.h"
using namespace std;

class Velocity_Model
{
public:
	int nlayers;
	double *z,*v,*grad;
	Velocity_Model(){z=NULL;v=NULL;grad=NULL;nlayers=0;};
	Velocity_Model(int n){nlayers=n;
		z=new double[nlayers]; 
		v=new double[nlayers];
		grad=new double[nlayers];};
	Velocity_Model(Dbptr *db,string name, string property);
	~Velocity_Model()
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
