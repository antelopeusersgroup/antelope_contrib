#ifdef __cplusplus
#ifndef _VMODEL_H
#define _VMODEL_H

#include <iostream>
#include <string>
#include "db.h"
using namespace std;
namespace SEISPP 
{

class Velocity_Model_1d
{
public:
	int nlayers;
	vector<double> z,v,grad;
	Velocity_Model_1d(){z.reserve(0);v.reserve(0);grad.reserve(0);nlayers=0;};
	Velocity_Model_1d(int n){nlayers=n;
		z.reserve(nlayers);
		v.reserve(nlayers);
		grad.reserve(nlayers);};
	Velocity_Model_1d(Dbptr *db,string name, string property);
	Velocity_Model_1d(string fname, string form, string property);
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
}
#endif
