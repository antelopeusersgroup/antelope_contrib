#include "HeaderMap.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
HeaderAttributeProperties::HeaderAttributeProperties()
{
	name=string("INVALID");
	dtype=HDRINVALID;
	nbytes=0;
	offset=0;
}
HeaderAttributeProperties::HeaderAttributeProperties(string nm, AttributeType hdrdtype, int nb, int o)
{
	name=nm;
	dtype=hdrdtype;
	nbytes=nb;
	offset=(size_t) o;
}

HeaderAttributeProperties::HeaderAttributeProperties(char *line)
{
	const string base_error("HeaderAttributeProperties constructor: ");
	char cnm[64];
	char tpnm[64];
	int nb,off;
	sscanf(line,"%s%s%d%d",cnm,tpnm,&nb,&off);
	nbytes=nb;
	name=string(cnm);
	string tname(tpnm);
	if( (tname=="REAL") || tname=="real"
		|| tname=="float" || tname=="real32" || tname=="REAL32") 
	{
		if(nbytes!=4)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 4"));
		dtype=REAL32;
	}
	else if( (tname=="REAL64") || tname=="real64"
		|| tname=="double" )
	{
		if(nbytes!=8)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 8"));
		dtype=REAL64;
	}
	else if( tname=="LONG" || tname=="long" || tname=="int64"
		|| tname=="INT64" )
	{
		if(nbytes!=8)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 8"));
		dtype=INT64;
	}
	else if( tname=="INT" || tname=="int" || tname=="int32"
		|| tname=="INT32" )
	{
		if(nbytes!=4)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 4"));
		dtype=INT32;
	}
	else if( (tname=="SHORT") || tname=="short" || tname=="int16"
		|| tname=="INT16" )
	{
		dtype=INT16;
		if(nbytes!=2)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 2"));
	}
	else if( (tname=="BYTE") || tname=="byte" || tname=="int8"
		|| tname=="INT8" )
	{
		dtype=BYTE;
		if(nbytes!=1)
			throw SeisppError(base_error
			+ string("parameter=") + name
			+ string(" has inconsistent size for specified type")
			+ string("\nShould be 1"));
	}
	else if( (tname=="STRING") || tname=="string" )
		dtype=STRING;
	else if( tname=="BOOLEAN" || tname=="boolean"
		|| tname=="BOOL" || tname=="bool" )
		dtype=BOOL;
	else
		throw SeisppError(string("HeaderAttributeProperties constructor: ")
			+ string("Illegal data type specified on this parameter line->")
			+string(line) );
	offset=(size_t) off;
}
string HeaderMap::get_string(string name, unsigned char *h)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(string("HeaderMap::get attribute name=")
			+ name
			+ string("is not defined for this header type"));
	int nb=ithm->second.nbytes;
	nb=nb+1;
	char *s=new char[nb];
	char *hs=reinterpret_cast<char *>(h);
	for(int i=0;i<nb-1;++i)
		s[i]=hs[i];
	h[nb]='\0';
	string result(s);
	delete [] s;
	return(result);
}
bool HeaderMap::get_bool(string name, unsigned char *h)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(string("HeaderMap::get attribute name=")
			+ name
			+ string("is not defined for this header type"));
	size_t o=ithm->second.offset;
	int boolval;
	switch(ithm->second.nbytes)
	{
	case 1:
		unsigned char *ucptr;
		ucptr=reinterpret_cast<unsigned char *>(h+o);
		boolval=static_cast<int>(*ucptr);
		break;
	case 2:
		short int *sptr;
		sptr=reinterpret_cast<short int *>(h+o);
		boolval=static_cast<int>(*sptr);
		break;
	case 4:
		int *iptr;
		iptr=reinterpret_cast<int *>(h+o);
		boolval=(*iptr);
		break;
	default:
		throw SeisppError(string("HeaderMap::put_bool:  ")
		 + string("Illegal number of bytes for boolean. ")
		 + string("Only know how to handle 1,2, or 4"));
	}
	/* Weird to need this but different data types have magic numbers
	for true or false.  This is a general way handle this albeit 
	confusing. */
	if(boolean_default)
	{
		if(boolval==boolean_false)
			return false;
		else
			return true;
	}
	else
	{
		if(boolval==boolean_true)
			return true;
		else
			return false;
	}
}
void HeaderMap::put_string(string name,char *s, unsigned char *h)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(string("HeaderMap::put attribute named=")
			+ name
			+ string("is not defined for this header type"));
	int nb=ithm->second.nbytes;
	int i;
	for(i=0;i<nb;++i) h[i]='\0';
	// reduce the copy length if s is short
	if(strlen(s)<nb) nb=strlen(s);
	char *scpy=reinterpret_cast<char *>(h);
	for(i=0;i<nb;++i) scpy[i]=s[i];
}
void HeaderMap::put_string(string name,string s, unsigned char *h)
{
	char *strptr;
	strptr=const_cast<char *>(s.c_str());
	this->put_string(name,strptr,h);
}
void HeaderMap::put_bool(string name,bool bval, unsigned char *h)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end())
		throw SeisppError(string("HeaderMap::put attribute named=")
			+ name
			+ string("is not defined for this header type"));
	size_t o=ithm->second.offset;
	int boolval;
	if(bval)
		boolval=boolean_true;
	else
		boolval=boolean_false;
	unsigned char *hptr;
	hptr=h+o;
	switch(ithm->second.nbytes)
	{
	case 1:
		unsigned char ucval;
		ucval=static_cast<unsigned char>(boolval);
		*hptr=ucval;
		break;
	case 2:
		short int sint;
		sint=static_cast<short int>(boolval);
		short int *siptr;
		siptr=reinterpret_cast<short int *>(hptr);
		*siptr=sint;
		break;
	case 4:
		int *iptr;
		iptr=reinterpret_cast<int *>(hptr);
		*iptr=boolval;
		break;
	default:
		throw SeisppError(string("HeaderMap::put_bool:  ")
		 + string("Illegal number of bytes for boolean. ")
		 + string("Only know how to handle 1,2, or 4"));
	}
}
HeaderMap::HeaderMap()
{
	headersize=0;
}
HeaderMap::HeaderMap(Pf *pfin,string tag)
{
	Pf *pf;
	// tag is a header type name nested with an Arr
	// Use pfget to grab the one we want.
	if(pfget(pfin,const_cast<char *>(tag.c_str()),(void **)&pf) != PFARR)
		throw SeisppError(
			string("Parameter file has no definition for header named ")
			+ tag);
	Tbl *t;
	t=pfget_tbl(pf,(char *)"attributes");
	if(t==NULL)
		throw SeisppError(
			string("Parameter file is missing required Tbl parameter=")
			+ tag);

	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		line=(char *)gettbl(t,i);
		HeaderAttributeProperties hap(line);
		attributes[hap.name]=hap;
	}
	freetbl(t,0);
	headersize=(size_t)pfget_int(pf,(char *)"headersize");
	boolean_true=pfget_int(pf,(char *)"boolean_true");
	boolean_false=pfget_int(pf,(char *)"boolean_false");
	int booltmp=pfget_boolean(pf,(char *)"boolean_default");
	if(booltmp)
		boolean_default=true;
	else
		boolean_default=false;
}
HeaderMap::HeaderMap(const HeaderMap& parent)
{
	headersize=parent.headersize;
	attributes=parent.attributes;
	boolean_true=parent.boolean_true;
	boolean_false=parent.boolean_false;
	boolean_default=parent.boolean_default;
}
HeaderMap& HeaderMap::operator=(const HeaderMap& parent)
{
	if(this!= &parent)
	{
		headersize=parent.headersize;
		attributes=parent.attributes;
		boolean_true=parent.boolean_true;
		boolean_false=parent.boolean_false;
		boolean_default=parent.boolean_default;
	}
	return(*this);
}
AttributeType HeaderMap::dtype(string name)
{
	map<string,HeaderAttributeProperties>::iterator ithm;
	ithm=attributes.find(name);
	if(ithm==attributes.end()) 
		return HDRINVALID;
	else
		return (ithm->second.dtype);
}

} // End SEISPP Namespace encapsulation
