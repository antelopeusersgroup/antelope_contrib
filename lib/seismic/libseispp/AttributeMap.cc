#include <sstream>
#include "Metadata.h"
#include "AttributeMap.h"
#include "seispp.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;
// AttributeProperties encapsulate concepts about what a piece
// of metadata is.  It was designed originally with attributes
// extracted from a relational database as the model.  The
// AttributeProperties object, however, should be more general
// than this as it can support things like lists ala antelope tbls.
AttributeProperties::AttributeProperties()
{
	db_attribute_name="none";
	db_table_name="";
	internal_name="NULL";
	mdt=MDstring;
}
AttributeProperties::AttributeProperties(string st)
{
	const string white(" \t\n");
	int current=0,next;
	int end_current;
	string mdtype_word;
	string bool_word;
	string stmp;  // use temporary to allow string edits
	const string emess("AttributeProperties(string) constructor failure:  failure parsing the following string:\n");

	if(st.empty()) 
		throw MetadataError(string("AttributeProperties constructor failure;  Constructor was passed an empty string\n"));

	// this should find first nonwhite position
	//INCOMPLETE needs error checking for missing values
	
	// create a copy of st and remove any leading white 
	// space at the head of the string
	stmp = st;
	if((current=stmp.find_first_not_of(white,0)) != 0)
	{
		stmp.erase(0,current);
		current = 0;
	} 
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) throw MetadataError(string(emess+st));
	internal_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw MetadataError(string(emess+st));
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) throw MetadataError(string(emess+st));
	db_attribute_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw MetadataError(string(emess+st));
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) throw MetadataError(string(emess+st));
	db_table_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw MetadataError(string(emess+st));
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) end_current=stmp.length();
	mdtype_word.assign(stmp,current,end_current-current);
	if(mdtype_word=="REAL" || mdtype_word=="real")
		mdt=MDreal;
	else if(mdtype_word=="INT" || mdtype_word=="int"
		|| mdtype_word=="integer")
		mdt = MDint;
	else if(mdtype_word=="STRING" || mdtype_word=="string")
		mdt=MDstring;
	else if(mdtype_word=="BOOL" || mdtype_word=="bool"
		|| mdtype_word=="BOOLEAN" || mdtype_word=="boolean")
		mdt=MDboolean;
	else
	{
		mdt = MDinvalid;
		throw MetadataError(string("AttributeProperties(string) constructor:  Unrecognized metadata type = ")+mdtype_word);
	}
	// optional is_key field.  Defaulted false 
	is_key = false;
	
	current = stmp.find_first_not_of(white,end_current);
	if(current>=0) 
	{
		end_current=stmp.find_first_of(white,current);
		if(end_current<0) end_current=stmp.length();
		bool_word.assign(stmp,current,end_current-current);
		if(bool_word=="yes" || bool_word=="true"
			|| bool_word=="TRUE")  is_key=true;
	}
}
AttributeProperties::AttributeProperties(const AttributeProperties& apin)
{
	db_attribute_name=apin.db_attribute_name;
	db_table_name=apin.db_table_name;
	internal_name=apin.internal_name;
	mdt = apin.mdt;
	is_key = apin.is_key;
}
AttributeProperties& AttributeProperties::operator=(const AttributeProperties& apin)
{
	if(&apin==this)return(*this);
	db_attribute_name=apin.db_attribute_name;
	db_table_name=apin.db_table_name;
	internal_name=apin.internal_name;
	mdt = apin.mdt;
	is_key = apin.is_key;
	return(*this);
}
string AttributeProperties::fully_qualified_name()
{
	string result;
	result=db_table_name + "." + db_attribute_name;
	return(result);
}

// An AttributeMap is a higher order object built up o
// name definitions defined by a set of AttributeProperties
// objects.  
//
	
AttributeMap::AttributeMap(Pf *pf,string name)
{
	Tbl *t;
	// temporary typedef to keep an already awful syntax for a map
	// from being impossible.  Used in most books for this reason.
	typedef map<string,AttributeProperties> APMAP;
	Pf *pfnested;
	int pftype_return;
	void *vptr;
	pftype_return=pfget(pf,const_cast<char *>(name.c_str()),&vptr);
	if(pftype_return != PFARR)
		throw SeisppError("AttributeMap pf constructor:  "
		  + string("parameter ") + name
		  + string(" must be tagged as an Arr in parameter file defining")
		  + string(" AttributeMap definitions"));
	pfnested=static_cast<Pf *>(vptr);

	char *attblkey="Attributes";
	t = pfget_tbl(pfnested,attblkey);
	if(t==NULL) 
		throw MetadataError(string("Parameter file missing required ")
			+name);
	int i;
	char *line;
	for(i=0;i<maxtbl(t);++i)
	{
		AttributeProperties *ap;
		line = (char *)gettbl(t,i);
		ap = new AttributeProperties(string(line));
		(*this).attributes.insert(APMAP::value_type(ap->internal_name,*ap));
		// had a memmory leak here.  insert copies need to delete
		delete ap;
	}
	freetbl(t,0);
	t=pfget_tbl(pfnested,"aliases");
	if(t!=NULL)
	{
		string token;
		string key;
		for(i=0;i<maxtbl(t);++i)
		{
			line = (char *)gettbl(t,i);
			istringstream in(line);
			in>>key;
			if(in.eof()) throw SeisppError("AttributeMap pf constructor:  "
				+ string("bad input line in alias list or schema=")
				+ name
				+ string("\nOffending line:")+string(line) );
			in>>token;
			list<string> aliaslist;
			aliaslist.push_back(token);
			if(!in.eof())
			{
			   do{
				in >> token;
				aliaslist.push_back(token);
				if(in.eof()) break;
			    }
			    while(1);
			}
			aliasmap[key]=aliaslist;
		}
			
		freetbl(t,0);
	}
}
// Default constructor uses a frozen name and utilizes the above
// constructor.

AttributeMap::AttributeMap()
{
	const string DEFAULT_SCHEMA_NAME("css3.0");
	const string pfname("seispp_attribute_maps");
	Pf *pf;
	if(pfread(const_cast<char *>(pfname.c_str()),&pf))
		throw MetadataError(
			string("pfread failure for attribute map parameter file = ")
				+ pfname);
	try {
		*this = AttributeMap(pf,DEFAULT_SCHEMA_NAME);
		pffree(pf);
	} catch (...)  
	{
		pffree(pf);
		throw;
	}
}
	

// Use alternative schema to css3.0 contained in the same 
// global parameter file
	
AttributeMap::AttributeMap(string schema)
{
	const string pfname("seispp_attribute_maps");
	Pf *pf;
	if(pfread(const_cast<char *>(pfname.c_str()),&pf))
		throw MetadataError(
			string("pfread failure for attribute map parameter file = ")
				+ pfname);
	try {
		*this = AttributeMap(pf,schema);
		pffree(pf);
	} catch (...)  
	{
		pffree(pf);
		throw;
	}
}
AttributeMap& AttributeMap::operator=(const AttributeMap& am)
{
	if(this!=&am)
	{
		attributes = am.attributes;
		aliasmap=am.aliasmap;
	}
	return (*this);
}
AttributeMap::AttributeMap(const AttributeMap& am)
{
	attributes = am.attributes;
	aliasmap=am.aliasmap;
}
bool AttributeMap::is_alias(string key)
{
	if(aliasmap.size()==0) return false;
	if(aliasmap.find(key)==aliasmap.end()) return false;
	return true;
}
map<string,AttributeProperties> AttributeMap::aliases(string key)
{
	map<string,AttributeProperties> result;
	/* reverse logic a bit odd, but cleanest solution */
	if(!this->is_alias(key))
		throw SeisppError("AttributeMap::aliases method: "
		 + string("Attribute ") + key
		 + string(" is not defined as an alias") );
	else
	{
		list<string> aml=aliasmap[key];
		map<string,AttributeProperties>::iterator amiter;
		list<string>::iterator listiter;
		for(listiter=aml.begin();listiter!=aml.end();++listiter)
		{
			amiter=attributes.find(*listiter);
			if(amiter==attributes.end()) 
			  throw SeisppError("AttributeMap::aliases method: "
				+ string("Attribute named ")
				+ (*listiter)
				+ string(" is not defined for this AttributeMap"));
                        /* We need to copy this AttributeProperty and 
                         * change the internal_name to the alias name */
                        AttributeProperties alias_property(amiter->second);
                        alias_property.internal_name=key;
			result[amiter->second.db_table_name]=alias_property;
		}
	}
	/* note this silently returns an empty list if key is not alias*/
	return(result);
	
}
/* This code has very strong parallels to aliases because they do similar
things even though they return very different results. */
list<string> AttributeMap::aliastables(string key)
{
	list<string> result;
	if(!this->is_alias(key))
		throw SeisppError("AttributeMap::aliastables method: "
		 + string("Attribute ") + key
		 + string(" is not defined as an alias") );
	else
	{
		list<string> aml=aliasmap[key];
		map<string,AttributeProperties>::iterator amiter;
		list<string>::iterator listiter;
		for(listiter=aml.begin();listiter!=aml.end();++listiter)
		{
			amiter=attributes.find(*listiter);
			if(amiter==attributes.end()) 
			  throw SeisppError("AttributeMap::aliastables method: "
				+ string("Attribute named ")
				+ (*listiter)
				+ string(" is not defined for this AttributeMap"));
			result.push_back(amiter->second.db_table_name);
		}
	}
	return(result);
}
		
	
} // End SEISPP Namespace declaration
