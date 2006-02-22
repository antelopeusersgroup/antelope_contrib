#include "Metadata.h"
#include "AttributeMap.h"
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

	t = pfget_tbl(pf,const_cast<char *>(name.c_str()));
	if(t==NULL) 
		throw MetadataError(string("Parameter file missing required ")
			+name);
	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		AttributeProperties *ap;
		line = (char *)gettbl(t,i);
		ap = new AttributeProperties(string(line));
		(*this).attributes.insert(APMAP::value_type(ap->internal_name,*ap));
		// had a memmory leak here.  insert copies need to delete
		delete ap;
	}
	freetbl(t,0);
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
	}
	return (*this);
}
AttributeMap::AttributeMap(const AttributeMap& am)
{
	attributes = am.attributes;
}
} // End SEISPP Namespace declaration
