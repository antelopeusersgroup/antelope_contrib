#include <strstream>
#include "stock.h"
#include "pf.h"
#include <string>
#include "metadata.h"
using namespace std;
namespace SEISPP
{

//
// This file is used to create a shared object library.  This 
// approach may not be universally applicable, but it is known to work
// in solaris.  When a library is first openned the _init() function is
// executed.  Here we utilized this to initialized a global parameter 
// space for the library used as a default template for metadata
//
static Pf *Metadata_defaults_pf;
void _init()
{
	char *mddname=(char *)"metadata_defaults";
	char *name_from_env;
	char *name;
	int ierr;
	// Get name from the environment
	// revert to default pf name if not defined 
	name_from_env = getenv("METADATA_DEFAULTS");
	if(name_from_env==NULL)
		name=mddname;
	else
		name=name_from_env;

	ierr = pfread(name,&Metadata_defaults_pf);
	if(ierr)
	{
		cerr << "Metadata_default initialization failed\npfread failed for" << name <<".pf\n";
		exit(-1);
	}
}
//
// The default constructor uses the defaults as a template which
// allows new parameters to replace defaults in place.
//
Metadata::Metadata()
{
	if(Metadata_defaults_pf==NULL)_init();
	pf=pfdup(Metadata_defaults_pf);
};
Metadata::Metadata(Pf *pfin)
{
	if(Metadata_defaults_pf==NULL)_init();
	pf=pfdup(Metadata_defaults_pf);
	// Have to do this using pf2string and pfcompile
	// to allow default to work
	char *pfs=pf2string(pfin);
	pfcompile(pfs,&pf);
	free(pfs);
};
// Variant that extracts a subset of a pf with the prefix
// label:  tag &Arr{  
Metadata::Metadata(Pf *pfin, string tag)
{
	void *result;
	int pftype_return;
	Pf *pfnested;

	if(Metadata_defaults_pf==NULL)_init();
	pf=pfdup(Metadata_defaults_pf);

	pftype_return = pfget(pfin,(char *)tag.c_str(),&result);
	if(pftype_return != PFARR)
	{
		throw  Metadata_error(string("Metadata pfsubset constructor: tag =")
			+ tag + string(" &Arr{\nNot found in parameter file"));
	}
	// Just like above
	pfnested = static_cast<Pf *>(result);
	char *pfs=pf2string(pfnested);
	pfcompile(pfs,&pf);
	free(pfs);
}

Metadata::~Metadata()
{
	pffree(pf);
};
Metadata::Metadata(const Metadata& md)
	throw(Metadata_parse_error)
{
	char *pfstr;  
	int ierr;

	// This approach uses the default as a template then compiles
	// the contents of md.pf on top of the defaults.  This is the
	// way in the current pf library to do this.  An ugly solution
	// in terms of memory because of the conversion to and from a string
	if(Metadata_defaults_pf==NULL)_init();
        pf=pfdup(Metadata_defaults_pf);
	pfstr = pf2string(md.pf);
	ierr = pfcompile(pfstr,&pf);
	// Just post this error and continue.  It should never happen, but
	// at least we handle the return from pfcompile correctly.
	if(ierr) throw Metadata_parse_error(ierr,
		"pfcompile error in copy constructor");
	free(pfstr);
}
// constructor from an antelope database (possibly view) row driven by
// mdlist and am.  The list of attributes found in mdlist are extracted
// from the database row using dbgetv.  am defines how the Antelope
// attributes (e.g. wfdisc.time) are translated to an internal namespace.
// That is, will attempt to read all attributes in Attribute_Map list 
// and put them in the Metadata object

Metadata::Metadata(Database_Handle& dbh,
	Metadata_list& mdlist, 
		Attribute_Map& am)
			throw(Metadata_error)
{
	Datascope_Handle& ddbh=dynamic_cast<Datascope_Handle&>(dbh);
	Metadata_list::iterator i;
	map<string,Attribute_Properties>::iterator ape=am.attributes.end();
	// We'll just use the Dbptr and use raw Datascope routines.  This
	// was done for two reasons.  The main one is I had a working form
	// of this algorithm before I converted to the generic database handle
	// concept.  If it isn't broken, don't fix it.  Second, it should
	// be slightly faster as it removes the overhead of a second set
	// of function calls if the object oriented handle is used
	Dbptr db = ddbh.db;

	if(Metadata_defaults_pf==NULL)_init();
        pf=pfdup(Metadata_defaults_pf);
	// Myer's book says I should use a for_each for a loop like this.
	// It is faster and more efficient.  I don't understand 
	// STL algorithms and function objects well enough to implement this 
	// right now.  Point is is maybe should be modified later.
	for(i=mdlist.begin();i!=mdlist.end();++i)
	{
		MDtype mdtype;
		char csval[128]; // ugly to used fixed buffer, but no choice
		double fpval;
		int ival;
		map<string,Attribute_Properties>::iterator ap;
		string dbattributename;
		string internal_name;

		internal_name = (*i).tag;
		mdtype = (*i).mdt;
		ap = am.attributes.find(internal_name);
		if(ap==ape) throw (Metadata_error(
				string("Metadata db constructor:  required attribute ")
				+ internal_name
				+string(" is not in Attribute_Map.  Check initialization")));
		// the weird ap->second is a STL oddity for the item
		// two of a pair <key,type>
		dbattributename=ap->second.db_table_name
				+ "." +ap->second.db_attribute_name;  // antelope naming
		if((*i).mdt != ap->second.mdt) throw (Metadata_error(
				string("Metadata db constructor:  mismatch of type definitions for database attribute ")+(ap->second.db_attribute_name)));

		switch((*i).mdt)
		{
		case MDreal:
			if(dbgetv(db,0,dbattributename.c_str(),
				&fpval,0)==dbINVALID)
			{
				pffree(pf);
				throw (Metadata_error(
				  string("Metadata db constructor failed with dbgetv error\n")
				  +string("Trying to read attribute ")
				  +internal_name
				  +string("from database")));
			}
			put_metadata(internal_name,fpval);
			break;
		case MDint:
			if(dbgetv(db,0,
				dbattributename.c_str(),
				&ival,0)==dbINVALID)
			{
				pffree(pf);
				throw Metadata_error(string("Metadata db constructor failed with dbgetv error\n")
				+string("Trying to read attribute ")
				+internal_name
				+string("from database"));
			}
			put_metadata(internal_name,ival);
			break;
		case MDstring:
			if(dbgetv(db,0,
			  dbattributename.c_str(),
				csval,0)==dbINVALID)
			{
				pffree(pf);
				throw Metadata_error(string("Metadata db constructor failed with dbgetv error\n")
				+string("Trying to read attribute ")
				+internal_name
				+string("from database"));
			}
			put_metadata(internal_name,csval);
			break;
		default:
			pffree(pf);
			throw Metadata_error(string("Metadata db constructor: ")
			 + string("requested unsupported metadata type.  Check parameter file definitions"));
		}

	}
}
Metadata& Metadata::operator=(const Metadata& md)
{
	if(&md!=this)
	{
		if(pf!=NULL) pffree(pf);
		pf=pfdup(md.pf);
	}
	return(*this);
}
//
// These functions get and convert values
//
double Metadata::get_double(string s)
	throw(Metadata_get_error)
{
	void *result;
	double val;
	int pftype_return;
	char *char_val;

	pftype_return = pfget(pf,(char *)s.c_str(),&result);
	if(pftype_return != PFSTRING) 
		throw Metadata_get_error("double",s,"");
	char_val=pfget_string(pf,(char *)s.c_str());
	val = atof(char_val);
	return(val);
}
int Metadata::get_int(string s)
	throw(Metadata_get_error)
{
	void *result;
	int val;
	int pftype_return;
	char *char_val;
	pftype_return = pfget(pf,(char *)s.c_str(),&result);
	if(pftype_return != PFSTRING) 
		throw  Metadata_get_error("int",s,"");
	char_val=pfget_string(pf,(char *)s.c_str());
	val = atoi(char_val);
	return(val);
}
string Metadata::get_string(string s) 
	throw(Metadata_get_error)
{
	void *result;
	string val;
	int pftype_return;
	char *char_val;
	pftype_return = pfget(pf,(char *)s.c_str(),&result);
	if(pftype_return != PFSTRING) 
		throw  Metadata_get_error("string",s,"");
	char_val=pfget_string(pf,(char *)s.c_str());
	val = char_val; //= is overloaded for this case so is a simple assign
	return(val);
}
bool Metadata::get_bool(string s)
	throw(Metadata_get_error)
{
	void *result;
	int val;
	int pftype_return;
	//
	// Boolean will default to false if not found in the pf
	// this is consistent with documentation and is rational behaviour
	//
	val = pfget_boolean(pf,(char *)s.c_str());
	if(val) 
		return(true);
	else 
		return(false);
}
Tbl *Metadata::get_list(string s)
	throw(Metadata_get_error)
{
	void *result;
	int val;
	int pftype_return;
	Tbl *t;
	pftype_return = pfget(pf,(char *)s.c_str(),&result);
	if(pftype_return != PFTBL) 
		throw  Metadata_get_error("Antelope Tbl",s,"");
	t = pfget_tbl(pf,(char *)s.c_str());
	return(t);
}
Arr *Metadata::get_map(string s)
	throw(Metadata_get_error)
{
	void *result;
	int val;
	int pftype_return;
	Arr *a;
	pftype_return = pfget(pf,(char *)s.c_str(),&result);
	if(pftype_return != PFARR) 
		throw  Metadata_get_error("Antelope Arr",s,"");
	a = pfget_arr(pf,(char *)s.c_str());
	return(a);
}

//
// Functions to put things into metadata object
//
void Metadata::put_metadata(string name, double val)
{
	pfput_double(pf,(char *)name.c_str(),val);
}
void Metadata::put_metadata(string name, int val)
{
	pfput_int(pf,(char *)name.c_str(),val);
}
void Metadata::put_metadata(string name, string val)
{
	pfput_string(pf,(char *)name.c_str(),(char *)val.c_str());
}
// for C style strings, we should not depend on the compiler
void Metadata::put_metadata(string name, char *val)
{
	pfput_string(pf,(char *)name.c_str(),val);
}
void Metadata::put_metadata(string name, Arr *val)
{
	pfput_arr(pf,(char *)name.c_str(),val);
}
void Metadata::put_metadata(string name, Tbl *val)
{
	pfput_tbl(pf,(char *)name.c_str(),val);
}
void Metadata::put_metadata(string name, bool val)
{
	if(val)
		pfput_boolean(pf,(char *)name.c_str(),1);
	else
		pfput_boolean(pf,(char *)name.c_str(),0);
}
void Metadata::remove(string name)
{
	Pf *pftmp;
	pftmp = pfdel(pf,const_cast<char *>(name.c_str()));
	// necessary because pfdel returns a pf copy of entry deleted on 
	// success
	if(pftmp!=NULL) pffree(pftmp);
}
Metadata::Metadata(char *mdin) 
	throw(Metadata_parse_error)
{
	int ierr;
	// We might think this was needed:  if(pf!=NULL) pffree(pf);
	// it is not because pfcompile checks for *pf==NULL and
	// assumes it is valid and to be updated if nonzero.
	// This is a handy way to deal with defaults
	// The char * cast is necessary to keep the compiler
	// from bitching about a const char
	if(Metadata_defaults_pf==NULL)_init();
        pf=pfdup(Metadata_defaults_pf);
	ierr = pfcompile(mdin,&pf);
	if(ierr!=0) throw Metadata_parse_error(ierr,"pfcompile failure in Metadata constructor");
}
// Nearly identical function for string input.  There are probably
// ways to have done this more simply, but this function is small
Metadata::Metadata(string mdin)
	throw(Metadata_parse_error)
{
	int ierr;
	if(Metadata_defaults_pf==NULL)_init();
        pf=pfdup(Metadata_defaults_pf);
	ierr = pfcompile(const_cast<char *>(mdin.c_str()),&pf);
	if(ierr!=0) throw Metadata_parse_error(ierr,"pfcompile failure in Metadata constructor");
}


//
//Sometimes we need to not copy all of the metadata from one object
//to another.  This function allows selective copy driven by a list
//

void  copy_selected_metadata(Metadata& mdin, Metadata& mdout,
			Metadata_list& mdlist)
	throw(Metadata_error)
{
	list<Metadata_typedef>::iterator mdti;
	int count;

	for(mdti=mdlist.begin(),count=0;mdti!=mdlist.end();++mdti,++count)
	{
		MDtype mdtest;
		double r;
		int iv;
		string s;
		Tbl *t;
		Arr *a;
		bool b;

		mdtest = mdti->mdt;
		try {
			switch(mdtest)
			{
			case MDreal:
				r=mdin.get_double(mdti->tag);
				mdout.put_metadata(mdti->tag,r);
				break;
			case MDint:
				iv=mdin.get_int(mdti->tag);
				mdout.put_metadata(mdti->tag,iv);
				break;
			case MDstring:
				s=mdin.get_string(mdti->tag);
				mdout.put_metadata(mdti->tag,s);
				break;
			case MDlist:
				t=mdin.get_list(mdti->tag);
				mdout.put_metadata(mdti->tag,t);
				break;
			case MDmap:
				a=mdin.get_map(mdti->tag);
				mdout.put_metadata(mdti->tag,a);
				break;
			case MDboolean:
				b=mdin.get_bool(mdti->tag);
				mdout.put_metadata(mdti->tag,b);
				break;
			case MDinvalid:
			// silently skip values marked as invalid
				break;
			default:
				cerr<<"Fatal: copy_selected_metadata was passed illegal type definition\nFatal error as this indicates a coding error that must be fixed" << endl;
				exit(-1);
			};
		} catch( Metadata_error merr)
		{
			cerr << "Error in copy_selected_metadata at item ";
			cerr << count << "with tag" << mdti->tag <<"\n" ;
			cerr << "Copy truncated" << endl;
			merr.log_error();
			throw;
		}
	}
}
// output stream operator.  Originally was in ignorance made
// a named function called print_all_metadata (older versions may
// have this as debris.
//
ostream& operator<<(ostream& os, Metadata& md)
{
	char *md_contents;
	md_contents=pf2string(md.pf);
	if(md_contents==NULL)
		cerr << "ERROR: Metadata object is empty";
	else
	{
		os << md_contents;
		free(md_contents);
	}
	return os;
}
//
// Small function to extract the entire metadata contents to a pf.  
// Necessary in current implementation because pf is private.  If the underlying
// method of storing this data changes, this would become and interface routine.
//
Pf *Metadata::extract_all_metadata_to_pf()
{
	return(pfdup(pf));
}
// Attribute_Properties encapsulate concepts about what a piece
// of metadata is.  It was designed originally with attributes
// extracted from a relational database as the model.  The
// Attribute_Properties object, however, should be more general
// than this as it can support things like lists ala antelope tbls.
Attribute_Properties::Attribute_Properties()
{
	db_attribute_name="none";
	db_table_name="";
	internal_name="NULL";
	mdt=MDstring;
}
Attribute_Properties::Attribute_Properties(string st)
{
	const string white(" \t\n");
	int current=0,next;
	int end_current;
	string mdtype_word;
	string stmp;  // use temporary to allow string edits
	const string emess("Attribute_Properties(string) constructor failure:  failure parsing the following string:\n");

	if(st.empty()) 
		throw Metadata_error(string("Attribute_Properties constructor failure;  Constructor was passed an empty string\n"));

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
	if(end_current<0) throw Metadata_error(string(emess+st));
	internal_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw Metadata_error(string(emess+st));
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) throw Metadata_error(string(emess+st));
	db_attribute_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw Metadata_error(string(emess+st));
	end_current=stmp.find_first_of(white,current);
	if(end_current<0) throw Metadata_error(string(emess+st));
	db_table_name.assign(stmp,current,end_current-current);
	current = stmp.find_first_not_of(white,end_current);
	if(current<0) throw Metadata_error(string(emess+st));
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
	else if(mdtype_word=="LIST" || mdtype_word=="list")
		mdt=MDlist;
	else if(mdtype_word=="MAP" || mdtype_word=="map")
		mdt=MDmap;
	else
	{
		mdt = MDinvalid;
		throw Metadata_error(string("Attribute_Properties(string) constructor:  Unrecognized metadata type = ")+mdtype_word);
	}
	
}
Attribute_Properties::Attribute_Properties(const Attribute_Properties& apin)
{
	db_attribute_name=apin.db_attribute_name;
	db_table_name=apin.db_table_name;
	internal_name=apin.internal_name;
	mdt = apin.mdt;
}
Attribute_Properties& Attribute_Properties::operator=(const Attribute_Properties& apin)
{
	if(&apin==this)return(*this);
	db_attribute_name=apin.db_attribute_name;
	db_table_name=apin.db_table_name;
	internal_name=apin.internal_name;
	mdt = apin.mdt;
	return(*this);
}
// An Attribute_Map is a higher order object built up o
// name definitions defined by a set of Attribute_Properties
// objects.  
//
	
Attribute_Map::Attribute_Map(Pf *pf,string name)
{
	Tbl *t;
	// temporary typedef to keep an already awful syntax for a map
	// from being impossible.  Used in most books for this reason.
	typedef map<string,Attribute_Properties> APMAP;

	t = pfget_tbl(pf,const_cast<char *>(name.c_str()));
	if(t==NULL) 
		throw Metadata_error(string("Parameter file missing required ")
			+name);
	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		Attribute_Properties *ap;
		line = (char *)gettbl(t,i);
		ap = new Attribute_Properties(string(line));
		(*this).attributes.insert(APMAP::value_type(ap->internal_name,*ap));
	}
}
// Default constructor uses a frozen name and utilizes the above
// constructor.

Attribute_Map::Attribute_Map()
{
	const string DEFAULT_SCHEMA_NAME("css3.0");
	const string pfname("seispp_attribute_maps");
	Pf *pf;
	if(pfread(const_cast<char *>(pfname.c_str()),&pf))
		throw Metadata_error(
			string("pfread failure for attribute map parameter file = ")
				+ pfname);
	try {
		*this = Attribute_Map(pf,DEFAULT_SCHEMA_NAME);
	} catch (...)  {throw;}
	pffree(pf);
}
	


	
/* an  inefficient way to do this constructor, but should be acceptable since
I don't expect this function to be called more than once in any given program.
*/

Attribute_Map::Attribute_Map(string s)
{
	string stbl;
	Pf *pf;
	int ierr;
	Attribute_Map *amtmp;

	stbl = string("Attribute_Map &Tbl{\n")
		+ s +string("\n}\n");
	ierr=pfcompile(const_cast<char *>(stbl.c_str()),&pf);
	if(ierr!=0)
		throw Metadata_parse_error(ierr,"pfcompile failure building Attribute_Map object");
	amtmp = new Attribute_Map(pf,"Attribute_Map");
	*this = *amtmp;
	delete amtmp;
	pffree(pf);
}
Attribute_Map& Attribute_Map::operator=(const Attribute_Map& am)
{
	if(this!=&am)
	{
		attributes = am.attributes;
	}
	return (*this);
}
Attribute_Map::Attribute_Map(const Attribute_Map& am)
{
	attributes = am.attributes;
}
} // Termination of namespace SEISPP definitions
