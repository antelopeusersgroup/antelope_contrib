using namespace std;
#include "stock.h"
#include "pf.h"
#include <string>
#include "metadata.h"
//
// needed until antelope 4.5
//
Pf *pfdup(Pf*);
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
void Metadata::load_metadata(string mdin) 
	throw(Metadata_parse_error)
{
	int ierr;
	// We might think this was needed:  if(pf!=NULL) pffree(pf);
	// it is not because pfcompile checks for *pf==NULL and
	// assumes it is valid and to be updated if nonzero.
	// This is a handy way to deal with defaults
	// The char * cast is necessary to keep the compiler
	// from bitching about a const char
	ierr = pfcompile((char *)mdin.c_str(),&pf);
	if(ierr!=0) throw Metadata_parse_error(ierr,"Failure in load_metadata");
}
// near dup of above done for convenience
void Metadata::load_metadata(char *mdin) 
	throw(Metadata_parse_error)
{
	int ierr;
	// We might think this was needed:  if(pf!=NULL) pffree(pf);
	// it is not because pfcompile checks for *pf==NULL and
	// assumes it is valid and to be updated if nonzero.
	// This is a handy way to deal with defaults
	ierr = pfcompile(mdin,&pf);
	if(ierr!=0) throw Metadata_parse_error(ierr,"Failure in load_metadata");
}

//
//Sometimes we need to not copy all of the metadata from one object
//to another.  This function allows selective copy driven by a list
//

void  copy_selected_metadata(Metadata& mdin, Metadata& mdout,
		                list<Metadata_typedef>& mdlist)
	throw(Metadata_error)
{
	list<Metadata_typedef>::iterator i;
	Metadata_typedef& mdti=*i;
	int count;

	for(i=mdlist.begin(),count=0;i!=mdlist.end();++i,++count)
	{
		MDtype mdtest;
		double r;
		int iv;
		string s;
		Tbl *t;
		Arr *a;
		bool b;

		mdtest = mdti.mdt;
		try {
			switch(mdtest)
			{
			case MDreal:
				r=mdin.get_double(mdti.tag);
				mdout.put_metadata(mdti.tag,r);
				break;
			case MDint:
				iv=mdin.get_int(mdti.tag);
				mdout.put_metadata(mdti.tag,iv);
				break;
			case MDstring:
				s=mdin.get_string(mdti.tag);
				mdout.put_metadata(mdti.tag,s);
				break;
			case MDlist:
				t=mdin.get_list(mdti.tag);
				mdout.put_metadata(mdti.tag,t);
				break;
			case MDmap:
				a=mdin.get_map(mdti.tag);
				mdout.put_metadata(mdti.tag,a);
				break;
			case MDboolean:
				b=mdin.get_bool(mdti.tag);
				mdout.put_metadata(mdti.tag,b);
				break;
			default:
				cerr<<"Fatal: copy_selected_metadata was passed illegal type definition\nFatal error as this indicates a coding error that must be fixed" << endl;
				exit(-1);
			};
		} catch( Metadata_error merr)
		{
			cerr << "Error in copy_selected_metadata at item ";
			cerr << count << "with tag" << mdti.tag <<"\n" ;
			cerr << "Copy truncated" << endl;
			merr.log_error();
			throw;
		}
	}
}
// Simple function to dump all metadata in a md object to 
// stdout
//
void Metadata::print_all_metadata()
{
	char *md_contents;
	md_contents=pf2string(pf);
	if(md_contents==NULL)
		cout << "ERROR: Metadata object is empty";
	else
	{
		cout << md_contents;
		free(md_contents);
	}
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
