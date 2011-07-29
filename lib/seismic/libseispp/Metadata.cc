#include <iostream>
#include <sstream>
#include <limits.h>
#include "stock.h"
#include "pf.h"
#include <string>
#include "AttributeMap.h"
#include "Metadata.h"
#include "dbpp.h"
using namespace std;
namespace SEISPP
{
	/***** Start with Helpers.  These have only file scope. */



    /* Small helper returns a string that can be used to 
    print the type name of a attribute */
    string mdtypename(MDtype mdt)
    {
	string result;
	switch(mdt)
	{
	case MDreal:
		result="REAL";
		break;
	case MDstring:
		result="STRING";
		break;
	case MDint:
		result="INT";
		break;
	case MDboolean:
		result="BOOLEAN";
		break;
	case MDinvalid:
		result="INVALID";
		break;
	default:
		result="UNKNOWN";
	}
	return result;
    }
    void pf2metadatastring(Pf *pfin, map<string,string>& mdstr)
    {
        // Parameter files translate almost directly to the mstring
        // map.  They are keyed arrays linking to strings.
        // Chose to not use the Pf directly here due to
        // clash found in memory management in earlier version.
        // The major complication is dealing with Arr and Tbl
        // that have to be converted to strings without the keys
        int i;
        Tbl *t;
        t=pfkeys(pfin);
        string key;
        char *sk,*sv;
        int ipftype;
        string svalue;
        Pf *pfnested;
        char *pfstr;
        for(i=0;i<maxtbl(t);++i)
        {
            sk=static_cast<char *>(gettbl(t,i));
            key=string(sk);
            void *pfget_result;
            ipftype=pfget(pfin,sk,&pfget_result);
            switch (ipftype)
            {
                case PFSTRING:
                    svalue=string(static_cast<char *>(pfget_result));
                    break;
                case PFARR:
                    pfnested = static_cast<Pf *>(pfget_result);
                    pfstr=pf2string(pfnested);
                    svalue=string(" &Arr{\n")
                        + string(pfstr)+string("\n}\n");
                    free(pfstr);
                    break;
                case PFTBL:
                    pfnested = static_cast<Pf *>(pfget_result);
                    pfstr=pf2string(pfnested);
                    svalue=string(pfstr);
                    free(pfstr);
            }
            mdstr[key]=svalue;
        }
        freetbl(t,0);
    }
    // constructors
    Metadata::Metadata(const Metadata& mdold)
    {
        mreal=mdold.mreal;
        mint=mdold.mint;
        mbool=mdold.mbool;
        mstring=mdold.mstring;
    }

    Metadata::Metadata(Pf *pfin)
    {
        pf2metadatastring(pfin,mstring);
    }
    // Variant that extracts a subset of a pf with the prefix
    // label:  tag &Arr{
    Metadata::Metadata(Pf *pfin, string tag)
    {
        void *result;
        int pftype_return;
        Pf *pfnested;
        pftype_return = pfget(pfin,(char *)tag.c_str(),&result);
        if(pftype_return != PFARR)
        {
            throw  MetadataError(string("Metadata pfsubset constructor: tag =")
                + tag + string(" &Arr{\nNot found in parameter file"));
        }
        // This casting seems necessary
        pfnested = static_cast<Pf *>(result);
        pf2metadatastring(pfnested,mstring);
    }

    // constructor from an antelope database (possibly view) row driven by
    // mdlist and am.  The list of attributes found in mdlist are extracted
    // from the database row using dbgetv.  am defines how the Antelope
    // attributes (e.g. wfdisc.time) are translated to an internal namespace.
    // That is, will attempt to read all attributes in AttributeMap list
    // and put them in the Metadata object

    Metadata::Metadata(DatabaseHandle& dbh,
        MetadataList& mdlist,
        AttributeMap& am)
        throw(MetadataError)
    {
	const string base_error("Metadata db constructor:  ");
	try {
        DatascopeHandle& ddbh=dynamic_cast<DatascopeHandle&>(dbh);
        MetadataList::iterator i;
        map<string,AttributeProperties>::iterator ape=am.attributes.end();
        // We'll just use the Dbptr and use raw Datascope routines.  This
        // was done for two reasons.  The main one is I had a working form
        // of this algorithm before I converted to the generic database handle
        // concept.  If it isn't broken, don't fix it.  Second, it should
        // be slightly faster as it removes the overhead of a second set
        // of function calls if the object oriented handle is used
        Dbptr db = ddbh.db;
	// This is used as a loop test below
	bool success;

        // This could, in principle, be done with a for_each loop
        // but I think this is easier to understand.
        for(i=mdlist.begin();i!=mdlist.end();++i)
        {
            MDtype mdtype;
            char csval[128];                      // ugly to used fixed buffer, but no choice
            double fpval;
            long ival;
            map<string,AttributeProperties>::iterator ap;
            string dbattributename;
            string internal_name;
            internal_name = (*i).tag;
	    /* This contains a list of db names that drive the loop below.
	    When a names is not an alias the list will contain only one entry.
	    When a name is an alias the members are tried in order until success
	    or reaching end of list.  If not found anywhere an exception is thrown */
	    list<string> dbnamelist;
	    list<string>::iterator dbniter;  //iterator for dbnamelist
	    if(am.is_alias(internal_name))
	    {
		list<string> tablenames=am.aliastables(internal_name);
		list<string>::iterator tbliter;
		map<string,AttributeProperties> aliasmap=am.aliases(internal_name);
		bool ascanfirstpass(true);
		for(tbliter=tablenames.begin();tbliter!=tablenames.end();++tbliter)
		{
		    AttributeProperties apalias=aliasmap[*tbliter];
		    if(ascanfirstpass)
		    {
			mdtype=apalias.mdt;
			ascanfirstpass=false;
		    }
		    else
		    {
			if(mdtype!=apalias.mdt)
				throw MetadataError(base_error
					+ "Data type mismatch for db attributes"
					+ string(" tagged with key=")
					+ internal_name);
		    }
		    dbattributename=apalias.fully_qualified_name();
		    dbnamelist.push_back(dbattributename);
		}
	    }
	    else
	    {
            	mdtype = (*i).mdt;
                ap = am.attributes.find(internal_name);
                if(ap==ape) throw MetadataError(base_error
                    + "required attribute "
                    + internal_name
                    +string(" is not in AttributeMap.  Check initialization"));
                // the weird ap->second is a STL oddity for the item
                // two of a pair <key,type>
		dbattributename=ap->second.fully_qualified_name();
                if((*i).mdt != ap->second.mdt) throw MetadataError(base_error
                        +string("mismatch of type definitions for database attribute ")+(ap->second.fully_qualified_name()));
		dbnamelist.push_back(dbattributename);
	    }

	    success=false;
	    for(dbniter=dbnamelist.begin();dbniter!=dbnamelist.end();++dbniter)
	    {
	    dbattributename=*dbniter;
	
            switch(mdtype)
            {
                case MDreal:
                    if(dbgetv(db,0,dbattributename.c_str(),
                        &fpval,NULL)!=dbINVALID)
                    {
                        put(internal_name,fpval);
		        success=true;
                    }
		    else
			elog_flush(0,0);
                    break;
                case MDint:
                    if(dbgetv(db,0,
                        dbattributename.c_str(),
                        &ival,NULL)!=dbINVALID)
                    {
                        put(internal_name,ival);
		        success=true;
                    }
		    else
			elog_flush(0,0);
                    break;
                case MDstring:
                    if(dbgetv(db,0,
                        dbattributename.c_str(),
                        csval,NULL)!=dbINVALID)
                    {
                        put(internal_name,csval);
		        success=true;
                    }
		    else
			elog_flush(0,0);
                    break;
                default:
                    throw MetadataError(base_error
                        + string("requested unsupported metadata type.  Check parameter file definitions"));
            }
	    if(success)break;

            }
	    if(!success)
	    {
			string mdnametype=mdtypename(mdtype);
			throw MetadataGetError(mdnametype,internal_name,
			 string("This attribute or any associated alias was not found in the database") );
	    }
        }
	} catch (...) {throw;};
    }
    //
    // These functions get and convert values
    //
    double Metadata::get_double(string s)
        throw(MetadataGetError)
    {
        map<string,double>::iterator iptr;
        iptr=mreal.find(s);
        if(iptr!=mreal.end()) return((*iptr).second);
        map<string,string>::iterator pfstyle;
        pfstyle=mstring.find(s);
        if(pfstyle==mstring.end())
            throw MetadataGetError("real",s,"");

        string valstring=(*pfstyle).second;
        return (  atof(valstring.c_str()) );
    }
    int Metadata::get_int(string s)
        throw(MetadataGetError)
    {
        try {
            const string base_error("Metadata::get_int:  long to int conversion error ->");
            const string mdt_this("int");
            long lival;
            lival=this->get_long(s);
            if(lival>INT_MAX)
                throw MetadataGetError(mdt_this,s,
                        base_error+"overflow");
            else if (lival<INT_MIN)
                throw MetadataGetError(mdt_this,s,
                        base_error+"underflow");
            return(static_cast<int>(lival));
        } catch (MetadataGetError& mderr){throw mderr;};
    }
    long Metadata::get_long(string s)
        throw(MetadataGetError)
    {
        map<string,long>::iterator iptr;
        iptr=mint.find(s);
        if(iptr!=mint.end()) return((*iptr).second);
        map<string,string>::iterator pfstyle;
        pfstyle=mstring.find(s);
        if(pfstyle==mstring.end())
            throw MetadataGetError("int",s,"");
        string valstring=(*pfstyle).second;
        return (  atol(valstring.c_str()) );
    }
    string Metadata::get_string(string s)
        throw(MetadataGetError)
    {
        map<string,string>::iterator iptr;
        iptr=mstring.find(s);
        if(iptr==mstring.end())
            throw MetadataGetError("string",s,"");
        return((*iptr).second);
    }
    bool Metadata::get_bool(string s)
    {
        map<string,bool>::iterator iptr;
        iptr=mbool.find(s);
        if(iptr!=mbool.end()) return((*iptr).second);
        map<string,string>::iterator pfstyle;
        pfstyle=mstring.find(s);
        if(pfstyle==mstring.end()) return (false);
        if( ((*pfstyle).second==string("t"))
            || ((*pfstyle).second==string("true"))
            || ((*pfstyle).second==string("1")) )
            return(true);
        return(false);
    }

    //
    // Functions to put things into metadata object
    //
    void Metadata::put(string name, double val)
    {
        mreal[name]=val;
    }
    void Metadata::put(string name, long val)
    {
        mint[name]=val;
    }
    void Metadata::put(string name, int val)
    {
        long newval=static_cast<long>(val);
        mint[name]=val;
    }
    void Metadata::put(string name, string val)
    {
        mstring[name]=val;
    }
    // for C style strings, we should not depend on the compiler
    void Metadata::put(string name, char *val)
    {
        mstring[name]=string(val);
    }
    void Metadata::put(string name, bool val)
    {
        mbool[name]=val;
    }
    void Metadata::append_string(string key, string separator, string appendage)
    {
        map<string,string>::iterator sptr;
        sptr=mstring.find(key);
        if(sptr==mstring.end())
        {
            // Ignore separator and just add appendage if the key is not
            // already in the object
            mstring[key]=appendage;
        }
        else
        {
            string newval=(*sptr).second+separator+appendage;
            mstring[key]=newval;
        }
    }

    void Metadata::remove(string name)
    {
        // We assume this is an uncommon operation for Metadata
        // so the approach used here is to search through all the
        // maps and destroying any entry with the key name
        map<string,long>::iterator iptr;
        iptr=mint.find(name);
        if(iptr!=mint.end()) mint.erase(iptr);

        map<string,double>::iterator rptr;
        rptr=mreal.find(name);
        if(rptr!=mreal.end()) mreal.erase(rptr);

        map<string,string>::iterator sptr;
        sptr=mstring.find(name);
        if(sptr!=mstring.end()) mstring.erase(sptr);

        map<string,bool>::iterator bptr;
        bptr=mbool.find(name);
        if(bptr!=mbool.end()) mbool.erase(bptr);
    }
    Metadata::Metadata(string mdin)
        throw(MetadataParseError)
    {
        Pf *pf;
        pf=pfnew(PFARR);
        int ierr;
        ierr = pfcompile(const_cast<char *>(mdin.c_str()),&pf);
        if(ierr!=0) throw MetadataParseError(ierr,"pfcompile failure in Metadata constructor");
        //
        // The following duplicates code in the Pf constructor.
        // There may be a tricky way to invoke that code I'm not
        // aware of, but without making maps public I don't see how to
        // deal with that
        //
        int i;
        Tbl *t;
        t=pfkeys(pf);
        string key;
        char *sk,*sv;
        for(i=0;i<maxtbl(t);++i)
        {
            sk=static_cast<char *>(gettbl(t,i));
            key=string(sk);
            sv=pfget_string(pf,sk);
            mstring[key]=string(sv);
        }
        freetbl(t,0);
        pffree(pf);
    }
    bool Metadata::is_attribute_set(string key)
    {
        map<string,double>::iterator rptr;
        rptr=mreal.find(key);
        if(rptr!=mreal.end()) return(true);
        map<string,long>::iterator iptr;
        iptr=mint.find(key);
        if(iptr!=mint.end()) return(true);
        map<string,string>::iterator sptr;
        sptr=mstring.find(key);
        if(sptr!=mstring.end()) return(true);
        map<string,bool>::iterator bptr;
        bptr=mbool.find(key);
        if(bptr!=mbool.end()) return(true);
    }
    bool Metadata::is_attribute_set(char *key)
    {
        return(this->is_attribute_set(string(key)));
    }
    MetadataList Metadata::keys()
    {
        MetadataList result;
        Metadata_typedef member;
        map<string,string>::iterator sptr;
        for(sptr=mstring.begin();sptr!=mstring.end();++sptr)
        {
            member.tag=(*sptr).first;
            member.mdt=MDstring;
            result.push_back(member);
        }
        map<string,long>::iterator iptr;
        for(iptr=mint.begin();iptr!=mint.end();++iptr)
        {
            member.tag=(*iptr).first;
            member.mdt=MDint;
            result.push_back(member);
        }
        map<string,double>::iterator rptr;
        for(rptr=mreal.begin();rptr!=mreal.end();++rptr)
        {
            member.tag=(*rptr).first;
            member.mdt=MDreal;
            result.push_back(member);
        }
        map<string,bool>::iterator bptr;
        for(bptr=mbool.begin();bptr!=mbool.end();++bptr)
        {
            member.tag=(*bptr).first;
            member.mdt=MDboolean;
            result.push_back(member);
        }
        return(result);
    }

    //
    //Sometimes we need to not copy all of the metadata from one object
    //to another.  This function allows selective copy driven by a list
    //

    void  copy_selected_metadata(Metadata& mdin, Metadata& mdout,
        MetadataList& mdlist)
        throw(MetadataError)
    {
        MetadataList::iterator mdti;
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
            try
            {
                switch(mdtest)
                {
                    case MDreal:
                        r=mdin.get_double(mdti->tag);
                        mdout.put(mdti->tag,r);
                        break;
                    case MDint:
                        iv=mdin.get_int(mdti->tag);
                        mdout.put(mdti->tag,iv);
                        break;
                    case MDstring:
                        s=mdin.get_string(mdti->tag);
                        mdout.put(mdti->tag,s);
                        break;
                    case MDboolean:
                        b=mdin.get_bool(mdti->tag);
                        mdout.put(mdti->tag,b);
                        break;
                    case MDinvalid:
                        // silently skip values marked as invalid
                        break;
                    default:
                        throw MetadataError(string("copy_selected_metadata: ")
				+ " was passed illegal type definition\n"
				+ string("This indicates a coding error that must be fixed\n")
				+ string("If caller does not exit on this error, expect a less graceful abort"));
                };
            } catch( MetadataError& merr)
            {
                cerr << "Error in copy_selected_metadata at item ";
                cerr << count << " with tag " << mdti->tag <<"\n" ;
                cerr << "Copy truncated" << endl;
                merr.log_error();
                throw;
            }
        }
    }
    Metadata& Metadata::operator=(const Metadata& mdold)
    {
        if(this!=&mdold)
        {
	        mreal=mdold.mreal;
	        mint=mdold.mint;
	        mbool=mdold.mbool;
	        mstring=mdold.mstring;
        }
        return(*this);
    }
    // output stream operator.  Originally was in ignorance made
    // a named function called print_all_metadata (older versions may
    // have this as debris.
    //
    ostream& operator<<(ostream& os, Metadata& md)
    {
        map<string,string>::iterator sptr;
        for(sptr=md.mstring.begin();sptr!=md.mstring.end();++sptr)
        {
            os << (*sptr).first <<" "<<(*sptr).second<<endl;
        }
        map<string,long>::iterator iptr;
        for(iptr=md.mint.begin();iptr!=md.mint.end();++iptr)
        {
            os << (*iptr).first <<" "<<(*iptr).second<<endl;
        }
        map<string,double>::iterator rptr;
        for(rptr=md.mreal.begin();rptr!=md.mreal.end();++rptr)
        {
            os << (*rptr).first <<" "<<(*rptr).second<<endl;
        }
        map<string,bool>::iterator bptr;
        for(bptr=md.mbool.begin();bptr!=md.mbool.end();++bptr)
        {
            os << (*bptr).first;
            if((*bptr).second)
                os<<" true"<<endl;
            else
                os<<" false"<<endl;
        }
        return os;
    }
    //
    // Small function to extract the entire metadata contents to a pf.
    // Implementation here is very crude being a memory pig and simultaneously
    // prone to failure with finite buffer to hold content.
    //
    Pf *Metadata_to_pf(Metadata& md)
    {
        const int BUFSIZE(65536);
        char  buf[BUFSIZE];
        ostringstream pfinp(buf);
        pfinp<< md;
        Pf *pf;
        pfcompile(const_cast<char *>(pfinp.str().c_str()),&pf);
        return(pf);
    }
}                                                 // Termination of namespace SEISPP definitions
