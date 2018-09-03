#include <ctype.h>
#include <unistd.h>
#include <string>
#include <list>
#include <iostream>
#include <fstream>
#include <sstream>
#include "PfStyleMetadata.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
enum PfStyleInputType {PFMDSTRING, PFMDREAL, PFMDINT, PFMDBOOL, PFMDARR, PFMDTBL};
/* this is functionally similar to antelope yesno function, but
   in a more C++ style.  int return code is the same:
   0 is false, -1 for boolean true, and +1 for no match.
   One difference from antelope is that 0 or 1 for the true or
   false value will yield a -1.  That approach conflicts with
   any notion of type as there is no way to tell these from
   an int.   Problem for words too, but the words are not likely
 to be used in another context while 0 is a very common parameter value.
 This feature should not cause harm anyway because all simple
 parameters get echoed to the string map anyway.*/
int yesno(string s)
{
    if( (s=="yes") || (s=="ok") || (s=="y") || (s=="true")
            || (s=="on") || (s=="t") ) return(-1);
    if( (s=="no") || (s=="n") || (s=="false") || (s=="off")
            || (s=="f") ) return(0);
    return(1);
}
/* This internal helper tests an std::string to make a guess of
   the type.   The algorithm is:
    if( contains &Arr ) return Arr
    else if (contains &Tbl) return Tbl
    else
      enter algorithm to parse simple tokens

    The else clause defines a real as containing a ".",
    a "-", and/or a e/E.
    */
PfStyleInputType arg_type(string token)
{
    const string arrtag("&Arr");
    const string tbltag("&Tbl");
    const string period(".");
    const string expchars("eE");
    size_t found;
    found=token.find(arrtag);
    if(found!=string::npos) return(PFMDARR);
    found=token.find(tbltag);
    if(found!=string::npos) return(PFMDTBL);
    if(yesno(token)!=1) return(PFMDBOOL);
    bool hasalpha(false);
    size_t slen=token.size();
    int i;
    bool found_an_e(false);
    for(i=0;i<slen;++i)
    {
        /* This will fail if one had an odd key like 0_0 but
           would at a lot of complexity to allow that.
           It also should skip negative sign. */
        if(isalpha(token[i]))
        {
            if((token[i]=='e') || (token[i]=='E'))
            {
                if(found_an_e)
                    return(PFMDSTRING);
                else
                    found_an_e=true;
            }
            else
                return(PFMDSTRING);
        }
    }
    /* If we got here we can assume token is numbers.  Now decide
       if it is real or int by presence of a decimal point*/
    found=token.find(period);
    if(found!=string::npos) return(PFMDREAL);
    found=token.find(expchars);
    if(found!=string::npos) return(PFMDREAL);
    return(PFMDINT);
}
/* returns true if the first nonwhite space character is a # sign*/
bool is_comment_line(string testline)
{
    const string white(" \t\n");
    size_t start;
    start=testline.find("#");
    if(start==string::npos) return(false);
    /* We define a comment line as one with nothing else
       Complexity here to deal with leading white space */
    start=testline.find_first_not_of(white,0);
    if(start!=0) testline.erase(0,start);
    char c=testline[0];
    if(c=='#')
        return true;
    else
        return false;
}
PfStyleMetadata pfread(string fname)
{
    ifstream inp;
    inp.open(fname.c_str(),ios::in);
    if(inp.fail()) throw SeisppError("SEISPP::pfread: open failed for file "
            +fname);

    /* Eat up the whole file - this assumes not a large
       file following antelope pf model.  */
    list<string> alllines;
    char inbuffer[512];
    while(inp.getline(inbuffer,512))
    {
        const string white(" \n\t");
        // Probably should test for overflow right here - do not know how
        string rawline(inbuffer);
        if(rawline.find_first_not_of(white,0)==string::npos) continue;
        if(is_comment_line(rawline)) continue;
        alllines.push_back(rawline);
    }
    try {
        PfStyleMetadata result(alllines);
        return(result);
    }
    catch(...){throw;};
}

/* This is a helper for the primary constructor below.   It searches
   for closing curly bracket.  It returns a count of the number of
   lines that define this block starting from the iterator first.
   */
int find_end_block(list<string>& alllines,list<string>::iterator first)
{
    try {
        int level(0);
        int count;
        list<string>::iterator current,lastline;
        lastline=alllines.end();
        for(count=0,current=first;current!=lastline;++current)
        {
            ++count;
            if(current->find("{")!=string::npos) ++level;
            if(current->find("}")!=string::npos) --level;
            if(level==0) break;
        }
        return(count);
    }catch(...){throw;};
}
/* Note this constructor is recursive.  That is when there is
   a nest &Arr{ in a pf this constructor will call itself for
   each Arr block */
PfStyleMetadata::PfStyleMetadata(list<string> alllines) : Metadata()
{
    const string base_error("PfStyleMetadata constructor:  ");
    list<string>::iterator lptr,end_block;
    list<string> block;
    string key,token2;
    try {
    for(lptr=alllines.begin();lptr!=alllines.end();++lptr)
    {
        int i,ival;
        int lines_this_block;
        /* This is redundant, but cost is low for long term stability*/
        if(is_comment_line(*lptr)) continue;
        istringstream is(*lptr);
        is>>key;
        is>>token2;
        PfStyleInputType type_of_this_line=arg_type(token2);
        switch(type_of_this_line)
        {
            /* Note all simple type variables are duplicated in
               as strings.  This is a failsafe mechanism used
               because Metadata will always fall back to string
               map if the type specific versions fail.   Realize
               if Metadata changes this will be wasted effort. */
            case  PFMDSTRING:
                this->put(key,token2);
                break;
            case PFMDREAL:
                this->put(key,token2);
                this->put(key,atof(token2.c_str()));
                break;
            case PFMDINT:
                /* save ints as both string and int some string
                   values could be all digits. */
                this->put(key,token2);
                this->put(key,atoi(token2.c_str()));
                break;
            case PFMDBOOL:
                ival=yesno(token2);
                /* perhaps should allow for +1 return by yesno, but
                   this should not happen with the current function.
                   Further, we are saving the same item to the
                   string map too. */
                if(ival<0)
                    this->put(key,true);
                else
                    this->put(key,false);
                this->put(key,token2);
                break;
            case PFMDTBL:
            case PFMDARR:
                lines_this_block=find_end_block(alllines,lptr);
                block.clear();
                /* Skips first and last lines in this loop.  first with
                   if and last with termination condition.  Note this
                   will cause problem if a curly back is not alone on
                   the last line */
                for(i=0;i<(lines_this_block-1);++i,++lptr)
                    if(i>0)block.push_back(*lptr);
                if(type_of_this_line==PFMDTBL)
                    pftbls.insert(pair<string,list<string> >(key,block));
                else
                {
                    PfStyleMetadata thispfarr(block);
                    pfbranches.insert(pair<string,PfStyleMetadata>
                            (key,thispfarr));
                }
                break;
            default:
                throw PfStyleMetadataError(base_error
                        +"Error parsing this line->"
                        +*lptr);
        }
    }
    }catch(...){throw;};
}
/* This is the private helper function for the PFPATH constructor */
int PfStyleMetadata::merge_pfmf(PfStyleMetadata& m)
{
  try{
    /* We simply use th operator [] because of the otherwise annoying
    propety of the stl map that this will silently overwrite the previous
    value if it exists and add it if it does not */
    map<string,double>::iterator mrit;
    map<string,long>::iterator  miit;
    map<string,bool>::iterator mbit;
    map<string,string>::iterator msit;
    int count(0);
    /* these loops are monotonously similar but I don't see a way to do this
    this with a template without a major design change.  The first four
    work because the map containers are protected attribures */
    for(mrit=m.mreal.begin();mrit!=m.mreal.end();++mrit)
    {
      this->mreal[mrit->first]=mrit->second;
      ++count;
    }
    for(miit=m.mint.begin();miit!=m.mint.end();++miit)
    {
      this->mint[miit->first]=miit->second;
      ++count;
    }
    for(mbit=m.mbool.begin();mbit!=m.mbool.end();++mbit)
    {
      this->mbool[mbit->first]=mbit->second;
      ++count;
    }
    for(msit=m.mstring.begin();msit!=m.mstring.end();++msit)
    {
      this->mstring[msit->first]=msit->second;
      ++count;
    }
    /* for thse we need to use the PfStyleMetadata methods to fetch keys
    and copy components one by one */
    list<string> akey,tkey;
    akey=m.arr_keys();
    tkey=m.tbl_keys();
    list<string>::iterator kptr;
    for(kptr=tkey.begin();kptr!=tkey.end();++kptr)
    {
      list<string> t=m.get_tbl(*kptr);
      this->pftbls[*kptr]=t;
      ++count;
    }
    for(kptr=akey.begin();kptr!=akey.end();++kptr)
    {
      PfStyleMetadata pfv;
      pfv=m.get_branch(*kptr);
      this->pfbranches[*kptr]=pfv;
      ++count;
    }
    return count;
  }catch(...){throw;};
}
/* small helper - splits s into tokens and assures each result is
is a pf file with the required .pf ending */
list<string> split_pfpath(string pfbase, char *s)
{
  /* make sure pfbase ends in .pf.  If it doesn't, add it */
  string pftest(".pf");
  std::size_t found;
  found=pfbase.find(pftest);
  if(found==std::string::npos) pfbase+=pftest;
  list<string> pftmp;
  char *p;
  p=strtok(s,":");
  while(p!=NULL)
  {
    pftmp.push_back(string(p));
    p=strtok(NULL,":");
  }
  list<string> pfret;
  list<string>::iterator pfptr;
  for(pfptr=pftmp.begin();pfptr!=pftmp.end();++pfptr)
  {
    string fname;
    fname=(*pfptr)+"/"+pfbase;
    pfret.push_back(fname);
  }
  return pfret;
}
PfStyleMetadata::PfStyleMetadata(string pfbase)
{
  try{
    const string envname("PFPATH");
    char *s=getenv(envname.c_str());
    list<string> pffiles;
    if(s==NULL)
    {
      pffiles.push_back(".");
    }
    else
    {
      pffiles=split_pfpath(pfbase,s);
    }
    list<string>::iterator pfptr;
    int nread;
    for(nread=0,pfptr=pffiles.begin();pfptr!=pffiles.end();++pfptr)
    {
      // Skip pf files that do not exist
      if(access(pfptr->c_str(),R_OK)) continue;
      if(nread==0)
      {
        *this=pfread(*pfptr);
      }
      else
      {
        PfStyleMetadata pfnext=pfread(*pfptr);
        this->merge_pfmf(pfnext);
      }
      ++nread;
    }
    if(nread==0) throw SeisppError(string("PFPATH=")+s
        +" had no pf files matching" + pfbase);
  }catch(...){throw;};
}

PfStyleMetadata::PfStyleMetadata(const PfStyleMetadata& parent)
    : Metadata(parent)
{
    pftbls=parent.pftbls;
    pfbranches=parent.pfbranches;
}
list<string> PfStyleMetadata::get_tbl(const string key)
{
    map<string,list<string> >::iterator iptr;
    iptr=pftbls.find(key);
    if(iptr==pftbls.end()) throw PfStyleMetadataError(
            "get_tbl failed trying to find data for key="+key);
    return(pftbls[key]);
}
PfStyleMetadata PfStyleMetadata::get_branch(const string key)
{
    map<string,PfStyleMetadata>::iterator iptr;
    iptr=pfbranches.find(key);
    if(iptr==pfbranches.end()) throw PfStyleMetadataError(
            "get_branch failed trying to find data for key="+key);
    return(pfbranches[key]);
}
list<string> PfStyleMetadata::arr_keys()
{
    map<string,PfStyleMetadata>::iterator iptr;
    list<string> result;
    for(iptr=pfbranches.begin();iptr!=pfbranches.end();++iptr)
        result.push_back((*iptr).first);
    return(result);
}
list<string> PfStyleMetadata::tbl_keys()
{
    map<string,list<string> >::iterator iptr;
    list<string> result;
    for(iptr=pftbls.begin();iptr!=pftbls.end();++iptr)
        result.push_back((*iptr).first);
    return(result);
}
PfStyleMetadata& PfStyleMetadata::operator=(const PfStyleMetadata& parent)
{
    if(this!=&parent)
    {
        /* These are protected members of Metadata.  There is a trick to
           dynamic_cast operator= in some situations but could not
           reproduce it and make it work here. Probably because
           Metadata has no virtual members */
        mreal=parent.mreal;
        mint=parent.mint;
        mbool=parent.mbool;
        mstring=parent.mstring;
        pftbls=parent.pftbls;
        pfbranches=parent.pfbranches;
    }
    return(*this);
}
/* Output from this should be pf compatible. The use of
 the print function makes this polymorphic for operator <<*/
void PfStyleMetadata::pfwrite(ostream& ofs)
{
    ofs << dynamic_cast<Metadata&>(*this)<<endl;
    map<string,list<string> >::iterator iptr;
    for(iptr=pftbls.begin();iptr!=pftbls.end();++iptr)
    {
        ofs << (*iptr).first<< " &Tbl{"<<endl;
        /* This is not very efficient, but avoiding it makes this code
           almost impossible to comprehend.  We simply copy the tbl */
        list<string> this_tbl((*iptr).second);;
        list<string>::iterator tptr;
        for(tptr=this_tbl.begin();tptr!=this_tbl.end();++tptr)
            ofs << *tptr<<endl;
        ofs << "}"<<endl;
    }
    /* Note this output may get hard to read with nested arrs, but
       I did not view this effort as worth the time */
    map<string,PfStyleMetadata>::iterator bptr;
    for(bptr=pfbranches.begin();bptr!=pfbranches.end();++bptr)
    {
        ofs << (*bptr).first <<" &Arr{"<<endl;
        // This is not efficient, but would be very oscure otherwise*/
        PfStyleMetadata b=(*bptr).second;
        b.pfwrite(ofs);
        ofs << "}"<<endl;
    }
}

} // End SEISPP Namespace declaration
