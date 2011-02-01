#include <sstream>
#include "AttributeCrossReference.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP {
AttributeCrossReference::AttributeCrossReference(string lines_to_parse)
{
    istringstream instrm(lines_to_parse);
    do
    {
        string inkey,outkey;
        string typestr;
        instrm>>inkey;
        // unfortunately this is the normal exit
        if(instrm.eof()) break;
        instrm>>outkey;
        instrm>>typestr;
        itoe.insert(pair<string,string>(inkey,outkey));
        etoi.insert(pair<string,string>(outkey,inkey));
        if(typestr=="int" || typestr=="INT" || typestr=="integer")
            imdtypemap.insert(pair<string,MDtype>(inkey,MDint));
        else if(typestr=="real" || typestr=="REAL" || typestr=="double")
            imdtypemap.insert(pair<string,MDtype>(inkey,MDreal));
        else if(typestr=="bool" || typestr=="BOOL" || typestr=="boolean")
            imdtypemap.insert(pair<string,MDtype>(inkey,MDboolean));
        else if(typestr=="string" || typestr=="STRING")
            imdtypemap.insert(pair<string,MDtype>(inkey,MDstring));
        else
        {
            imdtypemap.insert(pair<string,MDtype>(inkey,MDinvalid));
            cerr << "AttributeCrossReference constructor (Warning):  "
                <<" Attribute with tag="<<inkey <<" is tagged with an "
                << "illegal type name="<<typestr<<endl
                <<" Set to MDinvalid.  This may cause problems downstream"
                <<endl;
        }

    }while(!instrm.eof());
}
AttributeCrossReference::AttributeCrossReference(map<string,string> int2ext,
        MetadataList& mdlist)
{
    MetadataList::iterator mdlptr;
    for(mdlptr=mdlist.begin();mdlptr!=mdlist.end();++mdlptr)
    {
        imdtypemap.insert(pair<string,MDtype>(mdlptr->tag,mdlptr->mdt));
    }
    itoe=int2ext;
    map<string,string>::iterator iptr;
    /* This extracts each pair of the map and inverts them */
    for(iptr=itoe.begin();iptr!=itoe.end();++iptr)
        etoi.insert(pair<string,string>(iptr->second,iptr->first));
}
AttributeCrossReference::AttributeCrossReference
            (const AttributeCrossReference& parent)
{
    itoe=parent.itoe;
    etoi=parent.etoi;
    imdtypemap=parent.imdtypemap;
}
AttributeCrossReference& AttributeCrossReference::operator=
                (const AttributeCrossReference& parent)
{
    if(this!=&parent)
    {
        itoe=parent.itoe;
        etoi=parent.etoi;
        imdtypemap=parent.imdtypemap;
    }
    return(*this);
}
string AttributeCrossReference::internal(string key)
{
    map<string,string>::iterator iptr;
    iptr=etoi.find(key);
    if(iptr==etoi.end()) 
        throw SeisppError(string("AttribureCrossReference::internal:  ")
                + "Cannot find attribute "+key
                + " in external to internal namespace map");
    return(iptr->second);
}
string AttributeCrossReference::external(string key)
{
    map<string,string>::iterator iptr;
    iptr=itoe.find(key);
    if(iptr==itoe.end()) 
        throw SeisppError(string("AttribureCrossReference::external:  ")
                + "Cannot find attribute "+key
                + " in internal to external  namespace map");
    return(iptr->second);
}
MDtype AttributeCrossReference::type(string key)
{
    map<string,MDtype>::iterator iptr;
    iptr=imdtypemap.find(key);
    if(iptr==imdtypemap.end()) 
        throw SeisppError(string("AttributeCrossReference::type:  ")
                + "Cannot find attribute "+key
                + " in type definitions");
    return(iptr->second);
}
int AttributeCrossReference::size()
{
    // Assume the two maps are the same size 
    return(itoe.size());
}
void AttributeCrossReference::put(string i, string e)
{
    itoe.insert(pair<string,string>(i,e));
    etoi.insert(pair<string,string>(e,i));
}

} // end SEISPP namespace encapsulation
