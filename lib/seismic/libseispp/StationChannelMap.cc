#include <stdio.h>
#include <sstream>
#include "StationChannelMap.h"
#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
StationChannelMap::StationChannelMap()
{
	string defmap;
	defmap=string("BHE 0 0\n")
		+string("BHN 1 0\n")
		+string("BHZ 2 0\n");
	ThreeComponentChannelMap def(defmap);
	chanmap.insert(map<string,ThreeComponentChannelMap>
                                ::value_type(string("default"),def));
}
StationChannelMap::StationChannelMap(Pf *pf)
{
	string base_error("StationChannelMap parameter file constructor:  ");
	Pf *pfsta,*pftbl,*pfchan;
	Tbl *t;
	ThreeComponentChannelMap *tccm;
	if(pfget(pf,const_cast<char *>("StationChannelMap"),(void **)&pfsta) != PFARR)
		throw SeisppError(base_error
			+ string("Missing StationChannelMap Arr&{}"));
	t = pfkeys(pfsta);
	for(int i=0;i<maxtbl(t);++i)
	{
		char *key;
		key=static_cast<char *>(gettbl(t,i));
		string sta(key);
		if(pfget(pfsta,key,(void **)&pfchan) != PFTBL)
			throw SeisppError(base_error
			 + string("Syntax error in parameter file parsing Tbl key =")
			 + string(key));
		try {
			tccm=new ThreeComponentChannelMap(pfsta,sta);
			// a ugly substitute for chanmap[sta]=*tccm;
			// Dark side of STL syntax showing.  
			chanmap.insert(map<string,ThreeComponentChannelMap>
				::value_type(sta,*tccm));
			delete tccm;
		}
		catch (SeisppError& serr)
		{
			serr.log_error();
			throw SeisppError(base_error
			  + string("error parsing map for station=")+sta);
		}
	}
	//
	// We must make sure default is defined.  Will throw an exception
	// if it isn't as the object requires it to be defined.
	map <string,ThreeComponentChannelMap>::iterator it;
	it=chanmap.find("default");
	if(it==chanmap.end()) throw SeisppError(base_error
		+ string("missing ThreeComponentChannelMap definition for default") );
}
		
StationChannelMap::StationChannelMap(string fname)
{
	FILE *fp;
	char sta[40],statest[40];
	char line[256];
	char buffer[2048];
	ostringstream sstr(buffer);
	string base_error("StationChannelMap file constructor:  ");
	
	fp=fopen(fname.c_str(),"r");
	if(fp==NULL) throw SeisppError(base_error
		+ string("Cannot open file ")+fname);
	fscanf(fp,"%s",sta);
	rewind(fp);
	while(fgets(line,256,fp)!=NULL)
	{
		int ichan,lev;
		char chan[40];
		sscanf(line,"%s",statest);
		if(strcmp(sta,statest))
		{
			// Land here when station name changes
			ThreeComponentChannelMap tccm(sstr.str());
			chanmap.insert(map<string,ThreeComponentChannelMap>
				::value_type(sta,tccm));
			sstr.seekp(0); // a rewind
			strcpy(sta,statest);
		}
		// reformat line here
		if(sscanf(line,"%s%s%d%d",sta,chan,&ichan,&lev)!=4)
                {
                    fclose(fp);
		    throw SeisppError(base_error
			 + string("file format error.  Check data"));
                }
		sstr << chan <<" "<<ichan<<" "<<lev<<endl;
	}
	chanmap.insert(map<string,ThreeComponentChannelMap>
		::value_type(sta,ThreeComponentChannelMap(sstr.str())));
	map <string,ThreeComponentChannelMap>::iterator it;
	it=chanmap.find("default");
	if(it==chanmap.end()) throw SeisppError(base_error
		+ string("missing ThreeComponentChannelMap definition for default") );
		
}
// This could probably be defaulted
StationChannelMap::StationChannelMap(const StationChannelMap& parent)
{
	chanmap=parent.chanmap;
}
// This could probably also have been defaulted
StationChannelMap& StationChannelMap::operator=(const StationChannelMap& parent)
{
	if(this!=&parent)
	{
		chanmap=parent.chanmap;
	}
	return(*this);
}
ThreeComponentChannelMap StationChannelMap::channels(string sta)
{
	map <string,ThreeComponentChannelMap>::iterator it;
	it=chanmap.find(sta);
	// Perhaps should trap an exception here if default is not
	// found, but above constructors assure it should always
	// be defined.  
	if(it==chanmap.end())
		it=chanmap.find(string("default"));
	return((*it).second);
}

}  // End SEISPP Namespace Declaration
