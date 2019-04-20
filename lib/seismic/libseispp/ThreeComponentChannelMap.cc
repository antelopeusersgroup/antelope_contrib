#include <stdio.h>
#include <sstream>
#include "stock.h"
#include "pf.h"
#include "seispp.h"
#include "ThreeComponentChannelMap.h"
using namespace std;
using namespace SEISPP;
ThreeComponentChannelMap::ThreeComponentChannelMap(Pf *pf,string tccmkey)
{
	Tbl *t;
	const string errmess("ThreeComponentChannelMap parameter file constructor:  ");
	t=pfget_tbl(pf,const_cast<char *>(tccmkey.c_str()));
	if(t==NULL) throw SeisppError(errmess
	  + string(" ThreeComponentChannelMap key for Tbl list not in parameter file"));
	map<string,int>::iterator it;
	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		char chanbuf[20];
		int comp,lev;
		line=static_cast<char *>(gettbl(t,i));
		if(sscanf(line,"%s%d%d",chanbuf,&comp,&lev) != 3)
			throw SeisppError(errmess
				+ string("error parsing Tbl in pf"));
		string chan(chanbuf);
		/* Need to only prec array for this error condition */
		it=level.find(chan);
		if(it!=level.end()) 
                    cerr << errmess<<"WARNING - duplicate channel code="
                        << chan << " for station name key="<<tccmkey<<endl
                        << "Inconsistent channel mapping is possible"<<endl;
		channels[chan]=comp;
		level[chan]=lev;
	}
	freetbl(t,0);
}
ThreeComponentChannelMap::ThreeComponentChannelMap(string buffer)
{
	string chan;
	int comp,lev;
	istringstream in(buffer);
	map<string,int>::iterator it;
	while(!in.eof())
	{
		in >> chan;
		if(in.eof()) break;
		in >> comp;
		if(in.eof()) break;
		in >> lev;
		if(in.eof()) break;
		it=level.find(chan);
		if(it!=level.end()) 
		  throw SeisppError(
			string("ThreeComponentChannelMap(string) constructor:  ")
			+ string("duplicate channel code=") 
			+ chan
			+ string("\nChan code in definition must be unique.\n")
			+ string("Use a special sta key for irregular stations") );
		channels[chan]=comp;
		level[chan]=lev;
	}
}
ThreeComponentChannelMap::ThreeComponentChannelMap(const 
	ThreeComponentChannelMap& parent)
{
	channels=parent.channels;
	level=parent.level;
}
ThreeComponentChannelMap& ThreeComponentChannelMap::operator=
		(const ThreeComponentChannelMap& parent)
{
	if(this!=&parent)
	{
		channels=parent.channels;
		level=parent.level;
	}
	return(*this);
}
int ThreeComponentChannelMap::component(string chan)
{
	string errmess("ThreeComponentChannelMap::component:  ");
	map<string,int>::iterator it;
	it=channels.find(chan);
	if(it==channels.end()) throw SeisppError(errmess
			+ chan + string(" not defined"));
	return((*it).second);
}
int ThreeComponentChannelMap::precedence(string chan)
{
	string errmess("ThreeComponentChannelMap::precedence:  ");
	map<string,int>::iterator it;
	it=level.find(chan);
	if(it==level.end()) throw SeisppError(errmess
			+ chan + string(" not defined"));
	return((*it).second);
}

