#include <stdio.h>
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
		channels[chan]=comp;
		level[chan]=lev;
	}
	freetbl(t,0);
}
ThreeComponentChannelMap::ThreeComponentChannelMap(string buffer)
{
	char chanbuf[20];
	int comp,lev;
	// Just read items 3 at a time.  Not the most robust, because it
	// could easily detect an error if item count was not mod 3.
	// Minor concern
	while(sscanf("%s%d%d",chanbuf,&comp,&lev) == 3)
	{
		string chan(chanbuf);
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

