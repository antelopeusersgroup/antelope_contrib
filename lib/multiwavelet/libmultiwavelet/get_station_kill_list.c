#include <stdio.h>
#include "multiwavelet.h"

/* Creates a tbl of integer row indexes in a gather of stations that
are to be killed based on input from a parameter file.  Concept is
that this would be used in combination with at tcl/tk gui 
*/
Tbl *get_station_kill_list(MWgather *g,Pf *pf)
{
	int i;
	Arr *stations_to_kill;
	Tbl *kill_list;

	stations_to_kill = pfget_arr(pf,"stations_to_kill");
	if(cntarr(stations_to_kill)<=0)return(NULL);
	kill_list = newtbl(0);
	for(i=0;i<g->nsta;++i)
	{
		char *sta;
		char *pfvalue;
		char row[5];

		pfvalue=getarr(stations_to_kill,g->sta[i]->sta);
		if(pfvalue!=NULL)
		{
			sprintf(row,"%4.4d",i);
			pushtbl(kill_list,strdup(row));
		}
	}
	return(kill_list);
}
