#include <string.h>
#include "ak_ahheader.h"

get_null_ak_ahhead(hed)
ak_ahhed	*hed;
{
int	i;

	strcpy(hed->station.code,"null");
	strcpy(hed->station.chan,"null");
	strcpy(hed->station.stype,"null");
	hed->station.slat= 0.0;
	hed->station.slon= 0.0;
	hed->station.elev= 0.0;
	hed->station.DS= 0.0;
	hed->station.A0= 0.0;
	for(i=0; i< NOCALPTS; ++i)
	{
		hed->station.cal[i].pole.r= 0.0;
		hed->station.cal[i].pole.i= 0.0;
		hed->station.cal[i].zero.r= 0.0;
		hed->station.cal[i].zero.i= 0.0;
	}

	hed->event.lat= 0.0;
	hed->event.lon= 0.0;
	hed->event.dep= 0.0;
	hed->event.ot.yr= (short)0;
	hed->event.ot.mo= (short)0;
	hed->event.ot.day= (short)0;
	hed->event.ot.hr= (short)0;
	hed->event.ot.mn= (short)0;
	hed->event.ot.sec= 0.0;
	strcpy(hed->event.ecomment,"null");

	hed->record.type= (short)0;
	hed->record.ndata= 0L;
	hed->record.delta= 0.0;
	hed->record.maxamp= 0.0;
	hed->record.abstime.yr= (short)0;
	hed->record.abstime.mo= (short)0;
	hed->record.abstime.day= (short)0;
	hed->record.abstime.hr= (short)0;
	hed->record.abstime.mn= (short)0;
	hed->record.abstime.sec= 0.0;
	hed->record.rmin= 0.0;
	strcpy(hed->record.rcomment,"null");
	strcpy(hed->record.log,"null");

	for(i=0; i< NEXTRAS; ++i)
		hed->extra[i]= 0.0;

	return;
}
