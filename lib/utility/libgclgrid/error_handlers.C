#include <stdio.h>
#include "elog.h"
#include "gclgrid.h"
//
//This is a group of error handlers for routines in this library.
//They use the elog facility and ASSUME that elog_init has 
//already been called.
//

void GCLlookup_error_handler(int err)
{
	switch(err)
	{
	case 1:
		if(GCLverbose>1) elog_log(0,"GCLgrid::lookup point outside bounding box discarded\n");
		break;
	case -1:
		elog_notify(0,"GCLgrid::lookup convergence error\nCheck grid geometry\n");
		break;
	case 2:
		if(GCLverbose) elog_log(0,"GCLgrid::lookup found point on margins of grid\nInterpolation error possible\n");
		break;
	case -2:
		elog_die(0,"GCLgrid::lookup coding error\nRequired grid component is missing\n");
	}
}
	
