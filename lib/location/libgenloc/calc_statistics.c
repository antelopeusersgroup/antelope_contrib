#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <float.h>
#define MAXDOUBLE DBL_MAX
#include "stock.h"
#include "location.h"

extern int GenlocVerbose ;
#undef register_error
#define register_error  if ( GenlocVerbose ) elog_notify

/* Sort compare function for qsort function */
static int 
compare_float(const void *v1,const void *v2)
{
	float *x1,*x2;
	x1 = (float *)v1;
	x2 = (float *)v2;
        if(*x1<*x2)return (-1);
        else if(*x1==*x2) return (0);
        else return(1);
}


Robust_statistics calc_statistics(float *y,int ny)

/*  calc_statistics calculate robust statistics for vector y.
i.e. median and quartiles.  
  Inputs:  
	y - input vector of length ny for which statistics are to 
		be calculated
Returns median, upper, and lower quartiles in Robust_statistics structure. 

Algorithm calculates median, and quartiles by sorting input y array. 
IMPORTANT:  THIS PROCESS IS DESTRUCTIVE, AND THE Y ARRAY IS REORDERED 
ON RETURN SINCE THE ROUTINE RECEIVE A POINTER TO y.

If ny < 2 returns with error signaled by setting elements of the 
returned structure to MAXDOUBLE (defined in values.h from float.h).  If ny = 2 or 3
the interquartiles are determined from the full range and not error is
signaled.  

Author:  Gary L. Pavlis, Indiana University
Written:  1994, Minor modified Feb. 1995 for location code.  Original 
included max and min of y, which were unnecessary here.  Also 
changed definition to ansi C.

*/
{
	/* temporaries */
	float median, low_quartile, high_quartile;
	Robust_statistics stats;

	
	/* We can't do anything if ny < 2 so return an error in this case */
	if(ny < 2) 
	{
		elog_log(0,"calculate_statistics:  Insufficient data to calculate meaningful statistics\nReceived only %d data to process\n",ny);
		stats.median = MAXDOUBLE;
		stats.q1_4 = MAXDOUBLE;
		stats.q3_4 = MAXDOUBLE;
		return(stats);
	}

	/* sort y using the qsort function */
	qsort((char *)y,ny,sizeof(float),compare_float);


	/* These are done exactly using formulas appropriate for small samples*/
	if(ny%2)  /* this is case for odd number */
		median = *(y + ny/2);
	else
		median = (y[ny/2 - 1]+y[ny/2])/2.0;

	stats.median = (double) median;
	/* handle the case when ny < 4 specially to prevent seg faults */
	if(ny<4)
	{
		stats.q1_4 = y[0];
		stats.q3_4 = y[ny-1];
	}
	else
	{
		switch((ny-1)%4)
		{
		case(0):
			low_quartile = y[(ny-1)/4];
			high_quartile = y[3*(ny-1)/4];
			break;
		case(1):
			low_quartile = (0.75*y[(ny-1)/4]+0.25*y[((ny-1)/4)+1]);
			high_quartile = (0.75*y[3*(ny-1)/4]+0.25*y[(3*(ny-1)/4)+1]);
			break;
		case(2):
			low_quartile = (0.5*y[(ny-1)/4]+0.5*y[((ny-1)/4)+1]);
			high_quartile = (0.5*y[3*(ny-1)/4]+0.5*y[(3*(ny-1)/4)+1]);
			break;
		case(3):
			low_quartile = (0.25*y[(ny-1)/4]+0.75*y[((ny-1)/4)+1]);
			high_quartile = (0.25*y[3*(ny-1)/4]+0.75*y[(3*(ny-1)/4)+1]);
			break;
	
		}
		stats.q1_4 = (double) low_quartile;
		stats.q3_4 = (double) high_quartile;
	}
	return(stats);
}

/* $Id$ */
