#include "stock.h"
#include "pf.h"
#include "multiwavelet.h"
/* Fairly general function to build an arbitrarily complex window function based on
input from list of time-weight ordered pairs in a parameter file.  The input to 
the function comes from an &Tbl{ } grouped set of lines in a parameter file.  The
name used to find this tbl is passed to the function as tblname.  This allows 
multiple window functions to be parsed with the same structure, but different names.
Here is a sample that should make this clearer:

MWstack_time_window &Tbl{
-5.0 0.0
-4.0 1.0
5.0 1.0
6.0 0.0
}

This would produce a trapezoidal shaped window between -5 and 6 with 1 second linear
tapers at the ends.  

The output of the function is an nt length vector of doubles interpolated onto a 
regular time grid from time t0 at interval dt.  

The function returns NULL and posts elog messages if there are problems.

Author:  Gary Pavlis
Written:  January 1, 2002
This is a second generation of this function written for the mwap program.  
*/

double *get_time_weight_function(char *tblname, double t0, double dt, int nt, Pf *pf)
{
	Tbl *t;
	int npairs,nused;
	int i,ii,ierr;
	double *tpairs, *wpairs;
	double *timeweight;

	t = pfget_tbl(pf,tblname);
	if(maxtbl(t)<=0)
	{
		elog_complain(0,"get_time_weight_function: %s tbl object is not in input parameter file\n",
			tblname);
		return(NULL);
	}
	npairs=maxtbl(t);
	allot(double *,tpairs,npairs);
	allot(double *,wpairs,npairs);
	for(i=0,ii=0,nused=0;i<npairs;++i)
	{
		char *line;
		line=gettbl(t,i);
		sscanf(line,"%lf%lf",tpairs+ii,wpairs+ii);
		if(i==0)
		{
			++ii;
			++nused;
		}
		else
		{
			if(tpairs[ii]<tpairs[ii-1])
			{
				elog_complain(0,"get_time_weight_function: Illegal input for time weight function:  list of time-weight pairs must be in time order\nFound %lf following %lf;  dropping this point and attempting to continue\n",
					tpairs[ii],tpairs[ii-1]);
			}
			else
			{
				++ii;
				++nused;
			}
		}
	}
	if(nused<=1)
	{
		elog_complain(0,"get_time_weight_function: Not enough time-weight pairs found to define a valid time window\nParsed only %d valid pairs.  Correct parameter file and try again\n",
			nused);
		return(NULL);
	}
	allot(double *,timeweight,nt);
	ierr = irregular_to_regular_interpolate(tpairs,wpairs,nused,
		timeweight,t0,dt,nt);
	if(ierr)
	{
		elog_complain(0,"get_time_weight_function: Interpolation of window pairs to data interval failed.\n");
		free(timeweight);
		return(NULL);
	}
	free(tpairs);
	free(wpairs);		
	return(timeweight);
}
