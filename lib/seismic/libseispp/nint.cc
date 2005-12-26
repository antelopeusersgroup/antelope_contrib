#include "seispp.h"
namespace SEISPP {
/* this group of functions implement gap processing for time series
 * objects and their descendents called ThreeComponentSeismograms.
 * They use indexing through the STL standard container called a
 * set.  The comparison function is defined in the seispp.h file
 * that allows interval indexing.  That is, we can look up a time
 * interval through an indexing algorithm that is an intrinsic part
 * of the generic set object.  This provides a fast lookup mechanism
 * to make this processing reasonably efficient.
 *
 * Author:  Gary L. Pavlis
 * Written:  May 2003
 */

//This is a standard function on Suns found in sunmath.  
// I've added this implementation here to get this to compile on linux
int nint(double x)
{
        double remainder;
        if(x==0.0)
                return(0);
        int i0 = (int)x;
        if(i0==0)
                remainder = x;
        else if(x>0.0)
                remainder = x - ((double)i0);
	else
		remainder = x-((double)i0);
        if(x>0)
        {
                if(remainder>0.5)
                        return(i0+1);
                else
                        return(i0);
        }
        else
        {
		if(remainder<-0.5)
                        return(i0-1);
                else
                        return(i0);
        }
}
}
