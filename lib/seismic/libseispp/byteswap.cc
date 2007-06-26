#include <string.h>
#include "swapbytes.h"
namespace SEISPP {
/*! \brief Test for little endian condition.

To handle mixed processors it is essential to know if the
word structure of this machine you are on is little or big 
endian.  Intel processors are dominate today and are little
endian while Sun machines, which are commonly used in geophysics,
are big endian.  Because it is common today to mix these platforms
on the same network a way to detect which byte order the current
machine is, is necessary.

\return true if this processor is little endian (Intel byte order).
	Conversely returns false if the processor is big endian.
\author  Gary L. Pavlis with the core algorithm stolen from the
	University of Texas supercomputer web site.
*/
bool IntelByteOrder()
{
        long i = 0x11223344; unsigned char* c = (unsigned char*) &i;
        if(*c != 0x44)
                return(false);
        else
                return(true);
}
/*! \brief Architecture indedependent procedure 
to byte swap a vector of doubles.

In the seispp library most data are stored internally as doubles.
External data representations, however, are subject to byte order
issues.  This routine will take a vector of doubles and automatically
swap bytes using a method appropriate for the parent architecture.
It should always be preceded by logic to decide if byte swapping
is necessary as this will always swap bytes one way or the other.

\param x pointer to array of doubles to be byte swapped.
\param nx number of elements in x.  This is quietly assumed
	to be correct and not bounds checking is done by this procedure.
*/

void swapdvec(double *x,int nx)
{
        double *buf=new double[nx];
        unsigned char **xptr;
        // ugly interface to low level C functions
        // requires this cast
        xptr=reinterpret_cast<unsigned char **>(&x);
        // We need to reset x before returning as the
        // antelope byte swap routines alter it
        double *x0=x;
        if(IntelByteOrder())
        {
                md2hd(xptr,buf,nx);
        }
        else
        {
                vd2hd(xptr,buf,nx);
        }
        x=x0;
        memcpy((void *)x,(void *)buf,nx*sizeof(double));
        delete [] buf;
}

} // End SEISPP namespace declaration
