#include <stdio.h>
#include <string>
#include "seispp.h"
/* This is an overloaded version of a file of the same name below.  It provides an
alternative interface using dir/dfile as opposed to the simple file name in the
function it calls.  Note this is a strange case of a function calling another of
the same name with different argments.
*/
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(seispp_error)
{
	string fname;
	long int foff;
	fname = dir + "/" + dfile;
	try {
		foff = vector_fwrite(x,n,fname);
	} catch ( seispp_error err) { throw err;};
	
	return(foff);
}

/*  Low level, bombproof write function for C++ for a vector of doubles.  
The function blindly saves a vector of doubles x of length n to a 
specified file.  The algorithm used is this:  
The file is openned, we seek to the file end, get the position, and then write
new data to the end of this file.  The file offset, in bytes, from ftell is
returned as a long int.  This provides a fairly bombproof way to save data 
because data are simply appended.  This avoids file name collision problems under
an assumption that the caller is saving foff to a database table like wfdisc.
*/
long int vector_fwrite(double *x,int n, string fname) throw(seispp_error)
{
	FILE *fp;
	long int foff;

	if((fp=fopen(fname.c_str(),"a")) == NULL)
		throw seispp_error("Open failed on file "+fname);
	fseek(fp,0L,2);
	foff = ftell(fp);
	if( fwrite(x,sizeof(double),n,fp)!=n) 
	{
		fclose(fp);
		throw seispp_error("fwrite error to file "+fname);
	}
	fclose(fp);
	return(foff);
}
