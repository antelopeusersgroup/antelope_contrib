#include "response.h"
#include "db.h"
#include "tr.h"
#include <math.h>
/* Corrects spectra values stored in f by 1/response of instrument.

f and s are parallel vectors of frequency and spectral estimates respectively
trace is a trace object defined in tr.h

Error returns:

	0 - normal return, aok
	-1 - open failure of instrument response file
	-2 - read response failure
	greater than zero is a count of eval_response errors 

Author:  Gary Pavlis
Written:  October 1994
*/

int correct_for_response(float *f, float *s, int nf, Dbptr trace)
{
	Response       *response;

	char 		response_file_name[512];
	int 		ierr;
	FILE		*insfile;
	double		resmag;
	double 		fresp;
	double          omega, real, imag;
	int             i;
	int 		error_count=0;

	ierr = dbextfile(trace,"instrument",response_file_name);
	if(ierr<=0) 
	{
		elog_log(0,"Response file %s could not be openned\n",response_file_name);
		return(1);
	}
	insfile = fopen(response_file_name,"r");
	if(insfile == NULL)
	{
		elog_log(0,"Response file %s fopen failure\n",response_file_name);
		return(1);
	}
	if( read_response(insfile,&response))
	{
		elog_log(0,"read_response from file %s failed\n",response_file_name);
		fclose(insfile);
		return(2);
	}
	fclose(insfile);
	for (i = 0; i < nf; ++i) {
		omega = 2.0 * M_PI * ((double) f[i]);
		if(eval_response(omega,response,&real,&imag))
		{
			elog_log(0,"Eval_response of file %s failed for omega=%f\n",
				response_file_name,omega);
			++error_count;
			continue;
		}
		resmag=hypot(real,imag);
		if(resmag>0) 
			fresp=1.0/resmag;
		else
			fresp=1.0;
		s[i]*=fresp;
	}
	free_response(response);

	return (error_count);
}
