#include "response.h"
#include "db.h"
#include "tr.h"
#include <math.h>
/* Corrects spectra values stored in f by 1/response of instrument.

f and s are parallel vectors of frequency and spectral estimates respectively
trace is a trace object defined in tr.h

Error returns:

	0 - normal return, aok
	1 - dbgetv lookup failed for trace
	2 - no response info for current trace
	3 - eval_response failed during lookup at calper
	4 - eval_response failed during lookup loop 

Author:  Gary Pavlis
Written:  October 1994
*/
int 
correct_for_response(f, s, nf, trace)
	float          *f, *s;
	Dbptr           trace;
	int             nf;
{
	Response       *rsp;

	double          calib;
	char            segtype;
	double          calper;
	double          scale;
	double          omega, real, imag;
	int             i;

	if (dbgetv(trace, 0, "calib", &calib, "response", &rsp, "calper", &calper,
		   "segtype", &segtype, 0) == dbINVALID) {
		return (1);
	}
	if (rsp == NULL)
		return (2);

	/* Handle the case settype=D specially */
	if ((calper > 0.0) && (segtype == 'D')) {
		omega = 2.0 * M_PI / ((double) calper);
		if (eval_response(omega, rsp, &real, &imag))
			return (3);

		scale = ((double) calib) * sqrt(real * real + imag * imag);
	} else {
		scale = calib;
	}
	for (i = 0; i < nf; ++i) {
		omega = 2.0 * M_PI * ((double) *f);
		if (eval_response(omega, rsp, &real, &imag))
			return (4);
		s[i] *= scale * sqrt(real * real + imag * imag);
	}
	return (0);
}
