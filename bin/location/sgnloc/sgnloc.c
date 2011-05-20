#include <stdio.h>
#include <stdlib.h>
 
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "location.h"
char *format_hypo(Hypocenter *h)
{
	char *s;
	s = malloc(512);
	if(s == NULL) elog_die(1,"malloc error for output table\n");
	sprintf(s,"%g %g %g %g %g %g %g %g %g %g %g %g %g %g %d %d",
		h->lat0,h->lon0,h->z0,h->t0,
		h->lat,h->lon,h->z,h->time,
		h->dx,h->dy,h->dz,
		h->rms_raw, h->rms_weighted, h->interquartile,
		h->number_data,h->degrees_of_freedom);
	return(s);
}
		
usage()
{
	fprintf(stderr,"Usage:  genloc inpf outpf\n");
	exit(1);
}
 
main(int argc, char **argv)
{
	Pf *pf,*pf2;
	Tbl *t,*ta,*tu;
	Tbl *reason_converged, *residual;
	Location_options o;
	Arr *a,*a2;
	Arr *arr_phase;
	char *key;
	Station *s;
	Seismic_Array *sr;
	Arrival *ar;
	Slowness_vector *u;
	int i;
	Tbl *converge_history;
	char *line;

	Hypocenter h0;
	Hypocenter *hypos;

	char *inpf, *outpf;
	int ret_code;

	if(argc != 3) usage();

	inpf = argv[1];
	outpf = argv[2];
	
	/* First let's try to read the options parameter file */
	i = pfread(inpf,&pf);
	if(i != 0) elog_die(1,"Pfread error\n");

	o = parse_options_pf (pf);
	a = load_station_table(pf);
	a2 = load_array_table(pf);
 	arr_phase = parse_phase_parameter_file(pf);
	ta = read_arrivals(pf,arr_phase,a);
 	tu = read_slowness_vectors(pf,arr_phase,a2);

	h0 = initial_locate(ta, tu, o, pf);

	ret_code = ggnloc(h0,ta,tu,o,&converge_history,&reason_converged,&residual);
	if(ret_code < 0)
	{
		elog_die(1,"ggnloc failed to produce a solution\n");
	}
	else if(ret_code > 0) 
	{
		elog_complain(1,"%d travel time failures in ggnloc\nSolution ok\n",
			ret_code);
	}
	else
	{
		t = newtbl(maxtbl(converge_history));
	        for(i=0;i<maxtbl(converge_history);++i)
	        {
	                hypos = (Hypocenter *)gettbl(converge_history,i);
	                line = format_hypo(hypos);
			pushtbl(t,line);
	        }
		pf2 = pfnew(PFFILE);
		pfput_tbl(pf2,"convergence_history",t);
	
		printf("Reasons for convergence:\n");
		for(i=0;i<maxtbl(reason_converged);++i)
			printf("%s\n",gettbl(reason_converged,i));
		pfput_tbl(pf2,"convergence_criteria",reason_converged);
		pfput_tbl(pf2,"residuals",residual);
		pfwrite(outpf,pf2);
	}
}

/* $Id$ */
