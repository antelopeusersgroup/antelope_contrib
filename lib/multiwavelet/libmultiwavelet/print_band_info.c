#include "multiwavelet.h"
/* This little function logs mw transform info converting 
nondimensional frequency band numbers stored with the basis
objects (mw) to frequency in hz based on sample rate read from
pf and decimation factors passed through decfac array of length
nbands (obtained from pf to avoid passing it ).
*/
void print_band_info(MWbasis *mw,int *decfac,Pf *pf)
{
	int nbands;
	double sample_interval;
	double f0,fw,fhigh,flow;
	double scale;
	int i;

	sample_interval = pfget_double(pf,"sample_interval");
	nbands = pfget_int(pf,"number_frequency_bands");

	fprintf(stdout,"Multiwavelet transform frequency bands\n\nBand\tf0\tf_low\tf_high\tfw\n");
	for(i=0;i<nbands;++i)
	{
		scale = 1.0/(2.0*sample_interval*decfac[i]);
		f0 = (mw->f0)*scale;
		fw = (mw->fw)*scale;
		flow = f0 - (fw/2.0);
		fhigh = f0 + (fw/2.0);
		fprintf(stdout,"%8d%8.4lf%8.4lf%8.4lf%8.4lf\n",
			i,f0,flow,fhigh,fw);
	}
}
		

/* These are small functions called only by the main processing routine 
(see below) listed first because of the usual compiling order issue.*/
void print_window_data(int *dec,
	int n, 
	Time_Window *sig, 
	Time_Window *noise,
	Pf *pf)
{
	int i;
	double sstart, send, nstart, nend;

	fprintf(stdout,"Analysis time windows in samples:\nsignal start\tsignal end\tnoise start\tnoise end\tdecimation factor\n");
	for(i=0;i<n;++i)
	{
		fprintf(stdout,"band %d:\t%d\t%d\t%d\t%d\t%d\n",
			i,
			sig[i].tstart,
			sig[i].tend,
			noise[i].tstart,
			noise[i].tend,
			dec[i]);
	}
	fprintf(stdout,"Analysis time windows in seconds:\nsignal start\tsignal end\tsignal tpad\tnoise start\tnoise end\tsignal tpad\tsample interval\n");
	for(i=0;i<n;++i)
	{
		sstart = (sig[i].tstart)*(sig[i].si);
		send = (sig[i].tend)*(sig[i].si);
		nstart = (noise[i].tstart)*(noise[i].si);
		nend = (noise[i].tend)*(noise[i].si);
		fprintf(stdout,"band %d: %lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",
			i,
			sstart,send,sig[i].tpad,
			nstart,nend,noise[i].tpad,
			sig[i].si);
	}
}
