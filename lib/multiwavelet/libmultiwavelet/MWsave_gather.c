#include <stdio.h>
#include "multiwavelet.h"
#define MW_SCRATCH_DIR "/tmp"

/* This file contains functions used to save working data passed to a
GUI.  */

/* First we need this little ancillary function.  It checks for an environmental
variable called MW_SCRATCH_DIR and if present sets the directory used to write
the results here into to something other than the default of /tmp.
The strdup for the returned value is required because getenv returns a 
pointer to the actual enviroment variable.  A seg fault occurs if we
later try to free this.
*/
char *get_scratch_directory_name(char *name)
{
	char *s;
	s = getenv("MW_SCRATCH_DIR");
	if(s==NULL)
		name=strdup("/tmp");
	else
		name=strdup(s);
	return(name);
}
/* simple, crude function to save a matrix of multiwavelet transformed data
to a series of files.  This program is quite crude with the files having
frozen names of bankN where N=0,1,...,nwavelets.  It will fail if nwavelets>10.
Files are written in binary.  First two words are integers describing the 
size of the output matrix.  Real and imaginary parts are not multiplexed,but
written into separate vectors and dumped with a call to fwrite.  The current
implementation uses a matlab script to read and plot these files.  

Arguments:
g - parent MWgather for this group of traces
z - 3d array holding complex samples of multiwavelet transformed, time aligned
	signals. z is sized as z[nwavelets][nchan][nt];
nwavelets - number of wavelets in mw transform
nchan - number of stations (channels) in ensemble.
nt - length of seismograms for each trace in the ensemble.  
t0 - relative start time of first sample in elements of z.  Relative means
	time relative to current absolute arrival time reference.
dt - sample interval

Written:  December 2001
Author:  Gary Pavlis
*/
void MWsave_gather(
	MWgather *g,
	complex ***z, int nwavelets, int nchan, int nt, 
	double t0, double dt)		
{
	char fname[30];
	FILE *fpr,*fpi;
	int i,j,k;
	double *t;
	char *dir=NULL;

	allot(double *,t,nt);
	for(i=0;i<nt;++i) t[i] = t0+dt*((double)i);

	dir = get_scratch_directory_name(dir);	

	for(i=0;i<nwavelets;++i)
	{
		sprintf(fname,"%s/wavelet%d_r",dir,i);
		fpr = fopen(fname,"w");
		sprintf(fname,"%s/wavelet%d_i",dir,i);
		fpi = fopen(fname,"w");
		for(j=0;j<nchan;++j)
		{
			fprintf(fpr," %s",g->sta[j]->sta);
			fprintf(fpi," %s",g->sta[j]->sta);
		}
		fprintf(fpr,"\n");
		fprintf(fpi,"\n");

		fprintf(fpr,"%d  %d  %lf  %lf\n",nchan,nt,t0,dt);
		fprintf(fpi,"%d  %d  %lf  %lf\n",nchan,nt,t0,dt);
		for(k=0;k<nt;++k)
		{
			fprintf(fpr,"%lf",t[k]);
			fprintf(fpi,"%lf",t[k]);
			for(j=0;j<nchan;++j) 
			{
				fprintf(fpi," %f",z[i][j][k].i);
				fprintf(fpr," %f",z[i][j][k].r);
			}
			fprintf(fpr,"\n");
			fprintf(fpi,"\n");
		}
		fclose(fpi);
		fclose(fpr);
	}
	free(t);
	free(dir);
}	
/* Parallel function for saving coherence matrix.  DAta are saved in a common
format to above to allow recycling reading section of tcl/tk gui.  We store
two files that are plotted in the gui as if they were real and imaginary parts
of complex wavelets.  To fill this out two things are written here.  First,
we save coherence estimates for each station relative to the stack.  Second, 
we save a measure of the overall stack coherence passed as the beam argument.
The later wastes space because the same vector is written for all stations.
This was judged a small price to pay for not having to have conditionals in
the gui to deal with this special case. 

Arguments:
g - parent MWgather for this group of traces
beam - nt vector with estimate of overall stack coherence over the same
	time gate.
coh - matrix of estimated coherence for each trace in the ensemble versus
	time on a stacking time gate of length nt.  matrix is 
	assume to be [nchan][nt] (nchan by nt).
nwavelets - number of wavelets in mw transform
nchan - number of stations (channels) in ensemble.
nt - length of seismograms for each trace in the ensemble.  
t0 - relative start time of first sample in elements of z.  Relative means
	time relative to current absolute arrival time reference.
dt - sample interval
*/

void MWsave_coherence(
	MWgather *g,
	double **coh,
	double *beam,
	int nchan, 
	int nt, 
	double t0, 
	double dt)		
{
	char fname[30];
	FILE *fpr,*fpi;
	int i,j,k;
	double *t;
	char *dir=NULL;

	allot(double *,t,nt);
	for(i=0;i<nt;++i) t[i] = t0+dt*((double)i);
	dir = get_scratch_directory_name(dir);	

	sprintf(fname,"%s/sta_coherence",dir);
	fpr = fopen(fname,"w");
	sprintf(fname,"%s/beam_coherence",dir);
	fpi = fopen(fname,"w");
	for(j=0;j<nchan;++j)
	{
		fprintf(fpr," %s",g->sta[j]->sta);
		fprintf(fpi," %s",g->sta[j]->sta);
	}
	fprintf(fpr,"\n");
	fprintf(fpi,"\n");

	fprintf(fpr,"%d%d%lf%lf\n",nchan,nt,t0,dt);
	fprintf(fpi,"%d%d%lf%lf\n",nchan,nt,t0,dt);
	for(k=0;k<nt;++k)
	{
		fprintf(fpr,"%lf",t[k]);
		fprintf(fpi,"%lf",t[k]);
		for(j=0;j<nchan;++j) 
		{
				fprintf(fpr," %lf",coh[j][k]);
				fprintf(fpi," %f",beam[k]);
		}
		fprintf(fpr,"\n");
		fprintf(fpi,"\n");
	}
	fclose(fpi);
	fclose(fpr);
	free(t);
	free(dir);
}	
