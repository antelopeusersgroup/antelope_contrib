#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "tttaup.h"
#include "stock.h"

#define KM_PER_DEG 111.195

#define	MAXPHASES		60
#define	SIZE_PHCD	16

/* When the computed slope from the previous time point exceeds
JUMP_MAX or is less than JUMP_MIN the point is defined as a jump
discontinuity that cannot be safely interpolated. Note jump
used is slowness, not velocity because large velocities happen
all the time.*/

#define JUMP_MIN 0.0
#define JUMP_MAX 1.0

static int
tt_taup_slowness (elat, elon, slat, slon, selev, vp_cor, vs_cor, 
		nph, times, slowness, zslowness, dslowness, phs)

double     elat;
double           elon;
double                 slat;
double                       slon;
double                             selev;
double                                    vp_cor;
double                                            vs_cor;
int *          nph;
double **           times;
double **	    slowness ;
double **	    zslowness ;
double **	    dslowness ;
char ***                    phs;

{
	static int maxphases;
	static int nphase;
	static float del, tt[MAXPHASES], dtdd[MAXPHASES], dtdh[MAXPHASES], dddp[MAXPHASES];
	static char phcd[MAXPHASES][SIZE_PHCD];
	static char *phss[MAXPHASES];
	static double tts[MAXPHASES], dtdds[MAXPHASES], dtdhs[MAXPHASES], dddps[MAXPHASES];
	int k, l;

	*nph = 0;
	maxphases = MAXPHASES;
	del = ddistance (elat, elon, slat, slon);
	trtm_ (&del, &maxphases, &nphase, &tt[(*nph)], &dtdd[(*nph)],
			&dtdh[(*nph)], &dddp[(*nph)], &(phcd[(*nph)]), SIZE_PHCD);
	if (nphase <= 0) return (1);
	for (k=(*nph); k<(*nph)+nphase; k++) {
		for (l=0; l<SIZE_PHCD-1; l++) if (phcd[k][l] == ' ') break;
		phcd[k][l] = '\0';
	}
	(*nph) += nphase;
	for (k=0; k<(*nph); k++) {
		phss[k] = &(phcd[k][0]);
		tts[k] = tt[k];
		dtdds[k] = dtdd[k] ; 
		dtdhs[k] = dtdh[k] ; 
		dddps[k] = dddp[k] ; 
	}
	*times = tts;
	*slowness = dtdds ; 
	*zslowness = dtdhs ;
	*dslowness = dddps ;
	*phs = phss;
	return (1);
}
void usage()
{
	fprintf(stderr,"Usage:\ntaup_convert  phase [-d0 x -ddelta x -z0 x -dz x -ndelta n -nz n]\n");
}
main(int argc, char **argv)
{
	char *phase_desired;
	double d0=0.0,ddelta=1.0,z0=0.0,dz=5.0;
	int ndelta=180, nz=100;
	double z, delta;
	int i,j,k;
	/* times hold travel times, slowness - slowness in sec/degree,
	zslowness = vertical slowness in s/km, dslowness = dp/ddelta.  */
	double *times, *slowness, *zslowness, *dslowness;
	/* These are values extracted from above list, or set to no arrival flag*/
	double t, p, dpdz, ddeltadp, dpddelta;
	char **phases;
	int nphases;
	char *current_phase;
	char previous_phase[30];  
	char previous_branch;  
	double previous_time;
	char *err="ERROR";
	char branch;
	double ux, u;
	double *v;  /* Holds velocity model values at source depth 
		points */
	double vavg=0.0;
	double dtdx;

	if(argc < 2)
	{
		usage();
		exit(1);
	}
	phase_desired = argv[1];
	for(i=2;i<argc;++i)
	{
		if(!strcmp(argv[i],"-d0"))
		{
			++i;
			d0 = atof(argv[i]);
		}
		else if(!strcmp(argv[i],"-z0"))
		{
			++i;
			z0 = atof(argv[i]);
		}
		else if(!strcmp(argv[i],"-ddelta"))
		{
			++i;
			ddelta = atof(argv[i]);
		}
		else if(!strcmp(argv[i],"-dz"))
		{
			++i;
			dz = atof(argv[i]);
		}
		else if(!strcmp(argv[i],"-ndelta"))
		{
			++i;
			ndelta = atoi(argv[i]);
		}
		else if(!strcmp(argv[i],"-nz"))
		{
			++i;
			nz = atoi(argv[i]);
		}
		else
		{
			fprintf(stderr,"Unrecognized argument->%s\n",argv[i]);
			usage();
		}
	}
	fprintf(stdout,"nx  %d\nnz %d\n", ndelta, nz);
	fprintf(stdout,"x0  %.13g\nz0  %.13g\n",d0,z0);
	fprintf(stdout,"dx  %.13g\ndz  %.13g\n",ddelta, dz);
	fprintf(stdout,"uniform_grid_time_slowness_table &Tbl{\n");

	v = (double *)calloc(nz,sizeof(double));
	if(v == NULL)  
	{
		fprintf(stderr,"Fatal:  cannot alloc memory\n");
		exit(0);
	}
	/* We loop through a distance and depth range making repeated
	calls to tt_taup_slowness, which is an undocumented interface
	written by Dan Q to the Taup library.  We go ahead and 
	set the library to calculate "all" phases, then extract the
	phase we want looping through the entire depth range as
	the outer loop.  There are some bizarre pointer combinations
	that make this messy and I'm unsure why some of this is
	necessary.  */
	if ( tt_taup_set_phases ("all") == 0 ) {
		fprintf(stderr,"Fatal error trying to call taup library\n");
		exit(1);
	}

	for(j=0,z=z0;j<nz;++j,z+=dz)
	{
		fprintf(stdout,"#  Depth = %.13g\n",z);
		tt_taup_set_event_depth (z) ;
		for(i=0,delta=d0,previous_branch='0';i<ndelta;++i,delta+=ddelta)
		{
			tt_taup_slowness (0.0, 0.0, 0.0, delta, 0.0, 0.0, 0.0, 
				&nphases, &times,&slowness, 
				&zslowness, &dslowness, &phases) ;
			/*The above function returns all the phases it knows
			about at this distance in order of arrival.  We
			now take one of two actions.  We either scan for 
			a special phase, or if the phase is "P" or "S" we
			we hunt for a first arrival branch.  The code to
			do this is stolen from Dan Q's interface code. */
			if(nphases < 1) {
				fprintf(stderr,"%s (Warning):  tt_taup_slowness found no arrivals at delta = %.13g, depth = %.13g\n",argv[0],delta, z);
				t = -1.0;
				p = -1.0;
				dpdz = -1.0;
				ddeltadp = 1.0;
				current_phase = err;
			}
			else
			{
				if(!strcmp(phase_desired,"P"))
				{
					if(delta <= 105.0 ) 
					{
						t=times[0];
						p=slowness[0];
						dpdz=zslowness[0];
						ddeltadp=dslowness[0];
						current_phase = phases[0];
					}
					/* This odd code segment hunts for the
					first P phase after Pdiff because this
					phase is seldom observed as a first 
					arrival.  Modified from DSAP 
					pphase_slowness code.  */
					else { 
	    				   if ( strstr(phases[0], "Pdiff") != NULL 
							&& nphases > 1 ) {
						t = times[0];
						p=slowness[0];
						dpdz=zslowness[0];
						ddeltadp=dslowness[0];
						current_phase = phases[0];
						for ( k=1 ; k<nphases ; k++ ) 
							if(((phases[k][0])=='P')
							  || ((phases[k][0])=='p') 
							  && (strstr(phases[k],"Pdiff") == NULL))
							{
							  t=times[k];
							  p=slowness[k];
							  dpdz=zslowness[k];
							  ddeltadp=dslowness[k];
							  current_phase = phases[k];
			    				  break ;
					        	}
					  }
	    				  else
					  {
						t = times[0];
						p=slowness[0];
						dpdz=zslowness[0];
						ddeltadp=dslowness[0];
						current_phase = phases[0];
					  }
				    }
				}
				else if(!strcmp(phase_desired,"S"))
				{
					/* I repeat Dan's code here, but I think we should 
					not really use a branch of S beyond the core shadow.
					This code would use things like SS for an "S" phase */
					for ( k=0 ; k<nphases ; k++ ) 
						if (phases[k][0] == 'S'
		    					&& strchr(phases[k], 'p') == 0 
		    					&& strchr(phases[k], 'P') == 0 ) break ;

					if( k >= nphases )
					{
						t= -1.0;
						p= -1.0;
						dpdz= -1.0;
						ddeltadp= -1.0;
						current_phase = err;
					}
					else
					{
						t=times[k];
						p=slowness[k];
						dpdz=zslowness[k];
						ddeltadp=dslowness[k];
						current_phase = phases[k];
					}
				}
				else
				{
					t= -1.0;
					p= -1.0;
					dpdz= -1.0;
					ddeltadp= -1.0;
					current_phase = err;

					for ( k=0 ; k<nphases ; k++ )
						if (strcmp(phases[k],phase_desired) == 0) {
							t=times[k];
							p=slowness[k];
							dpdz=zslowness[k];
							ddeltadp=dslowness[k];
							current_phase = phases[k];
							break;
						}
				}
			}
			if(i==0)  
			{
				strcpy(previous_phase,current_phase);
				previous_time = t;
			}
			if(!strcmp(current_phase,err))
			{
				branch = 'n';
			}
			else
			{
				if(!strcmp(current_phase,previous_phase))
				{
					if(dpdz > 0.0) 
						branch = 'u';
					else
						branch = 't';
				}
				else
				{
					if(!strcmp(previous_phase,err)
					  ||  (previous_branch == '0')
                                          || (previous_branch == 'u') )

					{
						if(dpdz > 0.0) 
							branch = 'u';
						else
							branch = 't';
					}
					else
					{
						dtdx = t - previous_time;
						dtdx /= (ddelta*KM_PER_DEG);
						if( (dtdx < JUMP_MIN)
						 || (dtdx > JUMP_MAX) )
							branch = 'j';
						else
							branch = 'c';
					}
				}
			} 
			if(strcmp(current_phase,err))
			{
				ux = p*180.0/M_PI;
				ux /= (6371.0 - z);
				u = sqrt(ux*ux + dpdz*dpdz);
				vavg += 1.0/u;
				/* The unit conversions that follow come from
				the fact that the units of dtdh = p here are 
				in units of s/deg while dddp = ddeltadp here 
				has units of radias)/(s/radians) */
	
				p /= KM_PER_DEG;
				dpddelta = 1.0/(6371.0*6371.0*ddeltadp);
			}
			fprintf(stdout,"%.13g %.13g %.13g %c %s\n",
			  t,p,dpddelta,branch,current_phase);
			strcpy(previous_phase,current_phase);
			previous_branch = branch;
			previous_time = t;
		}
		v[j] = vavg/((double)ndelta);
		vavg = 0.0;
	}
	fprintf(stdout,"}\n");
	fprintf(stdout,"velocities &Tbl{\n");
	for(i=0;i<nz;++i) fprintf(stdout,"%.13g\n",v[i]);
	fprintf(stdout,"}\n");
	exit(0);
}

/* $Id$ */
