#include <stdio.h>
#include <strings.h>
#include "multiwavelet.h"
typedef struct MWsta_statics_{
	char sta[10];
	double rstatic;
	double errstatic;
	double pwstatic;
	double estatic;
	int ndgf;
}MWsta_statics;
Dbptr dbarrival_view(Dbptr);

/* Small function used below to map the above defined MWsta_statics
structure defined above to MWstatic structure used in mwap's
covariance function.  "statics" is the input arr keyed by sta name
and mwsarr is the output arr also keyed by sta.  Some fields in
the output structure are set to 0.0 assuming they are not used
internally here.  No NOT recycle this for that reason.  This
is a one shot function */

void map_static_arrays(Arr *statics,Arr **mwsarr)
{
	MWsta_statics *ins;
	MWstatic *mws;
	Tbl *keys;
	char *sta;
	int i;

	*mwsarr = newarr(0);
	keys = keysarr(statics);
	for(i=0;i<maxtbl(keys);++i)
	{
		sta = gettbl(keys,i);
		ins = (MWsta_statics *)getarr(statics,sta);
		allot(MWstatic *,mws,1);
		mws->dt_final = ins->rstatic;
		mws->t_raw = ins->rstatic;
		mws->log10amp=0.0;
		mws->sigma_t = ins->errstatic;
		mws->sigma_log10amp=0.0;
		mws->ndgf = ins->ndgf;
		setarr(*mwsarr,sta,mws);
	}
}

/* This is a function to compute a band average slowness vector from 
results stored in the database from mwap.  It also computes the 
covariance matrix of the average.  We use the input covariance
matrices of slowness vector components, C, stored in the database.
Using the standard weighting by inverse data covariance, it can be 
shown that the optimal estimate of the slowness vector is

uavg = (Cinvsum)^-1 [ sum i C_i^-1 u_i ]
where Cinvsum is the sum of C_i^-1 (inverse of covariance of each estimate)
and u_i is the ith slowness vector estimate.  The covariance of the 
result is:
[Cinvsum^-1][sum i C_i^-1][Cinvsum^-1]
where Cinvsum is as above and [sum i C_i^-2] is the sum of the squares of 
the inverse covariance matrices.   

Arguments:
	dbb - dbbundle pointer to grouped collection of mwslow table rows
		to be used for estimate.  They are processed using dbget_range
		against dbb.
	uavg - holds slowness vector result
	covariance - double vector of length 4 used to store covariance of
		uavg matrix.  Result is stored in FORTRAN order in a single
		vector because of use of sunperf (LAPACK) routine to compute
		inverses.

Author:  G Pavlis
Written: August 2000
*/
void MWcompute_average_slowness(Dbptr dbb, 
	MWSlowness_vector *uavg,
	double *covariance)
{
	int is, ie;
	int nest;
	double *ux,*uy;
	double *Cxx,*Cyy,*Cxy;
	double C[4],Cinvsum[4],Ci2sum[4],Cinvb[2];  
	int i,j;
	double slow,azimuth;
	int info;

	dbget_range(dbb,&is,&ie);
	nest = ie - is;

	allot(double *,ux,nest);
	allot(double *,uy,nest);
	allot(double *,Cxx,nest);
	allot(double *,Cyy,nest);
	allot(double *,Cxy,nest);

	for(dbb.record=is,i=0;dbb.record<ie;++dbb.record,++i)
	{
		if(dbgetv(dbb,0,
			"slo",&slow,
			"azimuth",&azimuth,
			"cxx",Cxx+i,
			"cyy",Cyy+i,
			"cxy",Cxy+i,0) == dbINVALID)
		{
			elog_notify(0,"dbgetv error reading mwslow table at row %d\nAttempting to continue",
				dbb.record);
			--i;
			continue;
		}
		ux[i] = slow*sin(rad(azimuth));
		uy[i] = slow*cos(rad(azimuth));
	}
	if(i<=0)elog_die(0,"Multiple read errors from mwslow table\nNo data to process\nCannot continue\n");
	/* I don't think this can actually happen in the current version
	of datascope, but it is a safe algorithm*/
	if(i!=nest)
	{
		elog_complain(0,"Only %d of %d entries from mwslow table were extracted from the database\n",
			i,nest);
		nest = i;
	}
	/* Using the LAPACK cholesky factorization routines here is
	really like using a sledge hammer to crack a peanut*/
	for(i=0;i<4;++i)
	{
		Cinvsum[i]=0.0;
		Ci2sum[i]=0.0;
	}
	Cinvb[0]=0.0;  Cinvb[1] =0.0;

	for(i=0;i<nest;++i)
	{
		C[0]=Cxx[i];
		C[1]=Cxy[i];
		C[2]=Cxy[i];
		C[3]=Cyy[i];
		dpotrf('U',2,C,2,&info);
		if(info>0)
			elog_notify(0,"Singular slowness covariance matrix detected in Cholesky factorization for estimate %d\nResults may be incorrect\n",
				i);
		dpotri('U',2,C,2,&info);
		/* Above routine writes only the upper triangle, complete it*/
		C[1]=C[2];
		for(j=0;j<4;++j) Cinvsum[j] += C[j];
		Cinvb[0] += C[0]*ux[i]+C[2]*uy[i];
		Cinvb[1] += C[1]*ux[i]+C[3]*uy[i];
	}
	/* We need a copy of Cinvsum below */
	for(j=0;j<4;++j) Ci2sum[j]=Cinvsum[j];
	/* compute the inverse of the sum of inverses */
	dpotrf('U',2,Cinvsum,2,&info);
	dpotri('U',2,Cinvsum,2,&info);
	Cinvsum[1]=Cinvsum[2];

	uavg->ux = Cinvsum[0]*Cinvb[0]+Cinvsum[2]*Cinvb[1];
	uavg->uy = Cinvsum[1]*Cinvb[0]+Cinvsum[3]*Cinvb[1];

	/* This time it is easier to use without all the related indexing
	BLAS even if it is overkill.  We use C as a workspace here. */
	C[0] = ddot(2,Ci2sum,2,Cinvsum,1);
	C[1] = ddot(2,Ci2sum+1,2,Cinvsum,1);
	C[2] = C[1];
	C[3] = ddot(2,Ci2sum+1,2,Cinvsum+2,1);
	covariance[0] = ddot(2,Cinvsum,2,C,1);
	covariance[1] = ddot(2,Cinvsum+1,2,C,1);
	covariance[2] = covariance[1];
	covariance[3] = ddot(2,Cinvsum+1,2,C+2,1);

	free(ux);  free(uy);
	free(Cxx);  free(Cyy);   free(Cxy);
}
/* The fc attribute in antelope css3.0 is stored as %lf11.6.  A
safe floating point test uses this absolute difference */
#define FCTEST 0.000002

/* This routine was added when I realized that the errstatic values
could sometimes be unrealistically small.  This makes sense because
randomly things call all agree sometimes.  So this routine computes
statistics of the errstatic values used later to set a floor on the
size of errstatic.  The use of a full MW_scalar_statistics structure
is perhaps overkill, but makes it easier to play with options below.
It isn't clear what the right choice for the cutoff is.

Arguments:
	db - input view (MUST be subsetted view of mwtstatic table
		for one and only one evid
	f0 - returned array of center frequencies of computed 
		statistics for this band.
	s - vector of MW_calc_scalar_statistics structures of length
		nbands returned to hold the statistical summary of
		errstatic values
	nbands - length of returned vectors f0 and s = number of 
		frequency bands found for this event.  
Author:  G Pavlis
*/ 
void compute_bavg_errors(Dbptr db,
	double **fc,
	MW_scalar_statistics **stat,
	int *nbands)
{
	Dbptr dbv;
	Tbl *sortkeys;
	int nrecs;
	double *work;
	int n,band;
	double f0last,f0test,errbuf;
	

	sortkeys =  newtbl(0);
	pushtbl(sortkeys,"fc");
	pushtbl(sortkeys,"sta");
	dbv = dbsort(db,sortkeys,0,0);
	dbquery(dbv,dbRECORD_COUNT,&nrecs);
	/* This is an excessively large work space, but it
	will always work */
	allot(double *,work,nrecs);
	/* This is an inefficient, but effective way to get
	a count of the number of bands */
	for(dbv.record=0,band=1;dbv.record<nrecs;++dbv.record)
        {
		dbgetv(dbv,0,"fc",&f0test,0);
		if(dbv.record==0) f0last = f0test;
		if(fabs(f0last-f0test)>FCTEST)
		{
			++band;
			f0last = f0test;
		}
	}
	*nbands = band;
		
	/* MAJOR memory leak potential here */
	allot(double *,*fc,*nbands);
	allot(MW_scalar_statistics *,*stat,*nbands);

	for(dbv.record=0,n=0,band=0;dbv.record<nrecs;++dbv.record)
	{
		dbgetv(dbv,0,"fc",&f0test,"errstatic",&errbuf,0);
		if(dbv.record==0) f0last = f0test; 
		if( (fabs(f0last-f0test)>FCTEST)
			|| (dbv.record==(nrecs-1) ) )
		{
			(*fc)[band]=f0last;
			(*stat)[band] = MW_calc_statistics_double(work,n);
			n=0;
			++band;
			f0last = f0test;
			work[0]=errbuf;
		}
		work[n] = errbuf;
		++n;
	}

	free(work);
	dbfree(dbv);
	freetbl(sortkeys,0);
}
/* This function applies the floor on errstatic using the 
results gathered from the previous function.

Arguments:
	estat - value to check
	ftest - current center frequency 
	f0 - array of center freqeuncies parallel to stats vector
	stats - vector of statistics for f0 bands
	nbands - number of elements of f0 and stats
*/

double check_efloor(double estat,
	double ftest,
	double *f0,
	MW_scalar_statistics *stats,
	int nbands)
{
	int i;

	/*This linear search is terribly inefficient, but since
	we don't expect nbands to ever be large it isn't a problem */
	for(i=0;i<nbands;++i)
	{
		if(fabs(f0[i]-ftest)<FCTEST)
		{
			if(estat<(stats[i].median))
				return(stats[i].median);
			else
				return(estat);
		}
	}
	/* This return shouldn't be hit, but we won't flag it. 
	This is a do nothing return in the event of no match
	against the f0 list*/
	return(estat);
}


/* This function computes a set of average statics from 
the mwtstatic table using a weighted sum of all entries in the table
for a given evid:sta:phase.  The input table is subsetted on evid:phase
and it is assumed the input view passed as db has been sorted by sta so 
that all the results for a given sta are sequential.  If not a lot of
bad things will probably happen.  

The computation of an average elevation static is not an exact
solution here, but we go ahead a compute it.  I may decide at the
end to recompute estatic using the final slowness vector rather
than use this average.  The difference ought to be trivially small.

Arguments:
	db - input db pointer.  It must point to a view of the mwtstatic
		table sorted so that the sta entries are sequential.
	evid - evid of event's data to process
	phase - seismic phase (used for subsetting).

Normal return is a pointer to an associative array keyed by the
station name of a special structure used internally here to store
all the results.  Returns NULL in the case of an unrecoverable error.

Author:  G Pavlis
Written:  August 2000
*/
Arr *MWcompute_average_residuals(Dbptr db,int evid, char *phase)
{
	MWsta_statics *s;
	char sset[100];
	Dbptr dbv;
	Arr *a;
	int j;
	int nrows;
	int band;
	char sta[10],sta0[10];
	double rstatic, errstatic, pwstatic, estatic;
	double pwold, rsavg, esavg, sumwt;
	double wt;
	int ndgf, ndgf_total;
	MW_scalar_statistics *stats;
	double *f0,fc;
	int nbands;
	double *west,*wresid;  /* hold weighted estimates and residuals */
	double jkscale;  /* holds jackknife estimated scale factor */


	sprintf(sset,"(evid == %d) && (phase =~ /%s/)",evid,phase);
	dbv = dbsubset(db,sset,0);
	dbquery(dbv,dbRECORD_COUNT,&nrows);
	if(nrows <= 0)
	{
		elog_notify(0,"No data in mwtstatic table for evid %d\n",
			evid);
		return(NULL);
	}
	/* This routine returns statistics of on the size of errstatics
	numbers in each band.  This is needed because errstatic values
	can sometimes become vanishingly small and yield unrealistic
	values.  */
	compute_bavg_errors(dbv,&f0,&stats,&nbands);
	allot(double *,west,nbands);
	allot(double *,wresid,nbands);
	a = newarr(0);
	dbv.record=0;
	dbgetv(dbv,0,"sta",sta0,0);
	rsavg = 0.0;  esavg = 0.0;  sumwt = 0.0;  ndgf_total=0;
	for(dbv.record=0,band=0;dbv.record<nrows;++dbv.record)
	{
		if(dbgetv(dbv,0,"fc",&fc,
			"sta",sta,
			"rstatic",&rstatic,
			"errstatic",&errstatic,
			"pwstatic",&pwstatic,
			"ndgf",&ndgf,0) == dbINVALID)
		{
			elog_notify(0,"dbgetv error on row %d processing subsetted table for residuals from evid %d\nAttempting to continue\n",
				dbv.record,evid);
			continue;
		}
		if(strcmp(sta,sta0) || (dbv.record == (nrows-1)) )
		{
			allot(MWsta_statics *,s,1);
			strcpy(s->sta,sta0);
			s->pwstatic = pwold;
			s->rstatic = rsavg/sumwt;
			s->estatic = esavg/sumwt;
			s->errstatic=sqrt((double)(ndgf_total+1))
					/sumwt;
			for(j=0;j<band;++j)wresid[j]=west[j]-(s->rstatic);
			jkscale = d1_jack_err(band,wresid);
			s->errstatic *= jkscale;
			s->ndgf = ndgf_total - 1;
			setarr(a,s->sta,s);
			rsavg = 0.0;  esavg = 0.0;
			sumwt = 0.0;  ndgf_total=0;
			strcpy(sta0,sta);
			pwold = pwstatic;
			band = 0;
		}
		pwold = pwstatic;
		errstatic = check_efloor(errstatic,fc,f0,stats,nbands);
		wt = 1.0/errstatic;
		rsavg += (rstatic*wt);
		west[band]=rsavg;
		esavg += (estatic*wt);
		sumwt += wt;
		ndgf_total += ndgf;
		++band;
	}

	free(f0);
	free(stats);
	free(west);
	free(wresid);
	dbfree(dbv);
	return(a);
}
#define MAXCON 10000.0
/* This routine is a variant of the estimate_slowness_vector
routine in mwap.  It takes the collection of average residual
static vectors found in the input array of statics and uses
these to compute a correction to the slowness vector that will
remove any tilt on the residual static surface that would trade
off with the slowness vector. i.e. it puts the minimum amount
of information possible in the residual static values.  
The residual static vector is the projected to remove any 
dependence on the slowness.  This differs little from simply 
correcting the residuals for the computed slowness vector
correction.  

Arguments:
	stations - associative array of MWstation objects keyed by
		the station name
	slow - current slowness vector.  It is altered on output
		by the computed correction.
	statics - associative array of special statics structures
		used internal in this program.  The entire list
		found is used and compared against the full list
		defined by stations.  Every element of the statics
		array is always used, but if a station cannot be
		found in the stations array the results may be
		slightly incorrect because we assume 0 dnorth and
		deast in that condition.  

Normal return is 0.  Positive indicates one or more nonfatal errors
which here means that an entry in the stations array could not be
found.  Returns -1 if there is insufficient data (<3 statics) invert
the required matrix to compute a slowness vector.  In that situation
this routine does nothing.

Author:  Gary Pavlis
Written:  August 2000
*/
int correct_residuals(
	Arr *stations,
	MWSlowness_vector *slow,
	Arr *statics)
{
	MWstation *s;
	double x[3];
	double *A,*b,*work,vt[9],sval[3];
        int m;
        int i,j;
        char *key;
        int nsvused;
	Tbl *t;
	MWsta_statics *stbar;
	int error_code=0;
	int info;

	t = keysarr(statics);
	m = maxtbl(t);
	if(m<3)
	{
		elog_complain(0,"correct_residuals:  only %d data to process\nSlowness vector is indeterminant\n",
			m);
		return(-1);
	}
	
	allot(double *,A,3*m);
	allot(double *,b,m);
	allot(double *,work,m);

	for(i=0;i<m;++i)
	{
		key = gettbl(t,i);
		stbar = (MWsta_statics *)getarr(statics,key);
		s = (MWstation *)getarr(stations,key);
		if(s == NULL)
		{
			elog_notify(0,"correct_residuals:  cannot find station %s in station list defined in the parameter file\nWill use 0 offsets which may bias results.  May want to rerun this program\n",key);
			A[i]=0.0;
			A[i+m]=0.0;
			A[i+2*m]=1.0;
			++error_code;
		}
		else
		{
			A[i] = s->deast;
			A[i+m] = s->dnorth;
			A[i+2*m] = 1.0;
		}
		b[i] = stbar->rstatic;
	}
	dgesvd('o','a',m,3,A,m,sval,NULL,m,vt,3,&info);	
	nsvused = pseudo_inv_solver(A,vt,sval,m,3,b,MAXCON,x);
	slow->ux += x[0];
	slow->uy += x[1];
	/* This routine applies the same null projector used in mwap
	to make the final residual static number unbiased wrt the 
	final slowness vector. */
	if(null_project(A,m,3,b,work))
        	elog_complain(0,"correct_residuals:  error in null projector computation\nPotential bias problems\n");

	/* Now we reset the residual static values */
	for(i=0;i<m;++i)
	{
		key = gettbl(t,i);
		stbar = (MWsta_statics *)getarr(statics,key);
		stbar->rstatic = work[i];
	}
	return(error_code);
}
/* This is a kind of messy function that updates the arrival table using the
newly computed average residual statics.  It is complicated by nasty issues
about the reference station and the need to use dbmatches to find the 
right row in the arrival table to be updated.  

Arguments:
	db - input view of event->origin->assoc->arrival view.  This table is
		subsetted to evid because is ASSUMES that the calling program
		subsetted it to have only arrivals for a particular phase.
		A software maintenance problem that must be noted.
	evid - evid to subset to
	slow - current slowness vector (elevation statics are based on this slowness)
	statics - associative array of static structures keyed by station name
	stations - associative array of MWstation objects keyed by station name
	refelev - reference elevatio passed to routine that computes elevation statics
		(same as what is used in mwap)
	refsta - reference station
	phase - name of phase results are being computed for 

Author:  Gary Pavlis
Date:  August 2000
*/
int update_arrivals(Dbptr db, 
	int evid,
	MWSlowness_vector slow, 
	Arr *statics,
	Arr *stations,
	double refelev,
	char *refsta,
	char *phase)
{
	char sset[50];
	int narrivals, nstatics;
	/* This could be done with structures but in this case parallel
	arrays are simpler.  We store sta:chan:time arrays used as
	keys for dbmatches to find the correct row of the original
	arrival table. */
	double *atime0,*atime;
	char **sta,**chan;
	int iref=-1;
	int i,error_code=0;
	Dbptr dba,dbas;
	MWstation *s,*sref;
	Tbl *match_keys,*t;
	Hook *hook;
	double reftime;
	MWsta_statics *savg;
	char stabuf[10],chanbuf[10];
	double dcshift;
	char *user;
	char authfield[16];

	user = cuserid(NULL);
	sprintf(authfield,"mwap:%-10s",user);

	hook = 0;
	sprintf(sset,"evid == %d",evid);
	db = dbsubset(db,sset,0);
	dbquery(db,dbRECORD_COUNT,&narrivals);
	nstatics = cntarr(statics);
	if(nstatics != narrivals)
	{
		elog_notify(0,"WARNING (update_arrivals):  arrival statics count mismatch\nThere are %d static estimates and %d arrivals for evid %d\n",
			nstatics,narrivals,evid);
	}
	allot(double *,atime,narrivals);
	allot(double *,atime0,narrivals);
	allot(char **,sta,narrivals);
	allot(char **,chan,narrivals);

	/* We loop through the arrival table view saving the keys and 
	hunting for the reference station */
	for(db.record=0,i=0;db.record<narrivals;++db.record,++i)
	{
		if(dbgetv(db,0,"sta",stabuf,
			"chan",chanbuf,
			"arrival.time",atime0+i,0) == dbINVALID)
		{
			elog_notify(0,"update_arrivals: dbgetv error reading row %d\nAttempting to continue\n",
				db.record);
			sta[i] = strdup("error");
			chan[i] = strdup("error");
			++error_code;
		}
		else
		{
			sta[i] = strdup(stabuf);
			chan[i] = strdup(chanbuf);
		}
		if(!strcmp(refsta,sta[i])) iref = i;
	}
	/* When the reference station is not defined, we have to find
	a different time standard.  We use a fairly crude algorithm
	here which finds the closest station to the origin that has
	an arrival time tabulated.  The time references is then taken
	from that time with no moveout correction.  Potentially dangerous,
	but since the absolute time is not very meaningful this should
	not be a big problem. */
	if(iref<0)
	{
		double *r,rmin;
		allot(double *,r,narrivals);
		for(i=0;i<narrivals;++i)
		{
			s = (MWstation *)getarr(stations, sta[i]);
			/* This constant isn't elegant, but should work*/
			if(s == NULL)
				r[i] = 9.9E99;
			else
				r[i]=hypot(s->dnorth,s->deast);
		}
		rmin = 9.9E99;
		for(i=0;i<narrivals;++i)
		{
			if(r[i]<rmin)
			{
				iref = i;
				rmin = r[i];
			}
		}
		/* this shouldn't happen, but better be sure or this
		would seg fault for sure */
		if( (iref<0) || (iref>=narrivals))
		{
			elog_complain(0,"reference scan error -- trying first arrival in list\n");
			iref = 0;
		}
		free(r);
	}
	reftime = atime0[iref];
	sref = (MWstation *)getarr(stations,refsta);
	/* If this happens we have to stop because we cannot compute a valid
	moveout correction.  Note this is independent of above search in 
	the event the reference station didn't record this event.  This will
	happen only if the reference station isn't defined in the parameter 
	file.*/
	if(sref == NULL)
		elog_die(0,"Reference station %s not found in list of stations defined in parameter file\nCannot continue\n",
			sta[iref]);
	
	/* We need this constant to avoid dc shifts in the picks */
	savg = (MWsta_statics *)getarr(statics,refsta);
	if(savg == NULL)
		dcshift = 0.0;
	else
	{
		dcshift = savg->rstatic + savg->pwstatic; 
		dcshift += compute_elevation_static(sref,slow,refelev,phase);
	}
	/* We now have to compute the new arrival times.  We compute new
	elevation statics based on the new slowness vector */
	fprintf(stdout,"Arrival corrections for evid %d\n",evid);
	for(i=0;i<narrivals;++i)
	{
		s = (MWstation *)getarr(stations, sta[i]);
		savg = (MWsta_statics *)getarr(statics,sta[i]);
		if((s==NULL) || (savg==NULL))
		{
		    if(s==NULL)
		    {
			elog_complain(0,"Internal error:  cannot find station %s in internal storage arrays\nSkipping this station for evid %d\n",
				sta[i],evid);
			free(sta[i]);
			sta[i] = strdup("error");
			++error_code;
		    }
		    else
			elog_log(0,"Station %s in arrival table does not have a computed static -- arrival not changed\n",sta[i]);
		}
		else
		{
			atime[i] = reftime + compute_moveout(s,sref,&slow);
			atime[i] += compute_elevation_static(s,slow,refelev,phase);
			atime[i] += savg->rstatic;
			atime[i] += savg->pwstatic;
			atime[i] -= dcshift;
			fprintf(stdout,"%s:  %lf\n",sta[i],atime[i]-atime0[i]);
		}
	}
			
	/*We could copy dbas below, but this makes sure dba.record defines
	the whole table */
	dba = dblookup(db,0,"arrival",0,0);
	dbas = dblookup(db,0,"arrival",0,0);
	match_keys = strtbl("sta","chan","time",0);

	for(i=0;i<narrivals;++i)
	{
		savg = (MWsta_statics *)getarr(statics,sta[i]);
		if(savg == NULL) continue;
		if(strcmp(sta[i],"error"))
		{
			dbas.record = dbSCRATCH;
			dbputv(dbas,0,
				"sta",sta[i],
				"chan",chan[i],
				"time",atime0[i],0);
			dbmatches(dbas,dba,&match_keys,&match_keys,&hook, &t);
			if(maxtbl(t)<=0)
			{
				elog_complain(0,"Cannot match arrival for %s:%s:%lf\nArrival not updated\n",
					sta[i],chan[i],atime[0]);
			}
			else
			{
				/* We shouldn't have to worry about multiple
				matches since we are using the primary 
				key of the arrival table to make the match*/
				dbas.record = (int) gettbl(t,0);
				dbputv(dbas,0,"time",atime[i],
					"deltim",savg->errstatic,
					"auth",authfield,0);
			}
			freetbl(t,0);
		}
	}

	free(atime);
	free(atime0);
	for(i=0;i<narrivals;++i)
	{
		free(sta[i]);
		free(chan[i]);
	}
	free(sta);
	free(chan);
	dbfree(db);
	return(error_code);
}
#define KMPERDEG 111.320
/* This long and ugly function store an updated estimate of the slowness
vector in both the mwslow table and arrival.  This is complicated by
two problems.  First, the arrival table is archaic and doesn't really
define the errors correctly for a slowness vector estimate like that
achievable here.  Until now there was not procedure to estimate a
full covariance matrix for a slowness vector estimate so the arrival
table has delslo and delaz.  We attempt to make a reasonable estimate
of these numbers here.  The second problem is that the uncertainty 
computed using covariances derived from the single narrow band 
averages seems to be overly optimistic.  Consequently I used the
newly computed arrival time errors stored in the statics structure
used inside this program to form a covariance estimate assuming
these are individual data uncertainties.  

Arguments:
	db - input db group pointer (MUST be the same as the one
		passed to the MW_compute_average_slowness function
		above.
	u - current slowness vector estimate
	array_name - array name to be used to tag the output sta attribute
	statics - output associate array used to update arrival table 
		(keyed by sta name)
	stations - MWstations associative array

Function has no error returns, although multiple potential errors
will post messages throughe log.

Normal return is 0.  Positive number is count of errors.

Author:  Gary Pavlis
Written:  August 2000
*/  

int save_slowness(Dbptr db,
	MWSlowness_vector u,
	char *array_name,
	Arr *statics,
	Arr *stations)
{
	int is, ie;
	int nbands;
	double *fc,*fwin,*time,*twin;
	double flow, fhigh, tmin, twinmax;
	int i;
	double fcbavg,fwinbavg;
	int match;
	int bankid, evid; 
	char phase[10];
	Arr *mwsarr;
	double C[9];
	double slow, azimuth;
	double delslo,delaz;
	double tarrival;
	char *user;
	char authfield[16];
	int err_count=0;

	/* This first step is kind of stupid, but we have to scan the
	mwslow and establish the total bandwidth of the result and 
	a useable fc.  The later is particularly ugly because fc is
	used as a key in mwslow so we must be sure it is unique 
	and a reasonable number */
	dbget_range(db,&is,&ie);
	nbands = ie - is;
	/* The use of these workspaces would be a dumb algorithm
	if nbands were large, but this will never be a problem here.*/
	allot(double *,fc,nbands);
	allot(double *,fwin,nbands);
	allot(double *,time,nbands);
	allot(double *,twin,nbands);

	for(db.record=is,i=0;db.record<ie;++db.record,++i)
	{
		if(dbgetv(db,0,"fc",fc+i,
			"fwin",fwin+i,
			"time",time+i,
			"twin",twin+i, 0) == dbINVALID)
		{
			elog_notify(0,"save_slowness: problems reading mwslow table for row %d of evid subset view\n",
				db.record);
			--i;
			++err_count;
			continue;
		}
	}
	if(i!=nbands) nbands=i;
	for(i=0;i<nbands;++i)
	{
		if(i==0)
		{
			flow = fc[0] -fwin[0]/2.0;
			fhigh = fc[0] + fwin[0]/2.0;
			tmin = time[0];
			twinmax = twin[0];
		}
		else
		{
			flow = MIN(flow,fc[i] - fwin[i]/2.0);
			fhigh = MAX(fhigh,fc[i] + fwin[i]/2.0);
			tmin = MIN(tmin,time[i]);
			twinmax = MAX(twinmax,twin[i]);
		}
	}
	if(flow<0.0) flow=0.0;
	fcbavg = (fhigh+flow)/2.0;
	fwinbavg = fhigh - flow;
	
	/* This is probably excessive, but it is safe */
	match = 1;
	while(match)
	{
		match = 0;
		for(i=0;i<nbands;++i) 
		{
			if(fcbavg==fc[i])
			{
				match = 1;
				fcbavg += 0.00001;
				break;
			}
		}
	}
	if(fcbavg>fhigh)
	{
		elog_notify(0,"Problem creating unique center frequency for band average estimate\nUsing fc=%lf which is outside passband upper limit of %d\nBlundering on anyway\n",
			fcbavg,fhigh);
		++err_count;
	}
	free(fc);  free(fwin);   free(time);  free(twin);

	/* We could have read these repeatedly in the above loop, but
	we blindly assume they all match and just grab them from the
	first record */
	db.record = is;
	dbgetv(db,0,"evid",&evid, "phase",phase,0);
	/* bankid is set to a large arbitrary number to signal that
	this estimate is tied to no specific multiwavelet but is 
	an average */
	bankid = 99999;

	/* We finally get to something beyond bookkeeping.  We need to
	recompute the slowness vector covariance.  We do this by using
	the new time error estimates contained in the Arr s passed here.
	We have an structure mismatch, however, and we have to first
	call a stupid little mapper program to allow us to use the
	same function used in mwap for consistency.  I should have
	not created the new internal structure used here as I probably
	could have gotten by with the one defined in multiwavelet.h.  
	A minor inefficiency that will not matter much unless this
	program tried to process millions of arrivals quickly.  */

	map_static_arrays(statics,&mwsarr);

	if(compute_slowness_covariance(stations,mwsarr,C))
	{
		elog_notify(0,"Errors in computing slowness vector covariance for evid %d\n",
			evid);
		++err_count;
	}

	/* Save this to the mwslow table first */
	db = dblookup(db,0,"mwslow",0,0);
	slow = hypot(u.ux,u.uy);
	azimuth = atan2(u.ux,u.uy);
        if(azimuth<0.0) azimuth += 2.0*M_PI;
        azimuth = deg(azimuth);
	if(dbaddv(db,0,"sta",array_name,
			"evid",evid,
			"bankid",bankid,
			"phase",phase,
			"fc",fcbavg,
			"fwin",fwinbavg,
			"time",tmin,
			"twin",twinmax,
			"slo",slow,
			"azimuth",azimuth,
			"cxx",C[0],
			"cyy",C[4],
			"cxy",C[1],
			"nsta",cntarr(mwsarr),
			"ncomp",3,
			"algorithm","mwapbavg",0) < 0)
	{
		elog_notify(0,"dbaddv error for mwslow table for evid %d\n",
				evid);
		++err_count;
	}

	/* Now we want to add an estimate to the arrival table too 
	since location algorithm will want to use this information.
	We have a problem in that the arrival table does not define
	the slowness vector uncertainty well using a delslo and delaz
	field.  We do this fairly crudely by using the (cxx+cyy)/2 
	and a small angle approximation for delaz
	*/
	delslo = sqrt((C[0]+C[4])/2.0);
	delaz = delslo/hypot(u.ux,u.uy);
	delaz = deg(delaz);
	/* Now arrival also stored the slowness in s/deg, so we have
	another conversion to make */
	slow *= KMPERDEG;
	delslo *= KMPERDEG;
	
	/* Another annoyance -- for whatever reason arrival uses
	the backazimuth, not the direction of propagation that mwap
	uses internally and stores in mwslow.  So, we have to switch
	the sign */
	azimuth += 180.0; /* azimuth is currently in degrees*/
	if(azimuth > 360.0) azimuth -= 360.0;

	/* This is not an absolutely accurate time, but it is
	a reasonable tag */
	tarrival = tmin + twinmax/2.0;
	user = cuserid(NULL);
	sprintf(authfield,"mwap:%-10s",user);

	db = dblookup(db,0,"arrival",0,0);
	if(dbaddv(db,0,"sta",array_name,
			"time",tarrival,
			"chan","3C",
			"iphase",phase,
			"azimuth",azimuth,
			"delaz",delaz,
			"slow",slow,
			"delslo",delslo,
			"auth",authfield,0) < 0)
	{
		elog_notify(0,"dbaddv error for arrival table with evid %d\n",
			evid);
		++err_count;
	}
	return(err_count);
	
}		

void usage(char *prog)
{
	elog_die(0,"Usage:  $s db [-pf pffile -phase phase]\n",prog);
}
/* This program takes database tables produced by mwap of slowness vector
estimates and residual statics from multiple frequency bands and averages
these to produce a global average.  This is more complicated that one 
might first think because the solution needs to have weights that depend
upon the frequency band.  Furthermore, the slowness vector estimate has
a 2-vector covariance estimate that needs to be averaged correctly to 
account from variations between bands.   Finally, the two estimates 
are not independent because a plane fit through the residual surface
can be exactly compensated by an appropriate adjustment in the phase
velocity vector.  As a result we have to apply a variant of the same
algorithm used in mwap in each band.  

Usage:
	mwapbavg db [-pf pffile]
where
	db = database (requires multiwavelet extentions tables)


The primary output of the program is to the arrival table.  The program 
will update the arrival table and add an entry for the array average 
slowness vector.  
Written:  August 2000
Author:  Gary Pavlis
*/
main(int argc, char **argv)
{
	Dbptr db,dbgrp;
	Dbptr db_bundle;
	Dbptr dbtsv;  /* sorted view of mwtstatic table */
	Dbptr dbav;  /* arrival table view */
	Pf *pf;
	char *dbname,*pfin=NULL,*phase=NULL;
	Tbl *sortkeys, *grp_tbl;
	double statime;
	Arr *stations;
	int nevents;
	MWSlowness_vector uavg;
	double Cu[4];
	Arr *static_avg;
	char sset[50];
	char *array_name;
	char *refsta;
	double refelev;
	int i;
	int errcount;


        /* Initialize the error log and write a version notice */
        elog_init (argc, argv);
        /* usual cracking of command line */
        if(argc < 2) usage(argv[0]);
        dbname = argv[1];

        for(i=2;i<argc;++i)
        {
              if(!strcmp(argv[i],"-pf"))
                {
                        ++i;
                        if(i>=argc) usage(argv[0]);
                        pfin = argv[i];
                }
                else if(!strcmp(argv[i],"-phase"))
                {
                        ++i;
                        if(i>=argc) usage(argv[0]);
                        phase = argv[i];
                }

                else
                        usage(argv[0]);
        }
        /* this sets defaults */
        if(pfin == NULL) pfin = strdup("mwapbavg");
	if(phase == NULL) phase = strdup("P");

        i = pfread(pfin,&pf);
        if(i != 0) elog_die(1,"Pfread error\n");

        if(dbopen(dbname,"r+",&db) == dbINVALID)
                elog_die(1,"Unable to open input database %s\n",dbname);

	/* We first call the multiwavelet library routine that
	builds an associative array of station objects.  That 
	stucture is a bit overkill for this program, but it's use
	assures consistency between mwap and this program as they
	should share a common parameter file that defines the
	list of stations to use.  The time field is used here
	like mwap in a less than bombproof way using the first
	time found.   Data sets spanning a long time period with
	stations coming and going will present a problem.  However,
	this is assumed to be dealt with by multiple runs of mwap
	followed by runs of this program with a common parameter
	file.  The general solution is more bookkeeping than seems
	justified here. */
	db =dblookup(db,0,"mwslow",0,0);
	db.record=0;
	dbgetv(db,0,"time",&statime,0);
	stations = build_station_objects(db,pf,statime);
        refsta = get_refsta(stations);
	uavg.refsta = strdup(refsta);
        array_name = pfget_string(pf,"array_name");
        if(array_name == NULL)
        {
                elog_complain(0,"WARNING:  array_name not defined in parameter file.  Set to default of ARRAY\n");
                array_name = strdup("ARRAY");
        }
        refelev = pfget_double(pf,"reference_elevation");

	/* We first subset the mwslow table to a single seismic phase */
	db =dblookup(db,0,"mwslow",0,0);
	sprintf(sset,"phase=~/%s/",phase);
	db = dbsubset(db,sset,0);
	/* We use a grouped view of the mwslow table below*/
	sortkeys = newtbl(0);
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"fc");
	db = dbsort(db,sortkeys,0,0);


	grp_tbl = newtbl(1);
	pushtbl(grp_tbl,"evid");
	dbgrp = dbgroup(db,grp_tbl,0,0);
	if(dbgrp.record == dbINVALID) elog_die(0,"Problems forming mwslow view\n");

	/* The algorithm below is simpler if we form a sorted view 
	of the mwtstatic table.  Sorted this way we can just work
	through the view sequentially. */
	settbl(sortkeys,1,"sta");
	settbl(sortkeys,2,"fc");
	dbtsv = dblookup(db,0,"mwtstatic",0,0);
	dbtsv = dbsort(dbtsv,sortkeys,0,0);
	if(dbtsv.record == dbINVALID) 
		elog_die(0,"Problems forming mwtstatic view\n");

	/* We need the standard event->origin->assoc->arrival view
	below.  This forms it */
	dbav = dbarrival_view(db);
	if(dbav.record == dbINVALID)
		elog_die(0,"Problems forming arrival view\n");
	/* This has to be subsetted for one phase only also */
	dbav = dbsubset(dbav,sset,0);

        dbquery(dbgrp,dbRECORD_COUNT,&nevents);
	if(nevents<=0) elog_die(0,"No data to process in mwslow table\n");
        fprintf(stdout,"%s:  start processing for %d events\n",
		argv[0],nevents);

	/* The main processing loops through the mwslow table grouped
	by evid.  We first estimate the average slowness vector and
	average residual vector.  The slowness vector is then 
	corrected based on the average residual vector, and the final
	residuals are computed with the slowness vector component 
	removed (residual of residuals).  */
        for(dbgrp.record=0;dbgrp.record<nevents;++dbgrp.record)
        {
                Dbptr db_bundle;
                int evid;
                if(dbgetv(dbgrp,0,"evid", &evid,
                        "bundle", &db_bundle,0) == dbINVALID)
                {
                        elog_complain(1,"dbgetv error for row %d of event grouped mwslow table\
nAttempting to continue by skipping to next event\n",
                                dbgrp.record);
                        continue;
                }
		else
		{
			fprintf(stdout,"Working on evid %d\n",evid);
		}

		/* This routine computes the average slowness vector
		and the covariance of the average.*/
		MWcompute_average_slowness(db_bundle,&uavg,Cu);
		if(Cu[0]<0.0)
		{
			elog_complain(0,"Error computing average slowness vector for evid %d\nNo results for this event\n",
				evid);
			continue;
		}

		/* Now we turn to the residual estimates. We pass this
		routine the sorted mwtstatic view and the evid number
		and it returns an Arr special structures keyed by
		the station name. */
		static_avg = MWcompute_average_residuals(dbtsv,evid,phase);
		if(static_avg == NULL)
		{
			elog_complain(0,"Problems computing average residuals for evid %d\nNo results for this event\n",
				evid);
			continue;
		}
		if(correct_residuals(stations,&uavg,static_avg))
		{
			elog_complain(0,"Problems in correct_residuals function.  Possible biases\n");
		}
		if(errcount=update_arrivals(dbav, evid, uavg,
			static_avg, stations, refelev, refsta, phase))
		{
			elog_notify(0,"%d nonfatal errors in update_arrival function\n",
				errcount);
		}
		/* The last thing we do is save the new slowness
		vector.  The reason we leave it to last is that
		internally we recompute the slowness vector
		covariance using the error band average error estimates
		of the statics which are more realistic than the
		narrowband estimates.*/
		if(errcount=save_slowness(db_bundle,uavg,
				array_name,static_avg,stations))
		{
			elog_notify(0,"%d nonfatal errors in save_slowness\n",
				errcount);
		}
		freearr(static_avg,free);
	}
}
