/* This is a collection of subprograms for mwap that utilize the 
datascope db library.  Primary use is to write out special tables
defined by mwap.  This requires a schema addendum to css3.0 
Currently this is called MWaveletX.X where X.X is a version number.
I won't attempt to keep these comments consistent with an evolution 
of a special schema definition. */

#include <stdlib.h>
#include "multiwavelet.h"

#define AMPCOMP "MAJ"
/* This function creates the mwslow table output of mwap.
This table hold slowness vector estimates.

arguments:
	phase - name of seismic phase
	u - estimated slowness vector
	t0 - start time at reference station
	twin - length of analysis time window relative to t0
	array - array name to use in table for station key field
	evid - css3.0 evid of parent data
	bankid - defines unique multiwavelet bank (could be 
		extracted from traces of gather, but it is
		so deep in indirection it gets ridiculous)
	fc - center frequency (in Hz) of this wavelet bank.	
	fwin - bandwidth (in Hz) of this wavelet
	C - 3x3 covariance matrix estimate for this slowness vector
		(ux,uy, t order assumed )
	cohtype - type of coherence measure used (mapped in multiwavelet.h)
	peakcm - value of coherence measure for this fc and evid. 
	db - output database
Returns 0 if dbaddv was successful, -1 if dbaddv fails.

Author:  G Pavlis
Written:  March 2000
*/
int MWdb_save_slowness_vector(char *phase, 
	MWSlowness_vector *u,
	double t0,
	double twin,
	char *array,
	int evid,
	int bankid,
	double fc,
	double fwin,
	double *C,
	int nsta,
	int ncomp,
	int cohtype,
	double peakcm,
	Dbptr db)
{
	double slo, azimuth;
	char cmeasure[2];

	slo = hypot(u->ux,u->uy);
	azimuth = atan2(u->ux,u->uy);
	azimuth = deg(azimuth);
	db = dblookup(db,0,"mwslow",0,0);

	switch(cohtype)
	{
	case(USE_COHERENCE):
		strcpy(cmeasure,"c");
		break;
	case(USE_SEMBLANCE):
	default:
		strcpy(cmeasure,"s");
	}

	if( dbaddv(db,0,"sta",array,
		"evid",(long)evid,
		"bankid",(long)bankid,
		"phase", phase,
		"fc",fc,
		"fwin",fwin,
		"time",t0,
		"twin",twin,
		"slo",slo,
		"azimuth",azimuth,
		"cxx",C[0],
		"cyy",C[4],
		"cxy",C[1],
		"nsta",nsta,
		"ncomp",ncomp,
		"cohtype",cmeasure,
		"cohmeas",peakcm,
		"algorithm","mwap",NULL) < 0) 
	{
		elog_notify(0,
			"dbaddv error for mwslow table on evid %d fc=%lf\n",
				evid,fc);
		return(-1);
	}
	else
		return(0);
}
/* This routine saves array average amplitude estimates in a table
called mwavgamp.  This routine always adds one and only one record.

arguments:

array - array name assigned to the group of stations used to produce
	this set of averages
evid - css3.0 event id
bankid - multiwavelet group id 
phase - phase name as in css3.0
fc - center frequency (in Hz) of the wavelets used
t0 - start time at reference station
twin - length of analysis time window relative to t0
mwamp - computed array average amplitude
erramp - estimate of error in mwamp 
	Note: both mwap and erramp are log10 values derived from nm/s
	converted trace data (assumed).
ndgf - degrees of freedom in estimate of mwamp
db - pointer to output database 

Author:  G Pavlis
Written:  March 2000
*/
int MWdb_save_avgamp(char *array,
	int evid, 
	int bankid,
	char *phase,
	double fc,
	double t0,
	double twin,
	double mwamp,
	double erramp,
	int ndgf,
	Dbptr db)
{

	db = dblookup(db,0,"mwavgamp",0,0);

	/* Note algorithm and ampcomp are frozen here.   */

	if( dbaddv(db,0,"sta",array,
		"evid",(long)evid,
		"bankid",(long)bankid,
		"phase", phase,
		"fc",fc,
		"ampcomp",AMPCOMP,
		"time",t0,
		"twin",twin,
		"mwamp",mwamp,
		"erramp",erramp,
		"ndgf",ndgf,
		"algorithm","mwap",NULL) < 0) 
	{
		elog_notify(0,
			"dbaddv error for mwavgamp table on evid %d with  fc=%lf\n",
				evid,fc);
		return(-1);
	}
	else
		return(0);
}
/* This function saves three results from mwap in three output extension
tables.  Quantities here are all related to static estimates, although
the one is done more out of convenience than logic.  That is, we produce
two static tables:  one for time statics and a second for relative 
amplitudes.  Note the amplitude statics are all relative amplitudes
passed in.  These are converted to db here before writing them out
using a standard formula. We also write a table holding signal to 
noise ratio estimates.  The computed time windows for these estimates
generally are different from those for the static tables.  Here is
the rather long argument list:

evid - css3.0 event id
bankid - multiwavelet group id 
phase - phase name as in css3.0
fc - center frequency (in Hz) of the wavelets used
t0 - start time at reference station
twin - length of analysis time window relative to t0
refelev - reference elevation in km used for elevation static estimates
g - MWgather structure from which a lot of base information is extracted.
	In fact, the gather is parsed so every nonnull entry in the gather
	structure should yield a row in the output database for each call
	to this function.
moveout - relative moveout time vector (parallel to g entries) for
	each station
statics - result array of static estimates indexed by station
stations - array of MW station objects
snrarr - array of Signal_to_Noise objects indexed by sta (Warning
	mwap has a stack of these for each frequency band being analyzed.)
arrivals - array of current measured arrival times keyed by sta name
model_times - array of theoretical arrival times for these same data
	(Both arrivals and model_arrivals contain double *'s to times)
db - output database 

Author:  G Pavlis
Written:  March 2000
*/
 	
	
int MWdb_save_statics(
	int evid, 
	int bankid,
	char *phase,
	double fc,
	double t0,
	double twin,
	double refelev,
	MWgather *g,
	double *moveout,
	Arr *statics,
	Arr *stations,
	Arr *snrarr,
	Arr *arrivals,
	Arr *model_times,
	Dbptr db)
{
	char *sta;
	int i;
        MWstatic *mws;
	MWstation *s;
	Signal_to_Noise *snr;
	int errcount=0;
	double time;
	int nsta;
	Dbptr dbt, dba, dbsnr;
	double ampdb, aerrdb;  
	double *atime,*modtime,resid;
	int ierr;

	dbt = dblookup(db,0,"mwtstatic",0,0);
	dba = dblookup(db,0,"mwastatic",0,0);
	dbsnr = dblookup(db,0,"mwsnr",0,0);

	nsta = g->nsta;

	for(i=0;i<nsta;++i)
	{
		/* We keep all snr estimates that can be found in the snarr
		array.  Station with low signal to noise ratio are automatically
		deleted in processing so it is useful to record this fact in
		the database in the mwsnr table.  Note we silently skip stations
		not found in the array for snr */
		sta = g->sta[i]->sta;
 		snr = (Signal_to_Noise *)getarr(snrarr,sta);
		if(snr != NULL)
		{
		/* negative snr values are used to flag null entries
		that are still processed.  We don't want these store
		here though so we skip them.*/
		  if (((snr->ratio_z) > 0.0)
			&& ((snr->ratio_n) > 0.0)
			&& ((snr->ratio_e) > 0.0)
			&& ((snr->ratio_3c) > 0.0)  )
		  {
		    if( dbaddv(dbsnr,0,
			"sta",sta,
			"fc",fc,
			"bankid",(long)bankid,
			"phase",phase,
			"evid",(long)evid,
			"nstime",snr->nstime,
			"netime",snr->netime,
			"sstime",snr->sstime,
			"setime",snr->setime,
			"snrz",snr->ratio_z,
			"snrn",snr->ratio_n,
			"snre",snr->ratio_e,
			"snr3c",snr->ratio_3c,
			"algorithm","mwap",NULL) < 0) 
		    {
			elog_notify(0,"mwtstatic dbaddv error for station %s\n",sta);

			++errcount;
		    }
		  }
		}
		mws = (MWstatic *)getarr(statics,sta);

   		/* Again we silently skip stations without an entry in
		this array because they can auto-edited in processing
		so this is not an error. */
		if(mws != NULL)
                {
			s = (MWstation *)getarr(stations,sta);
			if(s == NULL)
			{
				elog_complain(0,"MWdb_save_time_statics cannot find entry for station %s\nThis should NOT happen--program may have ovewritten itself!\n",
					sta);
				continue;
			}
			/* We have to correct the start time for moveout.
			This asssumes the moveout vector has the current 
			best estimate */
			time = t0 + moveout[i];
			/* We store the residual relative to a model time.
			There may be a more elegant way to skip saving resid
			in the conditional below with dbNULL, but I couldn't
			figure out that process. */
			atime = (double *)getarr(arrivals,sta);
		    	modtime = (double *)getarr(model_times,sta);
		    	if((atime==NULL) || (modtime == NULL) )
			{
				ierr = dbaddv(dbt,0,
					"sta",sta,
					"fc",fc,
					"bankid",(long)bankid,
					"phase",phase,
					"evid",(long)evid,
					"time",time,
					"twin",twin,
					"wgt",s->current_weight_base,
					"estatic",s->elevation_static,
					"pwstatic",s->plane_wave_static,
					"rstatic",s->residual_static,
					"errstatic",mws->sigma_t,
					"ndgf",mws->ndgf,
					"datum",refelev,
					"algorithm","mwap",NULL);
			}
		    	else
			{
				resid = (*atime) - (*modtime);
				ierr = dbaddv(dbt,0,
					"sta",sta,
					"fc",fc,
					"bankid",(long)bankid,
					"phase",phase,
					"evid",(long)evid,
					"time",time,
					"twin",twin,
					"wgt",s->current_weight_base,
					"estatic",s->elevation_static,
					"pwstatic",s->plane_wave_static,
					"rstatic",s->residual_static,
					"errstatic",mws->sigma_t,
					"ndgf",mws->ndgf,
					"datum",refelev,
					"timeres",resid,
					"algorithm","mwap",NULL);
			}
			if(ierr<0)
			{
				elog_notify(0,"mwtstatic dbaddv error for station %s\n",sta);

				++errcount;
			}
			/* zero weight stations will have a static 
			time shift computed, but the amplitude will
			be meaningless.  Thus, we skip stations with 
			a zero weight */
			if((s->current_weight_base)>0.0)
			{
			/* output amplitude statics are converted to db */
			    ampdb = 20.0*(mws->log10amp);
			    aerrdb = 20.0*(mws->sigma_log10amp);
			    if( dbaddv(dba,0,
				"sta",sta,
				"ampcomp",AMPCOMP,
				"fc",fc,
				"bankid",(long)bankid,
				"phase",phase,
				"evid",(long)evid,
				"time",time,
				"twin",twin,
				"wgt",s->current_weight_base,
				"ndgf",mws->ndgf,
				"ampstatic",ampdb,
				"erramp",aerrdb,
				"algorithm","mwap",NULL) < 0) 
			    {
				elog_notify(0,"mwtstatic dbaddv error for station %s\n",sta);

				++errcount;
			    }
			}
			
		}
	}
	return(errcount);
}
/* This function saves particle motion parameter estimates to a extension
table called mwpm.  Individual station estimates and an array average
estimate are all saved in the same table.  they can be sorted out through
the key field pmtype set to "ss" for single station and "aa" for array average.

Arguments:

array - array name used as tag on the array average row
evid - css3.0 event id
bankid - multiwavelet bank id tag
phase - seismic phase name as in css3.0
fc - center frequency of band in hz.
t0 - start time at reference station for particle motion analysis
twin - length of analysis time window relative to t0
g - MWgather structure for this band.  The routine loops through
	the list defined by this complicated structure.
moveout - moveout vector.  Elements of moveout are a parallel array
	to g->sta and related quanties in the gather structure.
pmarr - particle motion structures array indexed by station name
pmerrarr - particle motion error structure array indexed by station name
pmavg - particle motion ellipse parameters for array average
pmaerr - error parameters associated with pmavg
db - output database

Author:  G Pavlis
Written:  march 2000
*/
int MWdb_save_pm(
	char *array,
	int evid, 
	int bankid,
	char *phase,
	double fc,
	double t0,
	double twin,
	MWgather *g,
	double *moveout,
	Arr *pmarr,
	Arr *pmerrarr,
	Particle_Motion_Ellipse *pmavg,
	Particle_Motion_Error *pmaerr,
	Dbptr db)
{

	int i;
	Particle_Motion_Ellipse *pm;
	Particle_Motion_Error *pmerr;
	int errcount=0;
	double time;
	int nsta;
	Spherical_Coordinate scoor;
	double majaz, majema, minaz, minema;

	db = dblookup(db,0,"mwpm",0,0);

	nsta = g->nsta;

	/* We look through the whole gather quietly skipping entries
	flagged bad with a null pointer */
	for(i=0;i<nsta;++i)
	{
                pm = (Particle_Motion_Ellipse *)getarr(pmarr,g->sta[i]->sta);
		pmerr = (Particle_Motion_Error *)getarr(pmerrarr,g->sta[i]->sta);
		/* Silently skip null entries because autoediting makes
		this happen often.  We could trap the condition where
		one of these pointers is null and the other is not, but
		this should not happen so I skip it.*/
		if( (pm != NULL) && (pmerr != NULL) )
                {
			/* We have to correct the start time for moveout.
			This asssumes the moveout vector has the current 
			best estimate */
			time = t0 + moveout[i];
			/* The pm structure stores the major and minor axes
			as unit vectors.  It is more compact and more 
			intuitive to store these quantities in spherical coord
			form (az and ema) in the database so we have to convert
			them.  NOte also all angles are stored internally
			in radians and need to be converted to degrees with
			the deg for external consumption. */
			scoor = unit_vector_to_spherical(pm->major);
			/* Note azimuth in geographical coordinates is
			not the same as the phi angle in spherical coordinates
			used here.  It is 90 - phi */
			majaz = 90.0 - deg(scoor.phi);
			majema = deg(scoor.theta);
			scoor = unit_vector_to_spherical(pm->minor);
			minaz = 90.0 - deg(scoor.phi);
			minema = deg(scoor.theta);
			if( dbaddv(db,0,
				"sta",g->sta[i]->sta,
				"bankid",(long)bankid,
				"fc",fc,
				"phase",phase,
				"evid",(long)evid,
				"time",time,
				"twin",twin,
				"pmtype","ss",
				"majoraz",majaz,
				"majorema",majema,
				"minoraz",minaz,
				"minorema",minema,
				"rect",pm->rectilinearity,
				"errmajaz",deg(pmerr->dphi_major),
				"errmajema",deg(pmerr->dtheta_major),
				"errminaz",deg(pmerr->dphi_minor),
				"errminema",deg(pmerr->dtheta_minor),
				"errrect",pmerr->delta_rect,
				"majndgf",pmerr->ndgf_major,
				"minndgf",pmerr->ndgf_minor,
				"rectndgf",pmerr->ndgf_rect,
				"algorithm","mwap",NULL) < 0) 
			{
				elog_notify(0,"dbaddv error in mwpm table for station %s\n",
                                        g->sta[i]->sta);

				++errcount;
			}
		}
	}
	/* now we add a row for the array average.  This is flagged only
	by the pmtype field.  */
	scoor = unit_vector_to_spherical(pmavg->major);
	majaz = 90.0 - deg(scoor.phi);
	majema = deg(scoor.theta);
	scoor = unit_vector_to_spherical(pmavg->minor);
	minaz = 90.0 - deg(scoor.phi);
	minema = deg(scoor.theta);
	if( dbaddv(db,0,
		"sta",array,
		"bankid",(long)bankid,
		"fc",fc,
		"phase",phase,
		"evid",(long)evid,
		"time",time,
		"twin",twin,
		"pmtype","aa",
		"majoraz",majaz,
		"majorema",majema,
		"minoraz",minaz,
		"minorema",minema,
		"rect",pmavg->rectilinearity,
		"errmajaz",deg(pmaerr->dphi_major),
		"errmajema",deg(pmaerr->dtheta_major),
		"errminaz",deg(pmaerr->dphi_minor),
		"errminema",deg(pmaerr->dtheta_minor),
		"errrect",pmaerr->delta_rect,
		"majndgf",pmaerr->ndgf_major,
		"minndgf",pmaerr->ndgf_minor,
		"rectndgf",pmaerr->ndgf_rect,
	   "algorithm","mwap",NULL) < 0) 
	{
		elog_notify(0,"dbaddv error saving array average particle motion parameters in mwpm table for evid %d\n",
			evid);
		++errcount;
	}
	return(errcount);
}

/* This function loads a set of MWbasis objects from an input
database.  The name of the database is passed through the parameter
space.  It is functionally similar to load_multiwavelets_pf.

Arguments:

db - input database that contains a "mwdisc" table that indexes a selection
	of multiwavelet basis functions
pf - parameter object
nwavelets - returned as the number of wavelets found in the bank requested
	through definitions in pf
bankid - an id tag on this bank of multiwavelet functions  (returned).

The function returns a pointer to an array of MWbasis objects of length 
*nwavelets.  The fact that the function returns this object and also sets
*nwavelets and bankid is admittedly an abomination, but the syntax for 
using something like MWbasis ** causes so much confusion I considered
this the lesser of two evils.

Author:  Gary Pavlis
Written: April 2002
*/

MWbasis *load_multiwavelets_db(Dbptr db, Pf *pf,int *nwavelets, int *bankid)
{
	MWbasis *mwb=NULL;
	char *select_condition;
	Dbptr dbv;
	long nrecords;
	/* These are the attribute in the mwdisc table minus
	those returned in the argument list */

	long nsamp,foff;
	double f0,fw;
	char datatype[4],mworder[4];
	char fname[128];
	FILE *fp;
	int i,j,jj;
	
	dbv = dblookup(db,0,"mwdisc",0,0);
	if(dbv.record == dbINVALID) elog_die(0,"load_multiwavelets_db:  no mwdisc table in input database\nThese are required for multiwavelet transform\nCheck database descriptor\n");
	select_condition=pfget_string(pf,"multiwavelet_select_condition");
	if(select_condition==NULL)
		elog_die(0,"Missing required parameter string called multiwavelet_select_condition\nThis is required when loading multiwavelets from a database\n");
	dbv=dbsubset(dbv,select_condition,0);
	dbquery(dbv,dbRECORD_COUNT,&nrecords);
	if(nrecords<=0)
		elog_die(0,"load_multiwavelet_db:  no records in mwdisc match condition: %s\n",
			select_condition);
	else if (nrecords>1)
		elog_complain(0,"load_multiwavelet_db:  multiple records in mwdisc match the condition %s\nUsing first record found\n",
			select_condition);
	dbv.record=0;
	dbgetv(dbv,0,"bankid",(long)bankid,
		"nsamp",&nsamp,
		"nwavelets",nwavelets,
		"f0",&f0,
		"fw",&fw,
		"datatype",datatype,
		"foff",&foff,
		"mworder",mworder,NULL);
	if(strcmp(datatype,"t4"))
		elog_die(0,"multiwavelets must be in t4 binary form\n");
	if(strcmp(mworder,"ti") || strcmp(mworder,"ts") )
	{
		if(dbextfile(dbv,0,fname)<=0)
			elog_die(0,"load_multiwavelet_db: multiwavelet basis file %s not found\n",
				fname);
		fp = fopen(fname,"r");
		if(foff>0) fseek(fp,foff,0);
		allot(MWbasis *,mwb,*nwavelets);
		for(i=0;i<*nwavelets;++i)
		{
			allot(float *,mwb[i].r,nsamp);
			allot(float *,mwb[i].i,nsamp);
			mwb[i].n=nsamp;
			mwb[i].f0=f0;
			mwb[i].fw=fw;
		}
		
		if(!strcmp(mworder,"ti"))
		{
			/* This is case for storage as complex number */
			float *buffer;
			allot(float *,buffer,2*nsamp);
			for(i=0;i<*nwavelets;++i)
			{
				if(fread(buffer,sizeof(float),2*nsamp,fp)
					!= (2*nsamp))
				{
					elog_die(0,"read error on file %s while reading wavelet %d\n",
						fname,i);
				}
				for(j=0,jj=0;j<nsamp;++j,jj+=2)
				{
					mwb[i].r[j]=buffer[jj];
					mwb[i].i[j]=buffer[jj+1];
				}
			
			}
			free(buffer);
		}
		else
		{
			for(i=0;i<*nwavelets;++i)
			{
				int nr,ni;
				nr=fread(mwb[i].r,sizeof(float),nsamp,fp);
				ni=fread(mwb[i].i,sizeof(float),nsamp,fp);
				if( (nr!=nsamp) || (ni!=nsamp) )
					elog_die(0,"read error on file %s while reading wavelet %d\nRead %d real and %d imaginary samples while expecting %ld\n",
						fname,i,
						nr,ni,nsamp);
			}
		}
	        fclose(fp);
	}
	else
		elog_die(0,"load_multiwavelet_db: don't know how to read mworder %s\nCurrently support only ti and ts\n",
			mworder);
	
	return(mwb);
}
