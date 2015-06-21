#include "multiwavelet.h"
/* This function computes theoretical arrival times and a slowness vector
(at the reference station for an array) based on a model and method
defined in a parameter file and using the generic travel time interface
of datascope/antelope.  

Arguments:
///inputs////
	stations - Associative array of MWstation objects used in 
			multiwavelet programs
	refsta - name of reference station to use.  The slowness 
		vector is computed based on a source location accessed
		through the db pointer (see below) and the location of
		the reference station.  If the refsta is not found in 
		the stations array the first element of the sorted list
		of stations will be used in an attempt to recover.  For
		most arrays this would be a minor error.
	phase - name of seismic phase to compute theoretical times and
		slowness vector for.  
	db - This db pointer MUST point to a single row of a db view with 
		the origin table joined as part of the view.   The hypocenter
		information is read with dbgetv passing this db pointer 
		directly.
	pf - parameter space pointer.  We search for model and method
		fields from pf to get the interface right. 
///outputs///
	times - associative array keyed by station name of theoretical
		arrival times (stored as double * s).  If there are 
		problems computing any travel times they will be absent
		from this array.  When this array is used, be aware you
		can't assume all the times are filled.
	slow - theoretical slowness vector.  If the slowness vector
		calculation fails this is set to 0 and the refsta field
		is set to "BAD".  This allows the use of the slowness 
		vector without errors in a less than ideal way.  The 
		BAD condition should be trapped.

Return codes:
	0 = normal return, no problems
	> 0 = count of travel time failures
	< 0 = nothing was computed.  Both slowness and times should be
		assumed invalid.

Author:  Gary Pavlis
Written: December 2000
*/

static Hook *hook=0;  /*Used by ttcalc */
int MWget_model_tt_slow(Arr *stations,
			char *refsta,
			char *phase,
			Dbptr db,
			Pf *pf,
			Arr **times,
			MWSlowness_vector *slow)
{
	Tbl *t;  /* used to hold keys with keysarr */
	char *key;  /* key returned from Tbl *t */
	int i;
	MWstation *s, *s0; 
	char *model;
	char *method;
	TTGeometry geometry;
	TTTime *atime;
	TTSlow *u0;
	Tbl *treturn=NULL,*ureturn=NULL;
	int error_count=0;
	double *twork;

	s0 = (MWstation *)getarr(stations,refsta);
	if(s0 == NULL) elog_complain(0,"MWget_model_tt_slow:  cannot find reference station %s\nWill arbitarily pick first station found as refererence for computing model based time and slowness\n",refsta);
	if(dbgetv(db,0,"origin.lat",&(geometry.source.lat),
		"origin.lon",&(geometry.source.lon),
		"origin.depth",&(geometry.source.z),
		"origin.time",&(geometry.source.time),0) == dbINVALID)
	{
		elog_complain(0,"MWget_model_tt_slow:  dbgetv error reading origin data\nCannot compute theoretical arrival times and slowness\n");
		return(-1);	
	}
	model = pfget_string(pf,"TTmodel");
	method = pfget_string(pf,"TTmethod");
	if( (model == NULL) || (method == NULL) )
	{
		elog_complain(0,"MWget_model_tt_slow:  TTmodel or TTmethod missing from parameter file\nCannot compute theoretical travel times and slowness vector\n");
		return(-1);
	}
	t = keysarr(stations);
	if(maxtbl(t)<=0)
	{
		elog_complain(0,"MWget_model_tt_slow:  no data to process\n");
		return(-1);
	}
	/* recover from reference station error */
	if(s0==NULL)
	{
		key = gettbl(t,0);
		
		elog_log(0,"Setting reference station to %s for travel time computation\n",
			key);
	}
	/* We now compute the slowness vector estimated for the reference
	station location at 0 elevation. */
	strcpy(geometry.receiver.name,s0->sta);
	geometry.receiver.lat = s0->lat;
	geometry.receiver.lon = s0->lon;
	geometry.receiver.z = 0.0;
	if(slow->refsta == NULL) slow->refsta = strdup(refsta);
	if(ucalc(method,model,phase,0,&geometry,&ureturn,&hook))
	{
		elog_complain(0,"MWget_model_tt_slow:  slowness vector calculation failed for reference station %s\nSetting model slowness vector to zero and attempting to continue.\n",
			s0->sta);
		slow->ux = 0.0;
		slow->uy = 0.0;
	}
	else
	{
		u0 = (TTSlow *)gettbl(ureturn,0);
		slow->ux = u0->ux;
		slow->uy = u0->uy;
	}
	/* If the output times array is not empty we have to free it up 
	and start fresh.  Otherwise we will have a memory leak or access
	old values stored there.  This probably should be handled 
	externally, but better to be redundant.*/
	if(*times != NULL) freearr(*times,free);
	*times = newarr(0);

	/* Look through the station list */
	for(i=0;i<maxtbl(t);i++)
	{
		double estatic;
		key = gettbl(t,i);
		s = (MWstation *)getarr(stations,key);
		strcpy(geometry.receiver.name,s->sta);
		geometry.receiver.lat = s->lat;
		geometry.receiver.lon = s->lon;
		geometry.receiver.z = 0.0;
		if(ttcalc(method,model,phase,0,&geometry,&treturn,&hook))
		{
			elog_complain(0,"MWget_model_tt_slow: Travel time computation failed computing travel time for station %s\nCannot compute residuals\n",
				s->sta);
			++error_count;
		}
		else
		{
			atime = (TTTime *)gettbl(treturn,0);
			estatic = compute_elevation_static(s,*slow,0.0,phase);
			allot(double *,twork,1);
			*twork = geometry.source.time
				+ (atime->value) + estatic;
			setarr(*times,key,twork);
		}
	}
	freetbl(ureturn,0);
	freetbl(treturn,0);
	return(error_count);
}

