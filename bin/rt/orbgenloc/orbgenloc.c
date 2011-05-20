#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "db.h"
#include "stock.h"
#include "coords.h"
#include "arrays.h"
#include "bury.h"
#include "orb.h"
#include "location.h"
#include "elog.h"
#include "orbgenloc.h"

#define DEFAULT_PFFILE "orbgenloc"
#define DEFAULT_STATEFILE "state/orbgenloc"

static void usage (char *Program_Name)
{
	elog_die(0,"Usage:  %s orbserver [-S statefile -pf pffile]\n", 
		Program_Name);
}
/* Special function used in orbgenloc for repeated task sending a db record 
to the orb.  It basicially implements a standard error message if this
fails.  

Arguments:
	db - input db pointer.  It is ASSUMED that db.record is dbSCRATCH
		and the relevant record has been copied onto the scratch
		record before calling this function. 
	orb - orb descriptor to send the scratch record of db to.  
Returns 0 if all worked.  Returns negative of number of failures 
otherwise with messages placed in error log.  
Author:  Gary L. Pavlis
Written:  May 1997
*/
int save_dbrecord(Dbptr db, int orb)
{
	char *table_name;
	int ret_code=0;


	if(db2orbpkt(db,orb)) 
	{
		dbquery(db,dbTABLE_NAME,table_name);
		elog_log(0,"Error writing a record for table %s to orb\n",
			table_name);
		--ret_code;
	}
	return(ret_code);
}
	

/* Initializes the scratch records for each table used in this program.  
That is, origin, origerr, and assoc.  This only needs to be done
once.  The procedure loads NULL files into each attribute of the
scratch records for these tables.  This function is based
on a tricky initialization condition using dbget.  Setting
the db record to dbNULL and then calling dbget(db,0) loads the
scratch record with the contents of the NULL record.  This works
because the NULL record is initialized the first time it is
accessed, and passing 0 to dbget is a shortcut that directly 
copies the NULL record to the scratch record.  IMPORTANT:  note
that because this program uses dbputv directly to the scratch
record the assumption is that the SAME set of attributes are
always written to the scratch record of each table.  This is
correct for this program, but you are warned if you try to use
functions from this program elsewhere.  

Arguments:  db - db pointer of open database 

returns 0 on success.  Any errors will always result in
a return of dbINVALID.  The later should, of course, be
trapped but should probably not normally be fatal.
*/
initialize_scratch_records(Dbptr db)
{
	int return_code;
	int error_count=0;

	db = dblookup(db,0,"origin",0,0);
	db.record = dbNULL;
	return_code = dbget(db,0);
	if(return_code) ++error_count;
	db = dblookup(db,0,"origerr",0,0);
	db.record = dbNULL;
	return_code = dbget(db,0);
	if(return_code) ++error_count;
	db = dblookup(db,0,"assoc",0,0);
	db.record = dbNULL;
	return_code = dbget(db,0);
	if(return_code) ++error_count;
	if(error_count)
		return(dbINVALID);
	else
		return(0);
}
/* This function reads all parameters special to this program.  
It provides a simple way to keep all this together. */

RTlocate_Options parse_rt_options (Pf *pf)
{
	RTlocate_Options o;
	char failure_dirs[STRSZ] ;
	o.work_db=pfget_string(pf,"RT_working_db");
	if(o.work_db  == NULL) o.work_db = strdup("orbgenloc");
	o.logdir=pfget_string(pf,"RT_logfile_directory");
	if(o.logdir == NULL) {
	    o.logdir = strdup(".");
	} else { 
	    makedir ( o.logdir ) ; 
	}
	o.failure_sdir = pfget_string(pf,"RT_failure_subdir");
	strcpy ( failure_dirs, o.logdir ) ; 
	strcat ( failure_dirs, "/" ) ; 
	strcat ( failure_dirs, o.failure_sdir ) ; 
	makedir ( failure_dirs ) ; 
	if(o.failure_sdir == NULL) o.failure_sdir = strdup("failure");
	o.minimum_distance = pfget_double(pf,"RT_minimum_distance");
	o.maximum_distance = pfget_double(pf,"RT_maximum_distance");

	o.db_record_skip_timeout = pfget_int(pf,"RT_db_record_skip_limit");

	return(o);
}
/* This function forms an origin record from the hypocenter structure
defined by h, and transmits it to the orb defined by the variable orb.
It returns an the value of orid it obtained by running dbnextid against
the database pointer db.  

Two other args are passed that are needed to build the origin record,
but are stored in other structures elsewhere in this code:
evid - event id of this event.  In orbgenloc this is assumed to come
	from an associator running upstream of this program.
depth_fixed - when nonzero, code assumes the hypocenter was computed
	with the depth fixed.

Author:  Gary Pavlis
Written:  October 1997
*/

int save_origin(int nass, int evid, Dbptr master_db, Dbptr dbtmp,
			Hypocenter h,Location_options o, int orb)
{
	int ndef;
	char dtype[2];
        char algorithm[16]="ggnloc";
	char auth[16]="orbgenloc";
	int grn, srn;
	int orid;  /* orid returned */

	orid = dbnextid(master_db,"orid");
	if(orid  < 0 )
		elog_die(1,"save_origin:  dbnextid failure asking for new orid\n");
        if(o.fix[2])
        {
                ndef = h.degrees_of_freedom + 3;
                strcpy(dtype,"r");
        }
        else
        {
                ndef = h.degrees_of_freedom + 4;
                strcpy(dtype,"f");
        }
	grn = grnumber(h.lat, h.lon) ;
	srn = srnumber ( grn ) ;
	dbtmp = dblookup(dbtmp,0,"origin",0,0);
	dbtmp.record = dbSCRATCH;
	if((dbputv(dbtmp,0,
                "lat",h.lat,
                "lon",h.lon,
                "depth",h.z,
                "time",h.time,
                "orid",orid,
		"evid",evid,
		"nass",nass,
		"grn", grn,
		"srn", srn,
		"dtype",dtype,
		"ndef",ndef,
		"algorithm",algorithm,
		"auth",auth,0))
			 == dbINVALID)
	{
		elog_complain(0,"dbputv error while building origin record for orid %d\nNo results saved\n",
			orid);
	}
	else
	{
		if(save_dbrecord(dbtmp,orb))
			elog_complain(0,"Errors saving orid %d\n",orid);
	}
	return(orid);
}
	
/*This function adds a single row to the origerr table for this event.

arguements:
	orid - orid assign to this solution
	h - Hypocenter object for this solution
	dbo - output db

This version doesn't set much of this large table.  Eventually, it 
needs to include all the error terms, but that function is not 
written yet. 

Author:  Gary L. Pavlis
Written:  February 1997
*/
void save_origerr(int orid, Hypocenter h, double **C, Dbptr db, int orb)
{
	double sdobs; 
	int rc;
	double smajax,sminax,strike,sdepth,stime;
	double conf=0.683;

	db = dblookup(db,0,"origerr",0,0);
	db.record = dbSCRATCH;

	/* computer error ellipse parameters */
	rc = project_covariance(C,CHI_SQUARE,&conf,
			h.rms_weighted,h.degrees_of_freedom,
			&smajax,&sminax,&strike,&sdepth,&stime);

	/* Bad news here is that we can't save the more useful 
	statistics that this program calculates.  css3.0 only allows
	sdobs = sswr/ndgf */

	sdobs = h.rms_raw;
	if(dbputv(db,0,
		"orid",orid,
                "sxx",C[0][0],
                "syy",C[1][1],
                "szz",C[2][2],
                "stt",C[3][3],
                "sxy",C[0][1],
                "sxz",C[0][2],
                "syz",C[1][2],
                "stx",C[0][3],
                "sty",C[1][3],
                "stz",C[2][3],
		"sdobs",sdobs,
		"smajax",smajax,
		"sminax",sminax,
		"strike",strike,
		"sdepth",sdepth,
		"stime",stime,
		"conf",conf,
			0) == dbINVALID)
	{
		elog_complain(0,"save_origerr: dbputv error writing origerr record for orid %d\norigerr record not saved anywhere\n",
				orid);
	}
	else
	{
		if(save_dbrecord(db,orb))
			elog_complain(0,"Error saving origerr record for orid %d\n",
				orid);
	}

}
/* Do nothing function used to avoid double free in u_tmp arr 
below */
void free_nothing(void *x)
{
}
void save_assoc(Tbl *ta, Tbl *tu, 
	int orid, char *vmodel, Hypocenter hypo, 
	Dbptr db, int orb)
{
        double delta;
        double seaz;
        double esaz;
        double azres;
        double slores;
        Arr *u_arr;
	char key_arid[20];
	Tbl *udregs; 
        int i,n;
 
        double ux, uy, azimuth;
        double duphi;
	Arrival *a;
	Slowness_vector *u;

	/* We build an associative array keyed to arid for
	all the slowness vector measurements. 
	Then in the loop below we can efficiently find any
	slowness vectors associated with the same arid as
	an Arrival.  The overhead in this is significant, but
	it makes it completely general and open ended.  */
	n = maxtbl(tu);
	u_arr = newarr(0);
	for(i=0;i<n;i++)
	{
		Slowness_vector *utmp;
		utmp = (Slowness_vector *)gettbl(tu,i);
		sprintf(key_arid,"%d",utmp->arid);
		setarr(u_arr,key_arid,utmp);
	}
	db = dblookup(db,0,"assoc",0,0);
	db.record = dbSCRATCH;

	n=maxtbl(ta);
	for(i=0;i<n;i++)
	{
		a=(Arrival*)gettbl(ta,i);
		dist(rad(hypo.lat),rad(hypo.lon),
		  rad(a->sta->lat),rad(a->sta->lon),&delta,&esaz);
		dist(rad(a->sta->lat),rad(a->sta->lon),
		  rad(hypo.lat),rad(hypo.lon),&delta,&seaz);
		sprintf(key_arid,"%d",a->arid);
		u = (Slowness_vector *) getarr(u_arr,key_arid);
		if(u == NULL)
		{
		    if(dbputv(db,0,
			"orid",orid,
			"arid",a->arid,
			"sta",a->sta->name,
			"phase",a->phase->name,
			"delta",deg(delta),
			"seaz",deg(seaz),
			"esaz",deg(esaz),
			"timeres",(double)a->res.raw_residual,
			"timedef","d",
			"vmodel",vmodel,
			"wgt",(double)a->res.residual_weight,
		  	0)<0)
		    {
			  elog_complain(0,
			    "Can't add assoc record for station %s arid = %d orid = %d to working db scratch record\nRecord skipped and not saved anywhere\n",
				a->sta->name,a->arid,orid);
			  continue;
		    }
		}
		else
		{
			slores = deg2km(sqrt(sqr(u->xres.raw_residual) 
				+ sqr(u->yres.raw_residual)));
			azimuth = atan2 ( u->uy, u->ux ) ;
			duphi = (u->ux*cos(azimuth) 
				- u->uy*sin(azimuth)) 
				/ sqrt(sqr(u->ux)+ sqr(u->uy)) ;
			azres = deg(duphi);
			if(dbputv(db,"assoc",
				"orid",orid,
				"arid",a->arid,
				"sta",a->sta->name,
				"phase",a->phase->name,
				"delta",deg(delta),
				"seaz",deg(seaz),
				"esaz",deg(esaz),
				"timeres",(double)a->res.raw_residual,
				"timedef","d",
				"vmodel",vmodel,
				"slores",slores,
				"slodef","d",
				"azres",azres,
				"azdef","d",
				"wgt",(double)a->res.residual_weight,
		  	  0)<0)
			{
			  	elog_complain(0,
				  "Can't add assoc record for station %s arid = %d orid = %d to working db scratch record\nRecord skipped and not saved anywhere\n",
				a->sta->name,a->arid,orid);
				delarr(u_arr,key_arid);
				continue;
			}
			/* We delete this entry from u_arr, then we
			can scan below for the dregs easily */
			delarr(u_arr,key_arid);
		}
		if(save_dbrecord(db,orb))
			elog_complain(0,"Error saving assoc record for arid %d\n",
				a->arid);
	}
	/* Since it is possible that slowness vectors can be measured
	with no arrival time, we need to take care of that possibility.
	We do that by checking for dregs in u_arr not removed with
	delarr calls above */
	udregs = keysarr(u_arr);

	n = maxtbl(udregs);
	for(i=0;i<n;i++)
	{
		char *key;
		key = gettbl(udregs,i);
		u = (Slowness_vector *) getarr(u_arr,key);
                dist(rad(hypo.lat),rad(hypo.lon),
                  rad(u->array->lat),rad(u->array->lon),&delta,&esaz);
                dist(rad(u->array->lat),rad(u->array->lon),
                  rad(hypo.lat),rad(hypo.lon),&delta,&seaz);
                slores = deg2km(sqrt(sqr(u->xres.raw_residual)
                          + sqr(u->yres.raw_residual)));
                azimuth = atan2 ( u->uy, u->ux ) ;
                duphi = (u->ux*cos(azimuth)
                          - u->uy*sin(azimuth))
                          / sqrt(sqr(u->ux)+ sqr(u->uy)) ;
                azres = deg(duphi);
		/* The residual weight extraction from the ux component is 
		not ideal here because it could be wrong.  It is unavoidable
		due to polar-cartesian conversion */
		if(dbputv(db,"assoc",
			"orid",orid,
			"arid",u->arid,
			"sta",u->array->name,
			"phase",u->phase->name,
			"delta",deg(delta),
			"seaz",deg(seaz),
			"esaz",deg(esaz),
			"timedef","n",
			"vmodel",vmodel,
			"slores",slores,
			"slodef","d",
			"azres",azres,
			"azdef","d",
			"wgt",(double)u->xres.residual_weight,
	  	  0)<0)
		{
		  	elog_complain(0,"Can't add assoc record for array slowness vector with %s arid = %d and orid = %d to working db scratch record\nNothing saved\n",
			u->array->name,u->arid,orid);
			continue;
		}
		if(save_dbrecord(db,orb))
			elog_complain(0,"Error saving assoc record for arid %d\n",
				u->arid);		

	}
	/* We must not use regular free here, or later we could try
	to free the same area twice.  That is, u_tmp contains keyed
	version of the pointers stored in tu.  This releases only
	the Arr structures, but leaves the pointers to be freed 
	later.  I've never seen a better example of the need for
	a decent garbage collection system. */
	freetbl(udregs,free_nothing);
	freearr(u_arr,free_nothing);
}
char *format_hypo(Hypocenter *h)
{
        char *s;
        s = malloc(512);
        if(s == NULL) elog_die(1,"malloc error for hypocenter output tabulation\n");
        sprintf(s,"%lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %lg %d %d",
                h->lat0,h->lon0,h->z0,h->t0,
                h->lat,h->lon,h->z,h->time,
                h->dx,h->dy,h->dz,
                h->rms_raw, h->rms_weighted, h->interquartile,
                h->number_data,h->degrees_of_freedom);
        return(s);
}
/* This little routine dumps the parameter space of pf and the output
tbls of ggnloc to a log file tagged by orid.  Note, however, that
if ggnloc failed for some reason, it is assumed that orid will be set
to a NEGATIVE number.  When this occurs, the results are written to 
a "failures" directory defined in rtopts.  

IMPORTANT:  multiple instances of orbgenloc should arrange to write
in seperate failure directories or they can collide.  
*/
int write_to_logfile(RTlocate_Options rtopts, int orid, int evid,
	Pf *pf, Tbl *converge_history, Tbl *reason_converged,Tbl *residual)
{
	char fname[256];
	FILE *fp;
	/* We write the logfile output as a pf object copying
	code from sgnloc */
	Pf *pflog;
	Tbl *t;  /* formatted output tbl of hypo convergence history */
	char *line; 
	int i;
	Hypocenter *hypos;

	/* This can fail for very long directory entries, but it
	isn't worth messing with in my mind.  Note switch for 
	orid */
	if(orid > 0)
	{
		sprintf(fname,"%s/orid%d.pf",rtopts.logdir,orid);
	}
	else
	{
		sprintf(fname,"%s/%s/evid%d.pf",rtopts.logdir,
			rtopts.failure_sdir,evid);
	}
	pfwrite(fname,pf);

	/* Here we encapsulate the convergence history information into
	a second pf object, and write this onto the same pf file */
        t = newtbl(maxtbl(converge_history));
        for(i=0;i<maxtbl(converge_history);++i)
        {
                hypos = (Hypocenter *)gettbl(converge_history,i);
                line = format_hypo(hypos);
                pushtbl(t,line);
        }
        pflog = pfnew(PFFILE);
        pfput_tbl(pflog,"convergence_history",t);
  
        printf("Reasons for convergence:\n");
        for(i=0;i<maxtbl(reason_converged);++i)
                printf("%s\n",gettbl(reason_converged,i));
        pfput_tbl(pflog,"convergence_criteria",reason_converged);
        pfput_tbl(pflog,"residuals",residual);
	/* We use pf2string rather than pfwrite since I'm not 
	sure what pfwrite will do when the file already exists. 
	Here we just append the new pf info to the end of the
	stuff written above. */
	if((fp=fopen(fname,"a")) == NULL)
	{
		elog_complain(1,"Cannot open log file %s\n",fname);
		return(1);
	}
	fseek(fp,0L,SEEK_END);
        line = pf2string(pflog);
	fwrite(line,1,strlen(line),fp);
	fclose(fp);
	free(line);
	pffree(pflog);
	freetbl(t,free);
	return(0);	
}

/* This is the main location function for orbgenloc.  
Arguments:

rtopts - structure containing all the special options 
	used by orbgenloc beyond those of other genloc functions.
stations, arrays, phases - associative array pointers for station
	table, array table, and list of phase handles respectively.
	These are generated at startup from an initial parameter
	file using libgenloc functions.
pf - parameter file object.  Data used by orbgenloc are parsed from
	this pf object using standard pf functions.
db - pointer to master database.  The only real use for this db
	is accessing the nextid table to ask for an orid.  All
	database rows created by orbgenloc are send to the orb and
	assumed to be saved later by orb2db.
orbout - the output Object Ring Buffer

Author:  Gary Pavlis
Written:  May 1997, Updated October 1997 to interface with new 
associator.
*/

void compute_location(Location_options o,
	RTlocate_Options rtopts,
	Arr *stations, Arr *arrays, Arr *phases, 
	Pf *pf,
	Dbptr master_db, Dbptr dbtmp, ORB_Hypocenter hyp, int orbout)
{
	Tbl *ta,*tu;  /* Arrival and slowness tables respectively */
	Hypocenter h0;
	int ret_code;
	Tbl *converge_history,*reason_converged,*residual;
	Hypocenter *hypo;
	int niterations;
	char *vmodel;
	int i;
	char *s;
	int orid;
	Point origin;
	double delta, seaz;
	double **C;
	float *emodel;
	int nass;

	initialize_hypocenter(&h0);

	/* It is inefficient to reread these from the parameter
	space on each entry, but preferable to a burdensome
	argument list */  
	origin.lat = pfget_double(pf,"center_latitude");
	origin.lon = pfget_double(pf,"center_longitude");
	origin.z = 0.0;

	/* This routine translates hyp structure to return tbl
	of arrival object pointers */
	ta = orbhypo_to_genloc(&hyp,phases,stations);
	/* this is a pure place holder */
	tu = newtbl(0);

	vmodel = pfget_string(pf,"velocity_model_name");

	/* By default we use the location transmitted by orbassoc.
	This can be overriden in the parameter file by using the
	other options allowed in genloc*/
	s=pfget_string(pf,"initial_location_method");
	h0.lat = hyp.lat;
	h0.lon = hyp.lon;
	h0.z = hyp.depth;
	h0.time = hyp.time;
	/* this strange logic is to allow this parameter to be defaulted.
	If the "initial_location_method" is not defined, or set
	to "manual", we use the location given by orbassoc.  Otherwise
	we utilize genlocs suite of initial locate options. */
	if(s != NULL)
		if(strcmp(s,"manual"))
			h0 = initial_locate(ta, tu, o, pf);

	/* Now compute distance from origin, and process only if 
	the event falls in the specified range */
	dist(rad(origin.lat),rad(origin.lon),rad(h0.lat),rad(h0.lon),
			&delta,&seaz);
	delta = deg(delta);
	/* this is the distance sifting test to ignore things outside
	specified distance range */
	if( ((delta>=rtopts.minimum_distance) 
		&& (delta <= rtopts.maximum_distance)) )
	{

	/* Location with Generic Gauss_Newton code */
		orid = -1;
		nass = maxtbl(ta);
		ret_code = ggnloc(h0,ta,tu,o,
				&converge_history,&reason_converged,&residual);
        	if(ret_code < 0)
        	{
                	elog_notify (0,"ggnloc failed to produce a solution for evid %d\n",hyp.evid);
        	}
        	else
		{
			if(ret_code > 0)
				elog_notify(0,"Warning:  %d travel time calculator failures in ggnloc\nSolution ok for evid %d\n",
                                	ret_code,hyp.evid);
			C = dmatrix(0,3,0,3);
			emodel = (float *) calloc(4,sizeof(float));
			if((emodel == NULL) || (*C == NULL) )
				elog_die(0,"Malloc error for error arrays\n");
			niterations = maxtbl(converge_history);
                	hypo = (Hypocenter *)gettbl(converge_history,
							niterations-1);
			predicted_errors(*hypo, ta, tu, o, C, emodel);
               		orid = save_origin(nass,hyp.evid,master_db,
					 dbtmp,*hypo,o,orbout);

                	save_origerr(orid,*hypo,C,dbtmp,orbout);
			save_assoc(ta, tu, orid, vmodel, 
				*hypo, dbtmp,orbout);
			elog_notify(0,"orid %d converged in %d iterations\n",
				orid,niterations);
			elog_notify(0,"Reason(s) for convergence:  \n");
			for(i=0;i<maxtbl(reason_converged);++i)
                        	elog_notify(0,"%s",gettbl(reason_converged,i));
			elog_notify(0,"\n");
			s=format_hypo(hypo);
			elog_notify(0,"%s\n",s);
			free(emodel);
			free_matrix((char **)C,0,3,0);
			free(s);
		}
		write_to_logfile(rtopts, orid, hyp.evid,
			  pf, converge_history, reason_converged,residual);

		if(maxtbl(converge_history)>0)freetbl(converge_history,free);
		if(maxtbl(reason_converged)>0)freetbl(reason_converged,free);
		if(maxtbl(residual)>0)freetbl(residual,free);

	}
	destroy_data_tables(ta, tu);
	return;
}
#define DEFAULT_STATEFILE "state/orbgenloc"
void exhume_state(int ecode)
{
	switch (ecode)
	{
	case (-1):
		elog_complain(0,"Cannot open state file for writing\n");
		break;
	case (0):
		elog_complain(0,"State file not found, new one will be created\n");
		break;
	case (1):
		elog_complain(0,"Found old state file\n");
		break;
	case (2):
		elog_complain(0,"Old state file found, but it was corrupted\n");
		break;
	default:
		elog_complain(0,"Unrecognized error return by exhume\n");
	}
	return;
}

int main (int argc, char **argv)
{
	Dbptr master_db, dbtmp;
	char dbname[512];  /* dbtmp name assigned by maketmpdb */
	char *orbname;
	char *pffile=NULL;

	Pf *pf;  /* Input pf object handle */
	Arr *arr_sta;
	Arr *arr_a;  /*Array object associative array -- purely a place holder*/
	Arr *arr_phase;
	int i;
	char *statefile=NULL;
	Point origin;

	int orbin,orbout;  /* We establish both a read and write connection
				on seperate sockets so we can use orbreap on
				the input */
	int quit=0,last_pktid;
	double last_pkttime;
	int exhume_rcode;  /* value returned by exhume*/
	char *packet=0;
	int orid_used;
	Location_options o;
	RTlocate_Options rt_opts;
	ORB_Hypocenter hyp;

	
	/* This initialization is necessary for the orb_arrivals_in routine
	to work correctly on the first pass*/
	hyp.assocs = NULL;

	elog_init(argc, argv);
	elog_notify (0, "$Revision$ $Date$") ;
	if(argc < 2) usage(argv[0]);
	orbname = argv[1];

	for(i=2;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			pffile = argv[i];
		}
		else if(!strcmp(argv[i],"-S"))
		{
			++i;
			statefile = argv[i];
		}
		else
		{
/* For this kind of program it seems wise to make it a fatal error to 
have the arguments botched */
			elog_complain(0,"Unrecognized argument %s\n",argv[i]);
			usage(argv[0]);
		}
	}
        /* set default this way*/
        if(pffile == NULL) pffile = strdup(DEFAULT_PFFILE);
	if(statefile == NULL) statefile = strdup(DEFAULT_STATEFILE);

	/* parse parameter file and form all the genloc control and
	internal static data structures */
	i = pfread(pffile,&pf);
	if(i != 0) elog_die(1,"Pfread error\n");
	o = parse_options_pf (pf);
	arr_sta = load_station_table(pf);
	arr_a = load_array_table(pf);
 	arr_phase = parse_phase_parameter_file(pf);
	/* Note this is a slightly different use of these variables
	than that used by other genloc routines.  Here we use it
	like a coordinate system origin to select range of distances
	to use. We actually reset these again in the location function,
	but check them here to make sure these variables are in the
	parameter space.  pfget_double will cause the program to die
	if these aren't defined.*/
	origin.lat = pfget_double(pf,"center_latitude");
	origin.lon = pfget_double(pf,"center_longitude");
	origin.z = 0.0;

	rt_opts = parse_rt_options(pf);

	if(dbopen(rt_opts.work_db,"r+",&master_db ) == dbINVALID)
                elog_die(1,"Unable to open master database %s\n",
			rt_opts.work_db);


	/* Now we open the orb server connections */
	if( (orbin=orbopen(orbname,"r&")) < 0)
		elog_die(0,"Cannot open ring buffer %s for reading\n",orbname);
	
	if(orbselect(orbin,"/db/event|/db/origin|/db/assoc|/db/arrival") < 0)
		elog_die(0,"Cannot select any db records from ring buffer %s\n",
			orbname);

	/* These are the state saving routines.  quit is set nonzero 
	whenever the program catches a signal.  We call bury below when
	this happens.  exhume_state is a function because I expect
	it could be used again  */

	exhume_rcode = exhume ( statefile, 0, 0, 0 );
	exhume_state(exhume_rcode);  
        if ( orbresurrect ( orbin, &last_pktid, &last_pkttime ) == 0 )
		elog_complain( 0, "resurrection successful: repositioned to pktid #%d\n", last_pktid ) ;
        else
	{
		orbseek (orbin, ORBOLDEST);
		last_pktid = orbtell(orbin);
		elog_complain( 0, "resurrection unsuccessful\nStarting at beginning of current orb at packet id %d\n",last_pktid ) ;
	}
	/* The following is basically a trick to create a db pointer that
	never references any tables.  This is the preferred approach for
	orbpkt2db records which utilize the scratch record of this database
	pointer.  The fact that we destroy the file this creates turns
	out to be a feature of datascope we can exploit here.  */
	if (maketmpdb ("css3.0", &dbtmp, dbname) < 0) {
		elog_complain(0, "maketmpdb() error.\n");
		exit (1);
	}
	/* This little routine initilizes the null record for each table 
	used here.  This was necessary because we assemble records in the
	scratch record.  This sets proper nulls in fields that are not
	referenced by this program. */
	if(initialize_scratch_records(dbtmp) == dbINVALID)
		elog_complain(0,"Warning:  errors initializing null records in tables.  May generate invalid data in some fields\n");
/*
	unlink (dbname);
*/
	

	if( (orbout=orbopen(orbname,"w&")) < 0)
		elog_die(0,"Cannot open ring buffer %s for writing\n",orbname);

	/* This loop is broken only by an error.  We call bury after each 
	event is processed saving the current packet id.  This should 
	effectively skip events that cause orbgenloc to die for some
	reason. */
	while(1) 
	{
		int return_code;

		return_code = orb_arrivals_in(orbin, dbtmp, &hyp, 
			&last_pktid,rt_opts);
		if(return_code)
		{
		    if(return_code < 0)
			elog_complain(0,"Error reading db records from orb\nCurrent event skipped\n");
		    else
			elog_complain(0,"Sequencing error reading db packets from orbassoc.\nOne or more events were probably skipped\n");
		    continue;
		}
		if(bury())
			elog_complain(0,
			  "bury failed writing statefile %s\n",statefile);

		compute_location(o,rt_opts,arr_sta,arr_a,arr_phase,
				pf,master_db, dbtmp, hyp, orbout);

		/* when last_pktid is -1 orb_arrivals_in does not do 
		an orbseek, so we always reset it here */
		last_pktid = -1;
		/* This is the only appropriate place to release
		this space.  This block is malloced in orb_arrivals_in*/
		free(hyp.assocs);
		hyp.assocs = NULL;
	}
}
	
