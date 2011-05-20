#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <perf.h>
#include "elog.h"
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "db.h"
#include "coords.h"
#include "brttutil.h"
#include "location.h"
#include "pmel.h"
#include "pfstream.h"
#include "pmelgrid.h"

#ifdef MPI_SET
        #include <mpi.h>
#endif

/* small companion function to main.  It basically dumps the contents of
* the parameter space, pf, to a special database table.  
* It tries to extract a name for the output file from the input pf, but
* if that file exists or the parameter is not found in the database a
* diagnostic is issue and a unique name is generated from the process id.
*/

void save_run_parameters(Dbptr db,Pf *pf)
{
	char *dir,*dfile;
	char filename[512];
	char *vm,*vm3d;
	int ierr;
	
	dir = pfget_string(pf,"pmelrun_archive_directory");
	if(dir==NULL)elog_log(0,"Parameter pmelrun_archive_directory not in parameter file\nUsing default of current directory");
	dir=strdup(".");
	if(makedir(dir))
		elog_die(0,"makedir failed on directory %s\n",dir);
	dfile = pfget_string(pf,"pmel_run_name");
	if(dfile==NULL)
	{
		dfile=(char *)malloc(20);
		sprintf(dfile,"%dpmel",getpid());
		elog_log(0,"Missing parameter:  pmel_run_name\nControl pf will be saved to %s\n",
				dfile);
		pfput_string(pf,"pmel_run_name",dfile);
	}

	vm = pfget_string(pf,"travel_time_model");
	vm3d=pfget_string(pf,"3Dreference_model");
	if( (vm==NULL) || (vm3d==NULL) )
		elog_die(0,"Missing required velocity model definitions\nCheck parameters travel_time model and 3Dreference_model\n");
	db = dblookup(db,0,"pmelruns",0,0);
	ierr=dbaddv(db,0,"pmelrun",dfile,
		"vmodel",vm,
		"vmodel3d",vm3d,
		"dir",dir,
		"dfile",dfile,0);
	if(ierr<0) 
	{
		elog_complain(0,
		   "dbaddv error on pmelrun table using pmelrun=%s\nTrying to generate unique name\n",
		     dfile);
		dfile=(char *)malloc(20);
		sprintf(dfile,"%dpmel",getpid());
		elog_notify(0,"Trying again with pmelrun=%s\n",dfile);
		ierr=dbaddv(db,0,"pmelrun",dfile,
	               "vmodel",vm,
	                "vmodel3d",vm3d,
	                "dir",dir,
	                "dfile",dfile,0);
		if(ierr<0)elog_die(0,"Attempt to append to pmelruns table with pmelruns=%s failed\nCheck database\n",
				dfile);
		pfput_string(pf,"pmel_run_name",dfile);
	}

	strcpy(filename,dir);
	strcat(filename,"/");
	strcat(filename,dfile);
	if(pfwrite(filename,pf))
		elog_die(0,"pfwrite error for file %s\n",filename);
	free(dir);  free(dfile);
	free(vm); free(vm3d);
}
/* Small function to extract the hypocentroid information from this 
Pfensemble.  Assumption is that all members of the ensemble share
the same hypocentroid so the algorithm simply extracts the information
from the first member of the Pf array of the input Pf_ensemble 

Author :  Gary Pavlis
Written:  October 2002
*/
Hypocenter pfget_hypocentroid(Pf_ensemble *pfe)
{
	Hypocenter h;
	initialize_hypocenter(&h);
	h.lat=pfget_double(pfe->pf[0],"hypocentroid.lat");
	h.lon=pfget_double(pfe->pf[0],"hypocentroid.lon");
	h.z=pfget_double(pfe->pf[0],"hypocentroid.z");
	return(h);
}
/* This is a comparable function that returns a vector of Hypocenters
of initial location estimates from a Pf_ensemble.  That is, the
above gets one Hypocenter that defines the single hypocentroid
for the ensemble while this function returns a vector of all the
events that define that ensemble.  

Note:  This array is parallel with the array of Tbl's that contain
the Arrival objects.

Author:  Gary Pavlis
Written:  October 2002
*/
Hypocenter *pfget_hypocenters(Pf_ensemble *pfe)
{
	Hypocenter *h;
	int i;

	allot(Hypocenter *,h,pfe->ngroups);
	for(i=0;i<pfe->ngroups;++i)
	{
		/* All members of a group should have the same
		hypocenter information so we just grab the first */
		initialize_hypocenter(h+i);
		h[i].lat = pfget_double(pfe->pf[pfe->group_start[i]],
				"origin.lat");
		h[i].lon = pfget_double(pfe->pf[pfe->group_start[i]],
				"origin.lon");
		h[i].z = pfget_double(pfe->pf[pfe->group_start[i]],
				"origin.z");
		h[i].time = pfget_time(pfe->pf[pfe->group_start[i]],
				"origin.time");	
	}
	return(h);
}	
	
void usage()
{
	cbanner("1.0","db pfistream pfostream [-pf file -V]\n",
			"Gary L. Pavlis",
			"Indiana University",
			"pavlis@indiana.edu");
	elog_die(0,"\n");
}
Pfstream_handle *pfshi, *pfsho;
int main(int argc, char **argv)
{
	char *dbin;  /* Input db name */
	Dbptr db;  /* input db pointer */
	char *pfinfl=NULL;  /* input parameter file */
	Pf *pf,*vpf;
	Pf *pfi,*pfo,*pfehead;
	Pf_ensemble *pfe,*pfesc;
	Pf *pfsc;
	int i;
        /* Holds indexed list of stations with bad clocks problems 
        that should be used only with S-P type timing */
        Arr *badclocks;
	/*libgenloc large control object for single event location algorithm*/
        Location_options o;
        /* Hold station table.   We make the array table empty always. */
        Arr *stations;
        Arr *arr_phase;
        char *vmodel;
        Arr *arr_phase_3D;
        Tbl *phaselist;
        /* This is the structure that encapsulates all the station
        correction elements.  WARNING: it is built up in pieces below,
        and later modifications must handle this carefully */
        SCMatrix *smatrix;
        /* The associative array here is loaded from the pf and contains
        information on which events are to be treated as calibration events.
        The fixlist array of character strings is passed to pmel as a 
        parallel of vectors defining events with fixed coordinates */
        Arr *events_to_fix;
	int nbcs;
	FILE *fpo;
	char *runname;
	/* These are used for pmtfifo implementation.  They are analagous
	to a FILE * abstraction. */
	char *streamin, *streamout;
	
#ifdef MPI_SET

/*
 *         Initialize the MPI parallel environment, MPI_Init
 *         must be called before any other MPI call.
 *               rank -- rank of a specific process
 *               mp -- the total number of processes. This whole
 *                  process group is called MPI_COMM_WORLD
 *                                                 */
        int rank, np;

        MPI_Init(&argc, &argv);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &np);
#endif
	elog_init (argc, argv) ;

	if(argc<4) usage();
	dbin=argv[1];
	streamin=argv[2];
	streamout=argv[3];
	/* launch read and write threads */
#ifdef MPI_SET
        MPI_Barrier(MPI_COMM_WORLD);
	/* Only process 0 should launch the read and write thread in mp mode */
	if(rank==0) pfshi = pfstream_start_read_thread(streamin);
	if(rank==0) pfsho = pfstream_start_write_thread(streamout);
#else
	pfshi = pfstream_start_read_thread(streamin);
	pfsho = pfstream_start_write_thread(streamout);
#endif
	if(pfshi==NULL) elog_die(1,"Read thread  %s create failed\n",argv[2]);
	if(pfsho==NULL) elog_die(1,"Write thread %s create failed\n",argv[2]);

	for(i=4;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			if(i>argc) usage();
			pfinfl=argv[i];
		}
		else if(!strcmp(argv[i],"-V"))
			usage();
		else
			usage();
	}
	/* this set's default pf  name */
	if(pfinfl==NULL) pfinfl=(char *)strdup("pmelgrid");
	if(pfread(pfinfl,&pf)) elog_die(1,"pfread error from %s\n",pfinfl);
	o = parse_options_pf(pf);
	/* Pmel is known to fail if this option is turned on */
	if(((o.generalized_inverse)==PSEUDO_RECENTERED)
		|| ((o.generalized_inverse)==DAMPED_RECENTERED) )
	{
		elog_notify(0,"parameter file specifies recenter\
option which is know to cause problems\nrecenter set off\n");
		if((o.generalized_inverse)==PSEUDO_RECENTERED)
			o.generalized_inverse = PSEUDOINVERSE;
		if((o.generalized_inverse)==DAMPED_RECENTERED)
			o.generalized_inverse = DAMPED_INVERSE;
	}


	/* This uses the same method of defining phase handles as dbgenloc*/
	vmodel=pfget_string(pf,"travel_time_model");
	if(pfload("GENLOC_MODELS", "tables/genloc", vmodel, &vpf) != 0)
		elog_die(0,"pfload failed reading working velocity model %s\n",
			vmodel);
 	arr_phase = parse_phase_parameter_file(vpf);
	pffree(vpf);

	/* We want to explicitly clear the station corrections in the 
	phase handle list if any were set by the above function.  If not,
	we have a real mess with corrections in corrections.  */
	clear_station_corrections(arr_phase);

	/* Now load the model used to compute bias correction with 
	a (presumed) 3D model.  Not really required 3D, but we call it that
	anyway.*/
	arr_phase_3D = parse_3D_phase(pf);

	/* We have edit the array of phase handles to a list we select. 
	This way the user doesn't have to be concerned that the arr_phase and
	arr_phase_3d lists are one to one. */
	phaselist = pfget_tbl(pf,"phases_to_use");
	edit_phase_handle(arr_phase,phaselist);
	edit_phase_handle(arr_phase_3D,phaselist);

	/* We need the database for some constant metadata not passed directly 
	 * through the pfstream.  Consequently we open this immediately and 
	 * extract what we need here */
	if(dbopen(dbin,"r+",&db)==dbINVALID) 
		elog_die(0,"Unable to open input database %s\n",dbin);

	/* load the station table and use it to create the station
	indexes for the smatrix structure and setup the result
	vectors stored there.*/
	stations = pmel_dbload_stations(db,pf);
	smatrix = create_SCMatrix(stations,arr_phase);

	/* These routines set up definitions of stations to use 
	S-P type phases with due to "bad clocks".   We use this
	here because we want to either throw away data from stations
	with bad clocks or use the S-P feature.  This sets up
	that feature, but lets the decision of how to handle
	this problem be set in pf.*/
	badclocks = newarr(0);
	if(db_badclock_definition(db,pf,badclocks))
		elog_notify(0,"Problems in database definitions of bad clocks time periods\n");
	pfget_badclocks(pf,badclocks);
	/* nbcs is used as a boolean below, but the cntarr sets it 
	to a nonzero value if there are any entries in the badclock 
	array.  This is useful for efficiency to bypass the messy
	S-P code unless it is necessary. */
	nbcs = cntarr(badclocks);

	events_to_fix = load_calibration_events(pf);
#ifdef MPI_SET
	/* Only process 0 should save the state of the program when running mp mode */
	if(rank==0)save_run_parameters(db,pf);
#else
	/* don't need the above for single processor case */
	save_run_parameters(db,pf);
#endif
	runname = pfget_string(pf,"pmel_run_name");

	while((pfi=pfstream_get_next_ensemble(pfshi))!=NULL)
	{
		int nevents;
		Tbl **ta;
		Hypocenter hypocentroid;
		Hypocenter *h0;
		int *evid;
		int gridid;
		int ierr;
		int k;
		int ndata;
		Tbl *converge=NULL,*pmelhistory=NULL;
		int pmelfail;

		pfe=pfget_Pf_ensemble(pfi,"pmel_arrivals");
		if(pfe==NULL)
		{
			elog_complain(0,"Failure in trying to crack Pf_ensemble in input stream %s\nTrying to read another block\n",
				argv[2]);
			continue;
		}
		/* no longer needed as we copied it to create pfe*/
		pffree(pfi);
		/* Get the global parameters for this ensemble from
		the first entry of the vector of pf's in pfe */
		gridid = pfget_int(pfe->pf[0],"gridid");
		/* Get the hypocentroid for this ensemble */
		hypocentroid=pfget_hypocentroid(pfe);
		/* now get initial hypocenters for each event */
		h0=pfget_hypocenters(pfe);
		/* now create a parallel array of Tbl's to h0 
		that contain lists of Arrival objects.  At the
		same time we need a vector of event ids stored in
		the evid vector.*/
		nevents=pfe->ngroups;
		allot(Tbl **,ta,nevents);
		allot(int *,evid,nevents);
		for(i=0,ndata=0;i<nevents;++i)
		{
			ta[i]=pfextract_arrivals(pfe->pf,
					pfe->group_start[i],
					pfe->group_end[i],
					arr_phase,
					stations);
			evid[i]=pfget_int(
				pfe->pf[pfe->group_start[i]],"evid");
			ndata += maxtbl(ta[i]);
		}
		fprintf(stdout,"Working on gridid=%d\nEnsemble has %d arrivals\n",
			gridid,ndata);
		/* This function alters the phase handles by setting
		the station corrections to those computed from the
		difference in arrival time between the earth model
		defined by arr_phase_3D and arr_phase. This is how
		the current station correction information get's 
		moved through this program.  This is a bit obscure and
		potentially error prone for changes in the front end.*/
		ierr = initialize_station_corrections(arr_phase,
			arr_phase_3D,stations,&hypocentroid);
		if(ierr>0)
		{
			elog_notify(1,"%d problems setting path anomaly corrections for grid id %d\n",
				ierr,gridid);
		}
		else if(ierr<0)
		{
			elog_complain(0,"Cannot compute any path anomaly corrections for grid id %d\nData for this grid point skipped\n",
				gridid);
			for(k=0;k<nevents;++k) freetbl(ta[k],free);
			free(ta);
			free(evid);
			free(h0);
			continue;
		}
		/* WE make the S work space of size ndata times 
		the number of parameters (smatrix->ncol).  This 
		simplifies handling of fixed coordinates, which
		wastes minimal memory.  That is, we could use 
		as few rows as ndata-4*nevents, but this any
		fixed coordinates would lead to an overflow.  It is
		easier to just make sure there is enough space since
		this is a memory pig algorithm anyway.  To really save
		space would require the sequential accumulation routine
		used in the original pmel paper.*/
		reallot(double *,smatrix->S,ndata*(smatrix->ncol));
		smatrix->nrow=ndata;
		for(k=0;k<(ndata*(smatrix->ncol));++k)smatrix->S[k]=0.0;

		/* This computes the set of reference station corrections
		for this group of events */
		ierr = compute_scref(smatrix, &hypocentroid,stations,
			arr_phase,arr_phase_3D);
		if(ierr)
		{
			elog_notify(0,"%d errors in compute_scref\n",ierr);
		}
		/* It is necessary to initialize the sc vector in smatrix
		to the contents of the reference station corrections to make
		the results internally consistent.  pmel internally uses the
		station corrections in the phase handles in computing travel 
		times and sc is used to as the work vector to store the
		current estimates.  The reference corrections are always the
		starting solution and this copy is required to keep the
		solution consistent with this fact */
		dcopy(smatrix->ncol,smatrix->scref,1,smatrix->sc,1);
		

		/* This is the main processing routine.  It was 
		intentionally built without any db hooks to make it
		more portable */
		if(pmel(nevents,evid,ta,h0,
			events_to_fix,&hypocentroid,smatrix,
			arr_phase,&o,pf,&converge,&pmelhistory))
		{
			elog_notify(0,
			  "No solution from pmel for cluster id = %d\n",
				gridid);
			continue;
		}
		fprintf(stdout,"Cluster id=%d pmel convergence reason\n",
			gridid);
		for(k=0,pmelfail=0;k<maxtbl(converge);++k)
		{
			char *swork;
			swork = (char *)gettbl(converge,k);
			fprintf(stdout,"%s\n",swork);

			/* The string ABORT in the convergence list
			is used to flag a failure. */
			if(strstr(swork,"ABORT")!=NULL) pmelfail=1;
		}
		freetbl(converge,free);
		converge=NULL;
		/* This list contains history of the hypocentroid.
		Currently it is just dropped, but it could be printed
		in a verbose mode.  This option probably should
		be done eventually.*/
		freetbl(pmelhistory,free);
		pmelhistory=NULL;
		/* This inserts revised attributes into the pfe */

		update_ensemble(pfe,pf,h0,ta,o);
		/* We have to recreate the header block for pfe */
		pfehead=pfensemble_convert_group(pfe);
		/* This builds a separate structure for pmel extension
		tables that are stored in a different block */
		pfesc = build_sc_ensemble(gridid,smatrix,pf);
		/* These are added per ensemble */
		pfensemble_put_string(pfesc,"pmelrun",runname);
		pfensemble_put_double(pfesc,"sswrodgf",smatrix->sswrodgf);
		pfensemble_put_int(pfesc,"ndgf",smatrix->ndgf);
		pfensemble_put_int(pfesc,"gridid",gridid);
		pfensemble_put_double(pfesc,"sdobs",smatrix->rmsraw);
		pfo=build_ensemble(2,"pmel_arrivals",pfehead,pfe,
				"station_corrections",NULL,pfesc);
		free_Pf_ensemble(pfe);
		pffree(pfehead);
		free_Pf_ensemble(pfesc);
		if(pfstream_put_ensemble(pfsho,pfo)!=0)
			elog_complain(1,"pfstream_put_ensemble failed\n");	

		/* memory cleanup */
		for(i=0;i<nevents;++i)
		{
			freetbl(ta[i],free);
		}
		free(ta);
		free(h0);
		free(evid);
	}

#ifdef MPI_SET
	/* Terminate the MPI parallel environment and clean up. 
	 * This function must be called or chaos will follow */
	MPI_Finalize();
#endif
	/* Wait for the output queue to empty before exiting or 
	data at the end of the stream will be truncated */
	while(pmtfifo_data_available(pfsho->mtf))
	{
		fprintf(stderr,"Waiting for queue to exit: sleeping for 10\n");
		sleep(10);
	}
	sleep(30);
	destroy_SCMatrix(smatrix);
/*
	pthread_exit(NULL);
*/
	return(0);
}
