/* Data format converter for binary seismic data.  This program is sort
of an inverse to segy2css.  It maps everything it reasonably can onto 
the segy header.  There are drastic differences between what segy was
written for and the css3.0 schema, so there are strong limitations
in this.  The program works around a parameter file in which there
are two critical components:
1.  The "channels" tbl defines the output order of channels.  The program
builds an arr keyed to sta_chan that is used to build the output 
multichannel volume.  This part of the parameter file will define
the order of channels in the output segy volume.

2.  There are a series of optional rotation parameters that define
a general rotation mechanism.  Basically, the program can optional
transform all it's data an arbitrary set of orthonormal coordinate
axes.   The channel list has to reflect this case by using channel
codes R, Z, and T.  

The program read a set of start times from stdin that are used to 
define different events that are to be written to the segy output 
volume.  segy REQUIRES fixed length records and fixed sample rates.
The record length is defined in the parameter file.  The sample rate
needs to be defined in the parameter file also.  Any deviations from
that sample rate will cause the program to die.  

Author:  Gary L. Pavlis
Written:  November 1998 in a hack form.  This version is a descendent
that recycled some of the original code written in February 1999.

*/
#include <stdlib.h>
#include <stdio.h>
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "tr.h"
#include "pf.h"
#include "elog.h"
#include "segy.h"
#include "location.h"
#define min(a,b) ((a) <= (b) ? (a) : (b))
/* Newer compilers will complain if these prototypes are not defined.  
Could be placed in segy.y, but that would strike me as mysterious. */

void initialize_header(SegyHead *header);
char *make_key(char *s, char *c);
Arr *build_stachan_list(Pf *pf, int *nchan,int verbose);
int get_channel_index(Arr *a, char *sta, char *chan);
Arr *check_tables(Dbptr db, Pf *pf);
void check_for_required_tables(Arr *tabarray);
Dbptr join_tables(Dbptr db, Pf *pf, Arr *tables);
void set_shot_variable(Dbptr db, Arr *tables, int evid, SegyHead *h);
void repair_gaps(Dbptr trdb);

static void
usage()
{
	fprintf(stderr,"Usage:  db2segy dbin outfile [-pf pffile -SU -ss subset -v]\n");
	exit(-1);
}

void
initialize_header(SegyHead *header)
{
	header->lineSeq = 1;
	header->event_number = 1;
	header->channel_number = 1;
	header->energySourcePt = 1;
	header->cdpEns = 0;
	header->traceInEnsemble = 0;
	header->traceID = 2;  /* note default is dead */
	header->vertSum = 0;
	header->dataUse = 0;
	header->sourceToRecDist = 0;
	header->recElevation = 0;
	header->sourceSurfaceElevation = 0;
	header->sourceDepth = 0;
	header->datumElevRec = 0;
	header->recWaterDepth = 0;
	header->elevationScale = 1;
	header->coordScale = 1;
	header->sourceLongOrX = 0;
	header->recLongOrX = 0;
	header->coordUnits = 1;  /* This sets units to m */
	header->weatheringVelocity = 0;
	header->subWeatheringVelocity = 0;
	header->sourceUpholeTime = 0;
	header->sourceStaticCor = 0;
	header->totalStatic = 0;
	header->lagTimeA = 0;
	header->delay = 0;
	header->muteStart = 0;
	header->sampleLength = 0;
	header->deltaSample = 0;
	header->gainType = 0;
	header->gainConst = 0;
	header->correlated = 0;
	header->sweepStart = 0;
	header->sweepLength = 0;
	header->sweepType = 0;
	header->sweepTaperAtStart = 0;
	header->taperType = 0;
	header->aliasFreq = 0;
	header->notchFreq = 0;
	header->lowCutFreq = 0;
	header->lowCutSlope = 0;
	header->year = 0;
	header->timeBasisCode = 2;  /* this means gmt time */
	header->traceWeightingFactor = 0;
	header->phoneRollPos1 = 0;
	header->gapSize = 0;
	header->taperOvertravel = 0;
	header->extrash[10] = 0;
	header->samp_rate = 0;
	/* Always ieee floats in this program for now */
	header->data_form = 5;
	header->trigyear = 0 ;
	header->trigday=0;
	header->trighour=0;
	header->trigminute=0;
	header->trigsecond=0;
	header->trigmills = 0;
	header->scale_fac = 0;
	header->inst_no = 0;
	header->not_to_be_used = 0;
	header->num_samps = 0;
	header->extra[8] = 0;
	header->reelSeq=1;
	header->horSum=0;
	header->datumElemSource=0;
	header->sourceWaterDepth=0;
	header->sourceLatOrY = 0;
	header->recLatOrY=0;
	header->recUpholeTime=0;
	header->recStaticCor=0;
	header->lagTimeB = 0 ;
	header->muteEnd=0;
	header->sweepTaperAtEnd=0;
	header->aliasSlope=0;
	header->notchSlope=0;
	header->hiCutFreq=0;
	header->hiCutSlope=0;
	header->day=0;
	header->hour=0;
	header->minute=0;
	header->second=0;
 	header->phoneFirstTrace=0;
	header->phoneLastTrace=0;
	header->m_secs=0;
	header->initialGain=0;
	header->sweepEnd=0;
}
/* companion to below to build associative array key. 
Key is sta_chan with s=station and c-channel code */
char *make_key(char *s, char *c)
{
        char tkey[40];
        tkey[0]='\0';
        strcat(tkey,s);
        strcat(tkey,"_");
        strcat(tkey,c);
        return(strdup(tkey));
}

/* This small function builds a keyed list of channel codes 
from a list in a parameter file and returns this as an 
associative array "a".  The function returns the number of
channels actually found in the list.*/

Arr *build_stachan_list(Pf *pf, int *nchan,int verbose)
{
	char sta[10], chan[10];
	char *key;
	Arr *a;
	int i;
	Tbl *t;
	char *line;
	int *channel_number;  /* value stored in arr */

	if(verbose)
		fprintf(stdout,"Station   Channel_code    Channel_number\n");
	a = newarr(0);
	t = pfget_tbl(pf,"channels");
	if(t==NULL) elog_die(0,"Parameter file error:  no channels table\n");
	for(i=0;i<maxtbl(t);++i)
	{
		line = gettbl(t,i);
		sscanf(line,"%s %s",sta,chan);
		key = make_key(sta,chan);
		channel_number = (int *) malloc(sizeof(int));
		if(channel_number == NULL) 
			elog_die(0,"malloc error for channel_number\n");
		*channel_number = i;
		setarr(a,key,(void *)channel_number);
		if(verbose)
			fprintf(stdout,"%s  %s  %d\n",sta,chan,(*channel_number)+1);
		free(key);
	}
	*nchan = maxtbl(t);
	freetbl(t,free);
	return(a);
}
/* This is the routine that uses the Arr produced by build_stachan_list.
It returns and integer index of the station/channel defined by sta,chan.
If that station and channel are not found in the Arr, it returns -1.
*/
int get_channel_index(Arr *a, char *sta, char *chan)
{
	char *key;
	int *ichan;
	key = make_key(sta,chan);
	ichan = (int *)getarr(a,key);
	if(ichan == NULL) 
		return(-1);
	else
		return(*ichan);
}

	
		
/* This routine looks for a Tbl in the parameter space that 
defines a set of auxiliary tables that are to be used.  It 
cautiously checks to see if that table is defined in the 
schema and is nonempty.  It sets logical variables in associative
array it returns that define if the table is "ok".  This is serious
overkill added to make the code for expandable in the future. 
At the moment the only table that would be used is "shot".
It would, however, be easy to add similar auxiliary tables for
segy constructs like statics, mute definition, etc.  In that
case there probably should be a more general mechanism than this
but I'm more or less laying out a useful functionality here rather
than doing it in a completely general way.  
*/
Arr *check_tables(Dbptr db, Pf *pf)
{
	char *table;
	int *ilogic;
	int i;
	Tbl *t;
	Dbptr dbtmp;
	int table_ok;
	long int nrec;
	Arr *a;

	a = newarr(0);
	t = pfget_tbl(pf,"join_tables");
	if(t == NULL)
	{
		elog_die(0,"No list of tables to be joined\n");
	}
	for(i=0;i<maxtbl(t);++i)
	{
		/* This series of conditionals is safe:  It certifies
		a table (a) is defined in this schema and (b) is
		not empty.  */
		table = (char *)gettbl(t,i);
		dbtmp = dblookup(db,0,table,0,0);
		if(dbtmp.table == dbINVALID) 
			table_ok = 0;
		else
			table_ok = 1;
		if(table_ok)
		{
			dbquery(dbtmp,dbRECORD_COUNT,&nrec);
			if(nrec > 0) 
				table_ok = 1;
			else
				table_ok = 0;
		}
		ilogic = (int *) malloc(sizeof(int));
		if(ilogic == NULL) elog_die(0,"malloc error\n");
		*ilogic = table_ok;
		setarr(a,table,ilogic);
	}
	return(a);
}
/* trap needed because wfdisc and site are required tables. */
void check_for_required_tables(Arr *tabarray)
{
	int *test;
	int need_to_die=0;
	test = (int *)getarr(tabarray,"site");
	if( (test == NULL) || ((*test) == 0) )
	{
		elog_complain(0,"Cannot find required table site in db\n");
		++need_to_die;
	}
	test = (int *)getarr(tabarray,"wfdisc");
	if( (test == NULL) || ((*test) == 0) )
	{
		elog_complain(0,"Cannot find required table wfdisc in db\n");
		++need_to_die;
	}
	if(need_to_die) elog_die(0,"Cannot proceed without required tables\n");
}

/* This routine cautiously joins tables defined by "join_tables"
list in the input parameter spaced (same entity used by check_tables
above.)  The program attempts to join the tables in the order listed.
It does this cautiously in two ways.  First, it checks the associative
array defined  by check_tables and skipping any one that is not
defined as "ok".  Second, if the joined table at any time is found
to have zero length, the program will set db.record to dbINVALID
and return.  It normally returns a db pointers to the final joined
table.

Note this function is somewhat less general than it could be.
With a little work one could have the parameter file specify
a set of join keys and (optionally) even use a theta join.  
I took the easy way out and decided to only use the natural
join mechanism.  
*/

Dbptr join_tables(Dbptr db, Pf *pf, Arr *tables)
{
	char *table_name;
	int *ilogic;
	Tbl *t;
	Dbptr dbj;
	long int nrec;
	int i;
	int ntables=0;

	/* This is an exact copy of above, but it is duplicated because
	this function could get stolen by another future program
	because it is pretty general */
	t = pfget_tbl(pf,"join_tables");
	if(t == NULL)
	{
		elog_die(0,"No list of tables to be joined\n");
	}
	for(i=0;i<maxtbl(t);++i)
	{
		table_name = gettbl(t,i);
		ilogic = (int *)getarr(tables,table_name);
		if(ilogic == NULL) 
		{
			elog_die(0,"Table %s was not handled previously by check_tables.\nProgramming logic error\n",
				table_name);
		}
		else if(*ilogic)
		{
			if(ntables == 0)
				dbj = dblookup(db,0,table_name,0,0);
			else
				dbj=dbjoin(dbj,dblookup(db,0,table_name,0,0),
					NULL,NULL,0,NULL,0);
			++ntables;
			dbquery(dbj,dbRECORD_COUNT,&nrec);
			if(nrec == 0)
			{
				elog_complain(0,
					"join_tables error\njoined database has 0 length after joining table %s\n",
					table_name);
				dbj.record = dbINVALID;
				return(dbj);
			}
		}
	}
	return(dbj);
}

	

void set_shot_variable(Dbptr db, Arr *tables, int evid, SegyHead *h)
{
	int *ok;
	char ss_string[30];
	long int ntest;
	double dnorth, deast, elev, edepth;

	ok = getarr(tables,"shot");
	if(ok)
	{
		db = dblookup(db,0,"shot",0,0);
		sprintf(ss_string,"evid == %d",evid);
		db = dbsubset(db,ss_string,0);
		dbquery(db,dbRECORD_COUNT,&ntest);
		if(ntest <= 0) 
		{
			elog_complain(0,"evid %d not found in shot table\nShot coordinates will not be saved in segy headers\n",
				evid);
			dbfree(db);
			return;
		}
		else if(ntest > 1)
		{
			elog_notify(0,"multiple rows in shot found for evid %d\n",
				evid);
		}
		db.record = 0;
		if(dbgetv(db,0,
			"dnorth",&dnorth,
			"deast", &deast,
			"elev", &elev,
			"edepth",&edepth,
				NULL) == dbINVALID) 
		{
			elog_complain(0,"dbgetv error for evid %d\nShot coordinates will not be saved in segy headers\n",
				evid);
			return;
		}
		/*convert to m from km */
		deast *= 1000.0;
		dnorth *= 1000.0;
		h->sourceLongOrX = (int32_t) deast;
		h->sourceLatOrY = (int32_t) dnorth;
		h->sourceSurfaceElevation = (int32_t)elev;
		h->sourceDepth = (int32_t)edepth;
		/* WARNING:  This assumes receiver coordinates have already been set */
		h->sourceToRecDist = (int32_t) hypot(dnorth - ((double)(h->recLatOrY)),
						deast - ((double)(h->recLongOrX)) );
	}
	dbfree(db);
}
/* The trace library routines that existed at the time this code
was written were heavy handed about dealing with taps.  trload_css
calls trgetwf which in turn calls gaps2tr.  This sets all gaps to 
the fillgap value for the specified data type.  Unfortunately, 
this causes a problem with clipped data.  A clip is indeed a "gap"
of a type, but there is a difference between a + and - full scale
reading.  We call tr2gaps below, which is not a clean inverse
of gaps2tr because it only recovers the + full scale values 
correctly leaving a worse tear for negative values.  I circumvent 
this here by checking the sign of the sample preceeding a gap and
flipping the sign when appopriate.  
*/
void repair_gaps(Dbptr trdb)
{
	Trsample *trdata;
	long int nsamp;
	char datatype[4];
	long int ntraces;
	int i,i0;
	Wftype *g;

	dbquery(trdb,dbRECORD_COUNT,&ntraces);
	for(trdb.record=0;trdb.record<ntraces;++trdb.record)
	{
		if(dbgetv(trdb,0,
			"datatype",datatype,
			"data",&trdata,
			"nsamp",&nsamp,
		NULL) == dbINVALID)
		{
			elog_die(0,"dbgetv error during gap processing\n");
		}
		g = trwftype(datatype);
		/* this function is supposed to set values to g->fill*/
		tr2gaps(trdata,nsamp,datatype);
		/* this will fill a gaps at the front or end of a trace with
		zeros*/
		if(trdata[nsamp-1] >= (g->fill))
		{
			i=nsamp-1;
			while(trdata[i] >= (g->fill))
			{
				trdata[i] = (Trsample)0.0;
				--i;
			}
		}
		if(trdata[0] >= (g->fill))
		{
			i0=0;
			while(trdata[i0] >= (g->fill))
			{
				trdata[i0] = (Trsample)0.0;
				++i0;
			}
 

		}
		else
		{
			i0 = 1;
		}
		/* This is the repair for negative clips.  We start
		at one or we can induce a seg fault, but this means
		start transients are possible.  I judged this not
		worth messing with.  Note this assumes that g->fill
		is a positive number.  All the examples in gaps.pf are.*/
		for(i=i0;i<nsamp;i++)
		{
			if(trdata[i] >= (g->fill))
			{
				if(trdata[i-1] < 0.0)
				{
					while((trdata[i]>=(g->fill)) && (i<nsamp))
					{
						trdata[i]=(Trsample)((g->lower));
						++i;
					}
				}
				else
				{
					while((trdata[i]>=(g->fill)) && (i<nsamp))
						++i;
				}
			}
		}
	}
}
		

		
		

int main(int argc, char **argv)
{
	SegyReel reel;
	SegyHead *header;
	char *dbin;
	char *outfile;
	FILE *fp;
	Pf *pf;  
	Arr *channels;  /* channel order list */
	Arr *table_list;  /* array of valid tables */
	int nchan;
	char *stest;

	float **traces;
	char reel1[3200];
	Dbptr db, trdb, dbj;
	Dbptr trdbss;  
	int nsamp0;
	double time0, endtime0, samprate0;
	long int nsamp;
	double samprate;
	int i,j;
	char stime[30],etime[30];
	char s[128];
	double tlength;
	double phi, theta;
	char *newchan_standard[3]={"X1","X2","X3"};
	char *trsubset="chan=~/X./";
	char *newchan[3]={"R","T","Z"};
	Tbl *sortkeys=newtbl(0);
	char sta[10],chan[10];
	double lat, lon, elev, dnorth, deast, edepth;
	char refsta[10];
	int total_traces=0;
	char *time_str;
	long int evid,shotid=1;
	int rotate=0;
	long int ntraces;
        int ichan;
	int map_to_cdp;  /* logical switch to output data like cdp stacked data */
	char *fmt="%Y %j %H %M %S %s";
	char *pfname;
	int Verbose=0;
	/* New features added 2009 */
	/* this is a boolean.  If true (nonzero) it is assumed stdin will
	contain four numbers:  time,lat, lon, elev.  If false, only the
	time field is read and remainder of any input on each line is dropped.*/
	int input_source_coordinates;
	/* scale factor for source coordinates.  Needed because segy uses
	an int to store source coordinates.  Sensible choices are 
	3600 for arc seconds and 10000 for a pseudodecimal. Note this
	parameter is ignored unless input_source_coordinates is true.*/
	int coordScale;
	/* If true use passcal 32 bit extension num_samps as record length. 
	SEGY standard uses a 16 bit entry that easily overflows with large
	shots at long offset.  In this ase assume the 16 bit quantity is
	meaningless. */
	int use_32bit_nsamp;
	/* This is switched on by argument switch.  When set to a nonzero
	(default) the reel headers are written.  When 0 `
	the reel heades will not be written -- used by seismic unix 
r
	and passcal*/
	int write_reel_headers=1;
	char *substr=NULL;

	if(argc < 3) usage();
	dbin = argv[1];
	outfile = argv[2];
	pfname = NULL;
	for(i=3;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			pfname = argv[i];
		}
		else if(!strcmp(argv[i],"-SU"))
		{
			write_reel_headers=0;
		}
		else if(!strcmp(argv[i],"-v"))
		{
			Verbose=1;
		}
		else if(!strcmp(argv[i],"-ss"))
		{
			++i;
			substr=argv[i];
		}
		else
		{
			usage();
		}
	}
	if(pfname == NULL) pfname = strdup("db2segy");

	elog_init(argc, argv);

	if(pfread(pfname,&pf)) 
		elog_die(0,"pfread error for pf file %s.pf\n",argv[0]);
	/* rotation parameters */
	rotate=pfget_boolean(pf,"rotate");
	if(rotate)
	{
		phi = pfget_double(pf,"phi");
		theta = pfget_double(pf,"theta");
	}
	/* This function creates the channel order list keyed by
	station channel names */
	channels = build_stachan_list(pf,&nchan,Verbose);

	map_to_cdp = pfget_boolean(pf,"map_to_cdp");
	if(map_to_cdp && Verbose) 
		fprintf(stdout,"Casting data as CDP stacked section\n");
	if(dbopen(dbin,"r",&db) == dbINVALID) 
	{
		fprintf(stderr,"Cannot open db %s\n", dbin);
		usage();
	}
	/* We grab the sample rate and trace length (in seconds) and
	use this to define global sample rates for the data.  
	segy REQUIRES fixed length records and sample rates, so
	irregular sample rates will cause this program to die. 
	One could add a decimate/interpolate function, but this 
	is not currently implemented */
	samprate0 = pfget_double(pf,"sample_rate");
	tlength = pfget_double(pf,"trace_length");
	nsamp0 = (int)(tlength*samprate0);
	use_32bit_nsamp=pfget_boolean(pf,"use_32bit_nsamp");

	/* nsamp in segy is a 16 bit field.  Handling depends on
	setting of use_32bit_nsamp boolean */
	if(nsamp0 > 32767) 
	{
	    if(use_32bit_nsamp)
	    {
	    	elog_notify(0,"Warning:  segy ues a 16 bit entity to store number of samples\nThat field is garbage. Using the 32 bit extension field.\n");
	    }
	    else
	    {
		elog_complain(0,
		  "Warning:  segy uses a 16 bit entity to store number of samples\nRequested %d samples per trace.  Trucated to 32767\n",nsamp0);
		nsamp0 = 32767;
	    }
	}
	input_source_coordinates=pfget_boolean(pf,"input_source_coordinates");
	if(input_source_coordinates)
	{
		coordScale=pfget_int(pf,"coordinate_scale_factor");
	}
	else
	{
		coordScale=1;
	}
	/* boolean.  When nonzero set coordinates as geographic arc seconds values */
	int use_geo_coordinates=pfget_boolean(pf,"use_geo_coordinates");
	/* check list of tables defined in pf.  Return array of
	logicals that define which tables are valid and join 
	tables. */
	table_list = check_tables(db,pf);
	check_for_required_tables(table_list);
	dbj = join_tables(db,pf,table_list);
	if(dbj.record == dbINVALID) elog_die(0,"dbjoin error\n");
	if(substr!=NULL) dbj=dbsubset(dbj,substr,0);
	long int ndbrows;
	dbquery(dbj,dbRECORD_COUNT,&ndbrows);
	if(ndbrows<=0)
	{
		fprintf(stderr,"Working database view is empty\n");
		if(substr!=NULL) fprintf(stderr,"Subset condtion =%s a likely problem\n",
				substr);
		usage();
	}

	fp = fopen(outfile,"w");
	if(fp == NULL) 
	{
		fprintf(stderr,"Cannot open output file %s\n",outfile);
		usage();
	}

	/* These are needed for sort below */
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"chan");

	/*The reel1 header in true blue segy is ebcdic.  We are goingto 
	just fill it with nulls and hope for the best */
	for(i=0;i<3200;i++) reel1[i] = '\0';

	/* Just blindly write this turkey. Bad form, but tough*/
	if(write_reel_headers) fwrite(reel1,1,3200,fp);

	/* memory allocation for trace data.  This is a large matrix
	that is cleared for each event.  This model works because of
	segy's fixed length format.  This routine is a descendent of
	numerical recipes routine found in libgenloc.  This is not
	the most efficient way to do this, but it simplifies the
	algorithm a lot. */
	traces = matrix(0,nchan,0,nsamp0);
	if(traces == NULL) 
		elog_die(0,"Cannot alloc trace data matrix work space of size %d by %d\n",
			nchan, nsamp0);
	header = (SegyHead *)calloc((size_t)nchan,sizeof(SegyHead));
	if(header == NULL)
			elog_die(0,"Cannot alloc memory for %d segy header workspace\n",nchan);
	if(write_reel_headers)
	{

		/* now fill in the binary reel header and write it */
		reel.kjob = 1;
		reel.kline = 1;
		reel.kreel = 1;
		reel.kntr = (int16_t)nchan;
		reel.knaux = 0;
		reel.sr = (int16_t)(1000000.0/samprate0);
		reel.kfldsr = reel.sr;
		reel.knsamp = (int16_t)nsamp0;
		reel.kfsamp = (int16_t)nsamp0;
		reel.dsfc=5;  /* This is ieee floats*/
		reel.kmfold = 0;
		if(map_to_cdp)
			reel.ksort = 2;
		else
			reel.ksort = 1;
		reel.kunits = 1;  /* This sets units to always be meters */
		for(i=0;i<344;++i)reel.unused2[i]='\0';
	
		if(fwrite((void *)(&reel),sizeof(SegyReel),1,fp) != 1) 
		{
			fprintf(stderr,"Write error for binary reel header\n");
			exit(-2);
		}
	}

	/* Now we enter a loop over stdin reading start times.  
	Program will blindly ask for data from each start time to 
	time+tlength.  The trace buffer will be initialized to 
	zeros at the top of the loop always.  If nothing is found
	only zeros will be written to output.  
	*/
	while((stest=fgets(s,80,stdin)) != NULL)
	{
		double slat,slon,selev;  /* Used when reading source location*/
		if(Verbose)
			fprintf(stdout,"Processing:  %s\n",s);
		for(i=0;i<nchan;++i)
		{
			initialize_header(&(header[i]));
			header[i].lineSeq = total_traces + i + 1;
			header[i].reelSeq = header[i].lineSeq;
			if(map_to_cdp)
			{
				header[i].cdpEns = i + 1;
				header[i].traceInEnsemble = 1;  /* 1 trace per cdp faked */
			}
			else
			{
				header[i].channel_number = i + 1;
			}
			header[i].event_number = shotid;
			header[i].energySourcePt=shotid;
			for(j=0;j<nsamp0;++j)  traces[i][j] = (Trsample)0.0;
		}
		if(input_source_coordinates)
		{
			char stmp[40];
			sscanf(s,"%s%ld%lf%lf%lf",stmp,&shotid,&slon,&slat,&selev);
			time0=str2epoch(stmp);
		}
		else
		{
			time0 = str2epoch(s);
		}
		endtime0 = time0 + tlength;
		sprintf(stime,"%20.4f",time0);
		sprintf(etime,"%20.4f",endtime0);
		trdb.database = -1;
		if(trload_css(dbj,stime,etime,&trdb,0, 0) < 0)
		{
			if(Verbose) 
			{
			  fprintf(stdout,"trload_css failed for shotid=%ld",shotid);
			  fprintf(stdout,"  No data in time range %s to %s\n",
			  	strtime(time0),strtime(endtime0) );
			  fprintf(stdout,"No data written for this shotid block.");
			  fprintf(stdout,"  Handle this carefully in geometry definitions.\n");
			}

			continue;
		}
		/* This does gap processing */
		repair_gaps(trdb);
		
		trapply_calib(trdb);
			
		if(rotate)
		{
			if(rotate_to_standard(trdb,newchan_standard))
				elog_notify(0,"Data loss in rotate_to_standard for event %s to %s\n",
					stime, etime);
			/* This is need to prevent collisions of channel 
			names */
			trdbss = dbsubset(trdb,trsubset,0);
			if(trrotate(trdbss,phi,theta,newchan))
				elog_notify(0,"Data loss in trrotate for event %s to %s\n",
					stime, etime);
		}
		if(Verbose)
			fprintf(stdout,"Station  chan_name  chan_number seq_number shotid  evid\n");
		trdb = dbsort(trdb,sortkeys,0,0);
		dbquery(trdb,dbRECORD_COUNT,&ntraces);
		if(Verbose) fprintf(stdout,"Read %ld traces for event at time%s\n",
			ntraces,strtime(time0));
		for(trdb.record=0;trdb.record<ntraces;++trdb.record)
		{
			Trsample *trdata;
			if(dbgetv(trdb,0,
			    "evid",&evid,
			    "sta",sta,
			    "chan",chan,
			    "nsamp", &nsamp,
			    "samprate",&samprate,
			    "data",&trdata,
			    "lat", &lat,
			    "lon", &lon,
			    "elev",&elev,
			    "refsta",refsta,
			    "dnorth",&dnorth,
			    "deast",&deast,
			    "edepth",&edepth,
					NULL) == dbINVALID)
			{
				elog_complain(0," dbgetv error reading record %ld\nTrace will be skipped for station %s and channel %s\n",
				trdb.record,sta,chan);
				continue;
			}
			/* Allow 1 percent samprate error before killing */
			double fsrskew=fabs((samprate-samprate0)/samprate0);
			double frskewcut=0.01;
			if(fsrskew>frskewcut) 
			{
				elog_complain(0,"%s:%s sample rate %f is significantly different from base sample rate of %f\nTrace skipped -- segy requires fixed sample rates\n",
					sta,chan,samprate,samprate0);
				continue;
			}
			if(nsamp > nsamp0)
			{
				elog_complain(0,"%s:%s trace has extra samples=%ld\nTruncated to length %d\n",
					sta, chan, nsamp, nsamp0);
				nsamp = nsamp0;
			}
			else if(nsamp < nsamp0)
			{
				elog_complain(0,"%s:%s trace is shorter than expected %d samples\nZero padded after sample %ld\n",
					sta, chan, nsamp0, nsamp);
			}

			ichan = get_channel_index(channels,sta,chan);
			if(ichan > nchan) elog_die(0,"Channel index %d outside limit of %d\nCannot continue\n",
					ichan, nchan);
			if(ichan >= 0)
			{
				if(Verbose) 
				   fprintf(stdout,"%s:%s\t%-d\t%-d\t%-ld\t%-ld\n",
					sta,chan,ichan+1,
                                        header[ichan].reelSeq,
					shotid, evid);
				header[ichan].traceID = 1;
				for(j=0;j<nsamp;++j) 
				   traces[ichan][j] = (float)trdata[j];
				/* header fields coming from trace table */
				header[ichan].samp_rate = (int32_t)
						(1000000.0/samprate0);
				if(!use_geo_coordinates && ( coordScale==1))
				{
				  header[ichan].recLongOrX = (int32_t)(deast*1000.0);
				  header[ichan].recLatOrY = (int32_t)(dnorth*1000.0);
				}
				else
				{
				/* Note negative here.  This is a oddity
				of segy that - means divide by this to
				get actual.  Always make this negative in case 
				user inputs a negative number. */
				  header[ichan].coordScale=-abs(coordScale);
				  /* Force 2 = geographic coordinates.  Standard says when this is
				  so units are arc seconds, hence we multiply deg by 3600*coordScale */
				  if(use_geo_coordinates)
				  {
				    header[ichan].coordUnits=2;
				    header[ichan].recLongOrX
				     =(int32_t)(lon*3600.0*(double)coordScale);
				    header[ichan].recLatOrY
				     =(int32_t)(lat*3600.0*(double)coordScale);
				  }
				  else
				  {
				    header[ichan].recLongOrX
				     =(int32_t)(lon*(double)coordScale);
				    header[ichan].recLatOrY
				     =(int32_t)(lat*(double)coordScale);
				  }
				}
				header[ichan].recElevation = (int32_t)(elev*1000.0);
				header[ichan].deltaSample = (int16_t) 
						(1000000.0/samprate0);
				header[ichan].sampleLength = (int16_t)nsamp0;
				header[ichan].num_samps = (int32_t)nsamp0;
				/* This cracks the time fields */
				time_str = epoch2str(time0,fmt);
				sscanf(time_str,"%hd %hd %hd %hd %hd %hd",
					&header[ichan].year,
					&header[ichan].day,
					&header[ichan].hour,
					&header[ichan].minute,
					&header[ichan].second,
					&header[ichan].m_secs);
				/* These are PASSCAL extensions, but we'll
				go ahead and set them anyway.*/
				header[ichan].trigyear = header[ichan].year;
				header[ichan].trigday = header[ichan].day;
				header[ichan].trighour = header[ichan].hour;
				header[ichan].trigminute = header[ichan].minute;
				header[ichan].trigsecond = header[ichan].second;
				free(time_str);
				if(input_source_coordinates)
				{
				  if(use_geo_coordinates)
				  {
					slat*=3600.0;
					slon*=3600.0;
				  }
				  header[ichan].sourceLongOrX
				    =(int32_t)(slon*(double)coordScale);
				  header[ichan].sourceLatOrY
				    =(int32_t)(slat*(double)coordScale);
				  header[ichan].sourceSurfaceElevation
				             =(int32_t)selev;
				  /* No easy way to specify both elev and depth*/
				  header[ichan].sourceDepth=0;
				}
				else if(map_to_cdp)
				{
				/* When faking CDP data we make this look 
				like a zero offset, single fold data set */
				  header[ichan].sourceLongOrX = header[ichan].recLongOrX;
				  header[ichan].sourceLatOrY = header[ichan].recLatOrY;
				  header[ichan].sourceSurfaceElevation = header[ichan].recElevation;
				  header[ichan].sourceDepth = 0;
				  header[ichan].sourceToRecDist = 0;
				}
				else
				{
				/* This is the mechanism for adding other
				information with added tables.  The one
				table currently supported is a "shot" table 
				that holds shot coordinates.  If other tables
				were added new functions could be added with
				a similar calling sequence.  This procedure
				silently does nothing if a shot table is not
				present.*/
					set_shot_variable(db,table_list,
						evid,&header[ichan]);
				}
			}			
			else
			{
				if(Verbose)
					fprintf(stdout,"Station %s and channel %s skipped\n",
						sta,chan);
			}

		}
		/* Now we write the data */
		for(i=0;i<nchan;++i)
		{
			if(fwrite((void *)(&(header[i])),sizeof(SegyHead),1,fp) != 1)
				elog_die(0,"Write error on header for trace %d\n",total_traces+i);		
			if(fwrite((void *)traces[i],sizeof(float),
					(size_t)nsamp0,fp) != nsamp0)
				elog_die(0,"Write error while writing data for trace %d\n",
					total_traces+i);
		}
		total_traces += nchan;
		trdestroy(&trdb);		
		if(!input_source_coordinates) ++shotid;
	}
	return 0 ;
}

