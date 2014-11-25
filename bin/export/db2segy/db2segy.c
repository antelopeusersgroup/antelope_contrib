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

Authors:  Gary L. Pavlis and Geoffrey A Davis
Written:  November 1998 in a hack form.  This version is a descendent
that recycled some of the original code written in February 1999.
Updated: 2014 with SEGY Rev1 compatibility

*/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "tr.h"
#include "pf.h"
#include "elog.h"
#include "segy.h"
#include "deviants.h"
#define min(a,b) ((a) <= (b) ? (a) : (b))
/* Newer compilers will complain if these prototypes are not defined.
Could be placed in segy.y, but that would strike me as mysterious. */

void initialize_trace_header(SEGYTraceHeader *header, int16_t segy_format);
void initialize_binary_file_header(SEGYBinaryFileHeader *reel, int16_t segy_format);
char *make_key(char *s, char *c);
Arr *build_stachan_list(Pf *pf, int *nchan,int verbose);
int get_channel_index(Arr *a, char *sta, char *chan);
Arr *check_tables(Dbptr db, Pf *pf);
void check_for_required_tables(Arr *tabarray);
Dbptr join_tables(Dbptr db, Pf *pf, Arr *tables);
void set_shot_variable(Dbptr db, Arr *tables, int evid, SEGYTraceHeader *h);
void repair_gaps(Dbptr trdb);
int16_t get_trace_id_code_from_segtype(char segtype);
void initialize_text_header (char *out, int16_t segy_format, char *desc);

/* Fill out the 3200-byte text header block. This block is organized into 40
 * records of 80 character columns, with the first four characters of each
 * line/record serving as record markers. The record numbers are in the format
 * "CXX " where XX is a space padded two digit number.
 *
 * If the requested segy_format is not Rev 1 or greater, output all text in
 * EBCDIC, translating any supplied ASCII text on the fly.
 *
 * The desc string is an optional 76 character string that will go in the first
 * record (aka "C 1")
 * If desc overflows 76 characters, it is truncated and a warning is sent to
 * elog. */
void
initialize_text_header (char *out, int16_t segy_format, char *desc)
{
	memset(out, '\0', SEGY_TEXT_HEADER_SIZE);

	int ebcdic=1;
	/* If we're SEG-Y Rev1 or later, use ASCII */
	if(ntohs(segy_format) >= 0x0100) {
		ebcdic=0;
	}

	/* Fill in the standard record headers */
	for(int row=0; row < SEGY_TEXT_HEADER_RECORDS; row++)
	{
		snprintf(out + (row*SEGY_TEXT_HEADER_COLUMNS),
				SEGY_TEXT_HEADER_COLUMNS, "C%2d ", row+1);
	}

	int desclen=0;
	if (desc){
		desclen=strlen(desc);
	}

	if (desclen > 0) {
		/* insert our description in record 1 */
		elog_debug(0,"Got a description field: %s", desc);
		if (desclen > SEGY_TEXT_HEADER_USABLE_COLUMNS) {
			elog_complain(0,
					"The description field overflows the allowed %d character limit by %d characters. Truncating.",
					SEGY_TEXT_HEADER_USABLE_COLUMNS,
					desclen - SEGY_TEXT_HEADER_USABLE_COLUMNS);
		}
		strncpy(out+4, desc, SEGY_TEXT_HEADER_USABLE_COLUMNS);
	}

	/* Insert our advertising clause */
	snprintf( out + (37*SEGY_TEXT_HEADER_COLUMNS),
			SEGY_TEXT_HEADER_COLUMNS, "C38 OUTPUT BY: ANTELOPE DB2SEGY");

	/* Print out the mandatory SEG-Y Rev1 format marker */
	if (segy_format == SEGY_FORMAT_REV_1_0) {
		snprintf(out+(38*SEGY_TEXT_HEADER_COLUMNS),
				SEGY_TEXT_HEADER_COLUMNS, "C39 SEG Y REV1");
	}
	/* Print out the SEG-Y end blurb (Mandatory in Rev1) */
	if (ebcdic) {
		snprintf(out+(39*SEGY_TEXT_HEADER_COLUMNS),
				SEGY_TEXT_HEADER_COLUMNS, "C40 END EBCDIC");
	} else {
		snprintf(out+(39*SEGY_TEXT_HEADER_COLUMNS),
				SEGY_TEXT_HEADER_COLUMNS, "C40 END TEXTUAL HEADER");
	}

	/* Translate from ASCII to EBCDIC if necessary */
	if (ebcdic) { for(int i=0; i < SEGY_TEXT_HEADER_SIZE; i++) {
		char *s = out + i;
		*s = a2e[(unsigned char)*s ];
	} }
}

/* Map CSS3.0 segtype to one of the SEG-Y rev1 trace identification codes.
 * At this point we're going to assume 1 (seismic) for types A or V,
 * -1 (Other) for a few known types,
 * and 0 (Unknown) otherwise */
int16_t get_trace_id_code_from_segtype(char segtype)
{
	switch(segtype)
	{
		case 'A':
		case 'V':
		case 'D':
			return SEGY_TRACE_ID_SEISMIC;
			break;
		case 'I':
			return SEGY_TRACE_ID_OTHER;
			break;
		default:
			return 0;
	}
	return 0;
}

static void
usage()
{
	fprintf(stderr,"Usage:  db2segy dbin outfile [-pf pffile [-SU|-V SU|-V 0|-V 1] -ss subset -d \"description\"]\n");
	exit(-1);
}

void
initialize_binary_file_header(SEGYBinaryFileHeader *reel, int16_t segy_format)
{
	assert( sizeof(SEGYBinaryFileHeader)==SEGY_BINARY_HEADER_SIZE );
	assert( segy_format == SEGY_FORMAT_REV_0 || \
			segy_format == SEGY_FORMAT_REV_1_0 );

	memset(reel, '\0', sizeof(SEGYBinaryFileHeader));
	reel->segy_format = segy_format;

    /* This is technically REV1 only, but the field was part of a large
     * unused block in the original version of this program. */
    reel->fixed_length_trace_flag = SEGY_TRLEN_FIXED;
}

void
initialize_trace_header(SEGYTraceHeader *header, int16_t segy_format)
{
	assert( sizeof(SEGYTraceHeader)==SEGY_TRACE_HEADER_SIZE );
	memset(header, '\0', sizeof(SEGYTraceHeader));
	header->lineSeq = htonl(1);
	header->event_number = htonl(1);
	header->channel_number = htonl(1);
	header->energySourcePt = htonl(1);
	header->cdpEns = htonl(0);
	header->traceInEnsemble = htonl(0);
	header->traceID = SEGY_TRACE_ID_DEAD;
	header->vertSum = htons(0);
	header->dataUse = htons(0);
	header->sourceToRecDist = htonl(0);
	header->recElevation = htonl(0);
	header->sourceSurfaceElevation = htonl(0);
	header->sourceDepth = htonl(0);
	header->datumElevRec = htonl(0);
	header->recWaterDepth = htonl(0);
	header->elevationScale = htons(1);
	header->coordScale = htons(1);
	header->sourceLongOrX = htonl(0);
	header->recLongOrX = htonl(0);
	header->coordUnits = SEGY_TRACE_COORDUNITS_LENGTH;  /* sets units to m */
	header->weatheringVelocity = htons(0);
	header->subWeatheringVelocity = htons(0);
	header->sourceUpholeTime = htons(0);
	header->sourceStaticCor = htons(0);
	header->totalStatic = htons(0);
	header->lagTimeA = htons(0);
	header->delay = htons(0);
	header->muteStart = htons(0);
	header->sampleLength = htons(0);
	header->deltaSample = htons(0);
	header->gainType = SEGY_TRACE_GAIN_UNKNOWN;
	header->gainConst = htons(0);
	header->correlated = htons(0);
	header->sweepStart = htons(0);
	header->sweepLength = htons(0);
	header->sweepType = htons(0);
	header->sweepTaperAtStart = htons(0);
	header->taperType = htons(0);
	header->aliasFreq = htons(0);
	header->notchFreq = htons(0);
	header->lowCutFreq = htons(0);
	header->lowCutSlope = htons(0);
	header->year = htons(0);
    /* SEG-Y "classic" supports only time basis codes 0-3. Antelope records
     * in UTC (which has leap second corrections). */
    if (ntohs(segy_format)>=0x0100) {
        header->timeBasisCode = SEGY_TRACE_TIMEBASIS_UTC;
    } else {
        header->timeBasisCode = SEGY_TRACE_TIMEBASIS_GMT;
    }
	header->traceWeightingFactor = htons(0);
	header->phoneRollPos1 = htons(0);
	header->gapSize = htons(0);
	header->taperOvertravel = htons(0);
	/*header->extrash[10] is pre-zero'd */
	header->samp_rate = htonl(0);
	/* Begin Pavlis/IRIS-PASSCAL non-standard extensions */
	if (ntohs(segy_format)<0x0100) {
		/* Always ieee floats in this program for now */
		header->data_form  = htons(5);
		header->trigyear   = htons(0);
		header->trigday    = htons(0);
		header->trighour   = htons(0);
		header->trigminute = htons(0);
		header->trigsecond = htons(0);
		header->trigmills  = htons(0);
		header->scale_fac = htonf(0);
		header->inst_no = htons(0);
		header->not_to_be_used = htons(0);
		header->num_samps = htonl(0);
		/* header->extra is pre-zeroed */
	}
	/* End Pavlis non-standard extension */
	header->reelSeq = htonl(1);
	header->horSum = htons(0);
	header->datumElemSource = htonl(0);
	header->sourceWaterDepth = htonl(0);
	header->sourceLatOrY = htonl(0);
	header->recLatOrY = htonl(0);
	header->recUpholeTime = htons(0);
	header->recStaticCor = htons(0);
	header->lagTimeB = htons(0);
	header->muteEnd = htons(0);
	header->sweepTaperAtEnd = htons(0);
	header->aliasSlope = htons(0);
	header->notchSlope = htons(0);
	header->hiCutFreq = htons(0);
	header->hiCutSlope = htons(0);
	header->day    = htons(0);
	header->hour   = htons(0);
	header->minute = htons(0);
	header->second = htons(0);
	header->phoneFirstTrace = htons(0);
	header->phoneLastTrace  = htons(0);
	header->m_secs = htons(0);
	header->initialGain = htons(0);
	header->sweepEnd    = htons(0);
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
		elog_notify(0,"Station   Channel_code    Channel_number\n");
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
			elog_notify(0,"%s  %s  %d\n",sta,chan,(*channel_number)+1);
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



void set_shot_variable(Dbptr db, Arr *tables, int evid, SEGYTraceHeader *h)
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
		h->sourceLongOrX          = htonl((int32_t) deast);
		h->sourceLatOrY           = htonl((int32_t) dnorth);
		h->sourceSurfaceElevation = htonl((int32_t) elev);
		h->sourceDepth            = htonl((int32_t) edepth);
		/* WARNING:  This assumes receiver coordinates have already been set */
		h->sourceToRecDist = htonl( (int32_t) hypot(
					dnorth - ((double)(ntohl(h->recLatOrY ))),
					deast  - ((double)(ntohl(h->recLongOrX)))
		));
	}
	dbfree(db);
}

/* The trace library routines that existed at the time this code
was written were heavy handed about dealing with gaps.  trload_css
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
	SEGYBinaryFileHeader reel;
	SEGYTraceHeader *header;
	char *dbin;
	char *outfile;
	FILE *fp;
	Pf *pf;
	Arr *channels;  /* channel order list */
	Arr *table_list;  /* array of valid tables */
	int nchan;
	char *stest;

	float **traces;
	char text_file_header[SEGY_TEXT_HEADER_SIZE];
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
	char segtype;
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
	the reel headers will not be written -- used by seismic unix
	and passcal*/
	int write_reel_headers=1;

	/* SEG-Y version to output. Default is original 1975 spec (rev 0) */
	int16_t segy_format = SEGY_FORMAT_REV_0;

	/* dbsubset query string */
	char *substr=NULL;

	/* text_header_description is a buffer holding a user-supplied description
	 * to be placed in the 3200-byte text header block. It is controlled by
	 * the parameter file value text_header_description or by the -d command
	 * line option, with the latter taking precedence */
	char* text_header_description=NULL;

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
		else if(!strcmp(argv[i],"-d"))
		{
			++i;
			text_header_description = strdup(argv[i]);
		}
		else if(!strcmp(argv[i],"-ss"))
		{
			++i;
			substr=argv[i];
		}
		else if(!strcmp(argv[i],"-V"))
		{
			++i;
			if     (!strcmp(argv[i],"0")) {segy_format = SEGY_FORMAT_REV_0;}
			else if(!strcmp(argv[i],"1")) {segy_format = SEGY_FORMAT_REV_1_0;}
			else if(!strcmp(argv[i],"SU"))
			{
				segy_format = SEGY_FORMAT_SU;
				write_reel_headers=0;
			}
			else
			{
				elog_complain(0, "SEG-Y Version must be either 1 or 0");
				usage();
			}
		}
		else
		{
			usage();
		}
	}
	/* Command-line parameter sanity checking */
	if (write_reel_headers==0 && segy_format != SEGY_FORMAT_SU){
		complain(0, "The SU option cannot be used with the -V option");
		usage();
	}
	if(pfname == NULL) pfname = strdup("db2segy");

	elog_init(argc, argv);

	if(pfread(pfname,&pf)) {
		elog_die(0,"pfread error for pf file %s.pf\n",argv[0]);
	}

	/* Read the text_header_description if we weren't passed the -d option */
	if (!text_header_description) {
		text_header_description=pfget_string(pf, "text_header_description");
	}

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
		elog_notify(0,"Casting data as CDP stacked section\n");
	if(dbopen(dbin,"r",&db) == dbINVALID)
	{
		elog_complain(1,"Cannot open db %s\n", dbin);
		usage();
	}
	/* We grab the sample rate and trace length (in seconds) and
	use this to define global sample rates for the data.
	SEG-Y REV0 REQUIRES fixed length records and sample rates, so
	irregular sample rates will cause this program to die.
	One could add a decimate/interpolate function, but this
	is not currently implemented */
	samprate0 = pfget_double(pf,"sample_rate");
	tlength = pfget_double(pf,"trace_length");
	nsamp0 = (int)(tlength*samprate0);
	use_32bit_nsamp=pfget_boolean(pf,"use_32bit_nsamp");
	if (ntohs(segy_format) >= 0x0100 && use_32bit_nsamp) {
		elog_complain(0,"The 32-bit extension field is incompatible with SEG-Y REV 1. Ignoring 'use_32bit_nsamp' from the parameter file");
		use_32bit_nsamp=0;
	}

	/* nsamp in segy is a 16 bit field.  Handling depends on
	setting of use_32bit_nsamp boolean */
	if(nsamp0 > SEGY_MAX_NSAMP)
	{
		if(use_32bit_nsamp)
		{
			elog_notify(0,"Warning:  segy uses a 16 bit entity to store number of samples\nThat field is garbage. Using the 32 bit extension field.");
		}
		else
		{
		elog_complain(0,
		  "Warning:  segy uses a 16 bit entity to store number of samples. Requested %d samples per trace.  Trucated to %d", nsamp0, SEGY_MAX_NSAMP);
		nsamp0 = SEGY_MAX_NSAMP;
		}
	}

	/* boolean.  When nonzero set coordinates as geographic arc seconds values */
	int use_geo_coordinates=pfget_boolean(pf,"use_geo_coordinates");

	/* boolean. When nonzero, output decimal degrees instead of arcseconds if
	 * the requested output format supports it (rev1 only) */
	int prefer_decimal_degrees=pfget_boolean(pf, "prefer_decimal_degrees");

	/* We now have enough information to decide the coordUnits for all traces */
	int coordUnits = 0;
	if (!use_geo_coordinates) {
		coordUnits=SEGY_TRACE_COORDUNITS_LENGTH;
	} else if (ntohs(segy_format) >= 0x0100 && prefer_decimal_degrees) {
		coordUnits=SEGY_TRACE_COORDUNITS_DECIMAL_DEGREES;
	} else {
		coordUnits=SEGY_TRACE_COORDUNITS_ARCSECONDS;
	}
	/* We should have set our coordinate units now */
	assert(coordUnits!=0);

	input_source_coordinates=pfget_boolean(pf,"input_source_coordinates");
	if(input_source_coordinates)
	{
		coordScale=pfget_int(pf,"coordinate_scale_factor");
	}
	else if (coordUnits==SEGY_TRACE_COORDUNITS_DECIMAL_DEGREES)
	{
		/* Use a sane scalar for decimal degrees. 10000 gives four decimal
		 * places of accuracy, which matches the CSS3.0 spec for lat and lon */
		coordScale=10000;
	}
	else
	{
		coordScale=1;
	}

	/* Print a diagnostic message if the user gave a sub-optimal value for the
	 * coordScale */
	if (coordUnits == SEGY_TRACE_COORDUNITS_DECIMAL_DEGREES &&
			coordScale < 10000)
	{
		elog_alert(0,
				"The supplied parameter 'coordinate_scale_factor' value of %d is less than 10000, and will cause loss of precision for decimal degree coordinates.",
				coordScale);
	}
    else if (coordUnits == SEGY_TRACE_COORDUNITS_ARCSECONDS)
    {
        if (coordScale > 1000) {
            elog_alert(0,
                    "The supplied parameter 'coordinate_scale_factor' value of %d is greater than 1000, and will cause loss of precision for arcsecond coordinates.",
                    coordScale);
        }
    }

	/* trace_gain_type: signed int */
	int16_t trace_gain_type = pfget_int(pf,"trace_gain_type");
	if (trace_gain_type < 0)
	{
		die(0, "The trace_gain_type must be zero or greater");
	}
	else
	{
		trace_gain_type=htons(trace_gain_type);
	}


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
		elog_complain(1,"Working database view is empty\n");
		if(substr!=NULL) elog_complain(0,"Subset condtion =%s a likely problem\n",
				substr);
		usage();
	}

	fp = fopen(outfile,"w");
	if(fp == NULL)
	{
		elog_complain(0,"Cannot open output file %s\n",outfile);
		usage();
	}

	/* These are needed for sort below */
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"chan");

    /* Set up and write the Textual File Header */
	initialize_text_header(text_file_header, segy_format,
			text_header_description);

	if(write_reel_headers){
		if ( fwrite(text_file_header,1,SEGY_TEXT_HEADER_SIZE,fp) \
				!= SEGY_TEXT_HEADER_SIZE ) {
			elog_die(1,"An error occurred writing the textual file header");
		}
	}

	/* memory allocation for trace data.  This is a large matrix
	that is cleared for each event.  This model works because of
	segy's fixed length format.*/
	traces = calloc(nchan, sizeof(float*));
	if(traces == NULL)
		elog_die(1,"out of memory");
	for (int r = 0; r < nchan; r++)
	{
		traces[r] = calloc(nsamp0, sizeof(float));
		if(traces[r] == NULL)
			elog_die(1,"out of memory");
	}
	header = (SEGYTraceHeader *)calloc((size_t)nchan,sizeof(SEGYTraceHeader));
	if(header == NULL)
			elog_die(0,"Cannot alloc memory for %d segy header workspace\n",nchan);
	if(write_reel_headers)
	{
		if (Verbose) {
			elog_debug(0,"Binary Headers - Using segy_format code 0x%04X\n", ntohs(segy_format));
		}
		initialize_binary_file_header(&reel, segy_format);

		/* now fill in the binary reel header and write it */
		reel.kjob   = htonl(1);
		reel.kline  = htonl(1);
		reel.kreel  = htonl(1);
		reel.kntr   = htons((int16_t)nchan);
		reel.knaux  = htons(0);
		reel.sr     = htons((int16_t)(1000000.0/samprate0));
		reel.kfldsr = reel.sr;
		reel.knsamp = htons((int16_t)nsamp0);
		reel.kfsamp = htons((int16_t)nsamp0);
		reel.dsfc   = htons(5);  /* This is ieee floats*/
		reel.kmfold = htons(0);
		if(map_to_cdp)
			reel.ksort = htons(2);
		else
			reel.ksort = htons(1);
		reel.kunits = htons(1);  /* This sets units to always be meters */

		if(fwrite((void *)(&reel),sizeof(SEGYBinaryFileHeader),1,fp) != 1)
		{
			elog_die(1,"Write error for binary reel header");
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
			elog_notify(0,"Processing:  %s\n",s);
		for(i=0;i<nchan;++i)
		{
			initialize_trace_header(&(header[i]), segy_format);
			header[i].gainType = trace_gain_type;
			header[i].lineSeq = htonl(total_traces + i + 1);
			header[i].reelSeq = header[i].lineSeq;
			if(map_to_cdp)
			{
				header[i].cdpEns = htonl(i + 1);
				header[i].traceInEnsemble = htonl(1);/* 1 trace per cdp faked */
			}
			else
			{
				header[i].channel_number = htonl(i + 1);
			}
			header[i].event_number   = htonl(shotid);
			header[i].energySourcePt = htonl(shotid);
			for(j=0;j<nsamp0;++j)  traces[i][j] = htonf((Trsample)0.0);
		}
		if(input_source_coordinates)
		{
			char stmp[40];
			sscanf(s,"%s%ld%lf%lf%lf",stmp,&shotid,&slon,&slat,&selev);
			time0=str2epoch(stmp);
			if(coordUnits == SEGY_TRACE_COORDUNITS_ARCSECONDS) {
				slat*=3600.0;
				slon*=3600.0;
			}
			slat *= (double)coordScale;
			slon *= (double)coordScale;
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
			  elog_notify(0,"trload_css failed for shotid=%ld",shotid);
			  elog_notify(0,"  No data in time range %s to %s\n",
			  	strtime(time0),strtime(endtime0) );
			  elog_notify(0,"No data written for this shotid block.");
			  elog_notify(0,"  Handle this carefully in geometry definitions.\n");
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
			/* This is need to prevent collisions of channel names */
			trdbss = dbsubset(trdb,trsubset,0);
			if(trrotate(trdbss,phi,theta,newchan))
				elog_notify(0,"Data loss in trrotate for event %s to %s\n",
					stime, etime);
		}
		if(Verbose)
			elog_notify(0,"Station  chan_name  chan_number seq_number shotid  evid\n");
		trdb = dbsort(trdb,sortkeys,0,0);
		dbquery(trdb,dbRECORD_COUNT,&ntraces);
		if(Verbose) elog_debug(0,"Read %ld traces for event at time%s\n",
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
			    "segtype",&segtype,
			    NULL) == dbINVALID)
			{
				elog_complain(0," dbgetv error reading record %ld. Trace will be skipped for station %s and channel %s",
				trdb.record,sta,chan);
				continue;
			}
			/* Allow 1 percent samprate error before killing */
			double fsrskew=fabs((samprate-samprate0)/samprate0);
			double frskewcut=0.01;
			if(fsrskew>frskewcut)
			{
				elog_complain(0,"%s:%s sample rate %f is significantly different from base sample rate of %f. Trace skipped -- segy requires fixed sample rates",
					sta,chan,samprate,samprate0);
				continue;
			}
			if(nsamp > nsamp0)
			{
				elog_complain(0,"%s:%s trace has extra samples=%ld. Truncated to length %d",
					sta, chan, nsamp, nsamp0);
				nsamp = nsamp0;
			}
			else if(nsamp < nsamp0)
			{
				elog_complain(0,"%s:%s trace is shorter than expected %d samples. Zero padded after sample %ld",
					sta, chan, nsamp0, nsamp);
			}

			ichan = get_channel_index(channels,sta,chan);
			if(ichan > nchan)
			{
				elog_die(0,"Channel index %d outside limit of %d. Cannot continue",
					ichan, nchan);
			}
			if(ichan >= 0)
			{
				if(Verbose)
					elog_debug(0,"%s:%s\t%-d\t%-d\t%-ld\t%-ld\n",
					sta,chan,ichan+1,
					ntohl(header[ichan].reelSeq),
					shotid, evid);
				header[ichan].traceID = get_trace_id_code_from_segtype(segtype);
				for(j=0;j<nsamp;++j) {
				   traces[ichan][j] = htonf((float)trdata[j]);
				}
				/* header fields coming from trace table */
				header[ichan].samp_rate = htonl(
						(int32_t) (1000000.0/samprate0));
				/* according to the behavior specified in the man page:
				 * if use_geo_coordinates is false:
				 * - coordUnits is length (meters)
				 * - therefore, we use deast for X and dnorth for Y
				 * if use_geo_coordinates is true:
				 * - we're using either arcseconds or decimal degrees
				 * - and therefore, we use lon for X and lat for Y
				 *
				 * coordUnits is based on use_arcseconds and the requested
				 * version of segY */

				/* set the coordinate units in the trace header */
				header[ichan].coordUnits = coordUnits;

				/* Pick the source db fields for our receiver X and Y */
				double recLongOrX = 0;
				double recLatOrY  = 0;
				if (coordUnits == SEGY_TRACE_COORDUNITS_LENGTH) {
					/* Use deast and dnorth
					 * CSS3.0 Schema specifies deast and dnorth are in KM.
					 * SEG-Y specifies easting and northing as meters,
					 * hence the 1000.0 multiplier here. */
					recLongOrX = deast  * 1000.0;
					recLatOrY  = dnorth * 1000.0;
				} else if (coordUnits == SEGY_TRACE_COORDUNITS_ARCSECONDS){
					/* Use lat and lon, converted to arcseconds */
					recLongOrX = lon * 3600.0;
					recLatOrY  = lat * 3600.0;
				} else {
					/* Default case, which covers decimal degrees */
					recLongOrX = lon;
					recLatOrY  = lat;
				}

				/* Apply our coordScale - the user can specify negative numbers,
				 * but they are treated as inverting the value, not as a divisor
				 * as in the SEG-Y field usage. See below where we always treat
				 * the scalar as a divisor in the SEG-Y field */
				recLongOrX *= (double)coordScale;
				recLatOrY  *= (double)coordScale;

				/* Set the coordScale in the header.
				 * Note negative here.  This is a oddity of segy that - means
				 * divide by this to get actual.  Always make this negative in
				 * case user inputs a negative number.
				 * Don't set it -1 for cosmetic reasons */
				if (abs(coordScale) == 1)
				{
					header[ichan].coordScale = htons(1);
				} else
				{
					header[ichan].coordScale = htons(-abs(coordScale));
				}

				/* Finally, write out the X and Y */
				header[ichan].recLongOrX
					= htonl((int32_t)recLongOrX);
				header[ichan].recLatOrY
					= htonl((int32_t)recLatOrY);

				/* CSS3.0 specfies elev as being in km, SEG-Y wants it in m */
				header[ichan].recElevation = htonl((int32_t)(elev*1000.0));

				header[ichan].deltaSample = htons(
						(int16_t) (1000000.0/samprate0));
				header[ichan].sampleLength = htons((int16_t)nsamp0);
				if (ntohs(segy_format)<0x0100)
				{
					header[ichan].num_samps = htonl((int32_t)nsamp0);
				}
				/* This cracks the time fields */
				time_str = epoch2str(time0,fmt);
				int16_t hyear, hday, hhour, hminute, hsecond, hm_secs;
				hyear=hday=hhour=hminute=hsecond=hm_secs=0;
				sscanf(time_str,"%hd %hd %hd %hd %hd %hd",
						&hyear, &hday, &hhour, &hminute, &hsecond, &hm_secs);
				header[ichan].year   = htons(hyear);
				header[ichan].day    = htons(hday);
				header[ichan].hour   = htons(hhour);
				header[ichan].minute = htons(hminute);
				header[ichan].second = htons(hsecond);
				header[ichan].m_secs = htons(hm_secs);
				if (ntohs(segy_format)<0x0100)
				{
					/* These are IRIS-PASSCAL extensions */
					header[ichan].trigyear   = header[ichan].year;
					header[ichan].trigday    = header[ichan].day;
					header[ichan].trighour   = header[ichan].hour;
					header[ichan].trigminute = header[ichan].minute;
					header[ichan].trigsecond = header[ichan].second;
				}
				free(time_str);
				if(input_source_coordinates)
				{
					/* Write out our pre-scaled and optionally
					 * arcsecond-converted source lat/lon plus our elevation */
					header[ichan].sourceLongOrX = htonl((int32_t)slon);
					header[ichan].sourceLatOrY  = htonl((int32_t)slat);
					header[ichan].sourceSurfaceElevation
						= htonl((int32_t)selev);
					/* No easy way to specify both elev and depth*/
					header[ichan].sourceDepth=htonl(0);
				}
				else if(map_to_cdp)
				{
				/* When faking CDP data we make this look
				like a zero offset, single fold data set */
					header[ichan].sourceLongOrX   = header[ichan].recLongOrX;
					header[ichan].sourceLatOrY    = header[ichan].recLatOrY;
					header[ichan].sourceSurfaceElevation
					                              = header[ichan].recElevation;
					header[ichan].sourceDepth     = htonl(0);
					header[ichan].sourceToRecDist = htonl(0);
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
					elog_notify(0,"Station %s and channel %s skipped\n",
						sta,chan);
			}

		}
		/* Now we write the data */
		for(i=0;i<nchan;++i)
		{
			if(fwrite((void *)(&(header[i])),sizeof(SEGYTraceHeader),1,fp) != 1)
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
