typedef struct association_ {
	int arid;
	char sta[32];
	char chan[32];
	char iphase[32];
	char timedef[32];
	double time;
	double timeres;
	double delta;
	double seaz;
	double esaz;
} Association;

typedef struct hypocenter_ {
	int orid;
	int evid;
	int nass;
	int ndef;
	double time;
	double lat;
	double lon;
	double depth;
	char auth[64];
	int assocs_size;
	Association *assocs;
} ORB_Hypocenter;

typedef struct RTlocate_Options
{
	/* Set range of epicentral distances from center point
	to be examined by this instance of orbgenloc */
	double minimum_distance, maximum_distance;
	/* These locate the master database we mostly use to get
	orid from */
	char *work_db;
	/* logfile is written in this directory */
	char *logdir;
	/* This is a subdir below logdir that solution failures 
	are written to. i.e. results are in logdir/failure_sdir */
	char *failure_sdir;
	/* special timeout parameter used in input of db records 
	orb_arrivals_in -- it is the number of db records to skip
	before resetting the algorithm -- needed to avoid an 
	infinite loop.  It is a network tuneable parameter that should
	be set the the number of stations */
	int db_record_skip_timeout;
	
} RTlocate_Options;
int orb_arrivals_in(int, Dbptr, ORB_Hypocenter *, int *, RTlocate_Options);
Tbl *orbhypo_to_genloc(ORB_Hypocenter *, Arr *, Arr *);

