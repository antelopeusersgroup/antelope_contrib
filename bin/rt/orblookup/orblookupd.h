#ifndef ORBLOOKUPD_H 
#define ORBLOOKUPD_H

#define UPDATE_DEFAULT 5
#define PACKET_SIZE 1024

#define LATENCY   "latency"
#define CHANNAME  "channame"
#define CHANTYPE  "chantype"
#define LABEL     "label"
#define DIVFACTOR "divby"
#define HIGH      "high"
#define LOW       "low"
#define ALL       "all"
#define NAME      "name"
#define PRIORITY  "priority"
#define ALERTCHAN "alert"
#define STYLE     "style"
#define UNITS     "units"
#define NUMDIGITS "decdigits"
#define NUMCOLS   "numcols"
#define ORBLIST   "orblist"
#define ORBNAME   "orbname"
#define ORBFILE   "orbfile" 
#define ORDER     "order"
#define OFFSET    "offset"
#define DBNAME    "dbname"
#define STALIST   "stationlist"
#define MINNETLATENCY   "minnetlat"
#define MEDIANNETLATENCY "mediannetlat"
#define MAXNETLATENCY   "maxnetlat"
#define ALERT_STAGE     "alert_stage"
#define ROW_SEPERATOR   "row_seperator"
#define FILENAME        "filename"
#define ISNEW           "isnew"
#define THRESHOLD       "threshold"
#define FIELDS          "fields"
#define DISABLED_FIELDS "disabled_fields"

#define PACKETS         "packets"
#define ALERT_DESC      "alert_desc"
#define ALERT_STAGES    "alert_stages"
#define ALERT_RANGES    "alert_stage_ranges"
#define ROW_NUM_CLASS   "rownumclass"
#define FIELD_LIST      "fieldlist"
#define FIELD_CLASS     "fieldclass"
#define FIELD_VALUE     "fieldvalue"
#define FIELD_ALIGN     "fieldalign"
#define CHECKED_STATUS  "checkedstatus"
#define FILEDES         "filedes"
#define SCRIPT_NAME     "scriptname"
#define PERCENTWIDTH    "percentcolwidth"
#define SLIST           "slist"
#define NUMFIELDS       "numfields"
#define NUMCHANS        "numchans"
#define NUMNETS         "num_nets"
#define NUMSTAS         "numstas"
#define NUMSTASEXP      "numstas_expected"
#define FULLNET         "fullnet"
#define TIME            "time"
#define UPDATE          "update"
#define NETWORKS        "networks"
#define LATENCY_ORDER   "latency_order"

int verbosity;
pthread_mutex_t fileaccess;

typedef struct orbinfo {
    Arr * fields;
    Arr * supported_pkts;
    Arr * disabled_fields;
    Arr * alert_stages;
    Arr * alert_ranges;
    Arr * latency_order;
    char * dbname;
    char * name;
    char * file;
    char * cachefile;
    int update_mins;
    double lasttime;
} orbinfo;
        
void * getfrom_pfstruct( Pf * pf );
void * getpf( Arr * arr, char * index );
void * pf_collapse( Pf * pf );
Pf * create_pf ( void * value, int type );

int clrarr( Arr * arr, void (*free_value )() );
void recurse_free_pf ( Pf * pf );

char * ftoa ( double d, int precision );
char * itoa ( int i );
char * stradd( char * s1, char * s2 );

char * get_station_field( Arr * sinfo, char * name );
int set_station_field( Arr * sinfo, char * name, char * value );
int sinfo_update ( Arr * sold, Arr * snew );
int get_priority( char * alert_name, orbinfo * oi );
int is_station_new( Arr * sinfo );
Arr * get_network( Arr * networks, char * net );
Arr * get_stations ( Arr * networks, char * net );
int add_network ( Arr * networks, char * net );
Arr * get_station ( Arr * networks, char * net, char * sta );
int add_station ( Arr * networks, char * net, char * sta, Arr * sinfo );
int set_fullnetname( Arr * networks, char * net, char * name );
char * calc_latency ( double time1, double time2 );
int add_latencies ( Arr * networks );
int set_sinfo_limits( Arr * sinfo, orbinfo * oi, char * pkttype );

char * extract_sta ( char * srcname );
char * extract_net ( char * srcname );
char * extract_pkttype ( char * srcname );

Arr * create_new_sinfo( char * name );
Arr * sinfo_init ( char * pkttype, char * name, orbinfo * oi, char * priority );

Arr * get_network_names ( Arr * networks, orbinfo * oi );
Arr * check_station_db ( Arr * networks, orbinfo * oi );
Arr * get_orbnames();

int extract_pktinfo ( Arr * sinfo, Packet * pkt, char * pkttype, 
                      Arr * fieldsarr, Arr * disabled_fields, int numfields );
int cache_results( char * cachefile, Arr * networks, char * orbfilename, 
                   double cachetime );
Arr * get_orbdata ( orbinfo * oi );
void orbthread( orbinfo * oi );
void orbtimer( orbinfo * oi );

#endif
