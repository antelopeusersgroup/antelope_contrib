#ifndef IGPPRT_ANZA_H 
#define IGPPRT_ANZA_H

#define BADVAL -9999
#define EVENROW "evenrow"
#define ODDROW "oddrow"
#define UPDATE_DEFAULT 5
#define PACKET_SIZE 1024

#define LATENCY "latency"
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
#define NOSTA     "station_not_present_style"
#define NEWSTA    "new_station_style"
#define STYLES    "css_class_list"
#define NONET_BLURB     "Station has not reported."
#define NEWNET_BLURB    "Station has not been added to the database."
#define STALIST   "stationlist"
#define MINNETLATENCY   "minnetlat"
#define MEDIANNETLATENCY "mediannetlat"
#define MAXNETLATENCY   "maxnetlat"
#define ALERT_STAGE     "alert_stage"
#define ROW_SEPERATOR   "row_seperator"
#define FILENAME        "filename"
#define ISNEW           "isnew"
#define ZEROSTR         "0"
#define ONESTR          "1"
#define THRESHOLD       "threshold"
#define FIELDS          "fields"
#define DISABLED_FIELDS "disabled_fields"
#define NBSP            "&nbsp;"
#define UPDATENOW       "updatenow"
#define UPDATE          "update"
#define ALERTMODE       "alertmode"
#define PACKETS         "packets"
#define STACLASS        "staclass"
#define NETCLASS        "netclass"
#define TIME            "time"
#define ENCODEDNET      "encodednet"
#define ENCODEDNETSTA   "encodednetsta"
#define ALERT_CLASS     "alert_class"
#define ALERT_DESC      "alert_desc"
#define ALERT_STAGES    "alert_stages"
#define ALERT_RANGES    "alert_stage_ranges"
#define ROW_NUM_CLASS   "rownumclass"
#define FIELD_LIST      "fieldlist"
#define FIELD_CLASS     "fieldclass"
#define FIELD_VALUE     "fieldvalue"
#define FIELD_ALIGN     "fieldalign"
#define CHECKED_STATUS  "checkedstatus"
#define FILEDES "filedes"
#define SCRIPT_NAME     "scriptname"
#define PERCENTWIDTH    "percentcolwidth"
#define INDENT  "indent"
#define CURRENT "current"
#define VARS    "vars"
#define LIST    "list"
#define FP      "fp"
#define NET     "net"
#define STA     "sta"
#define SLIST   "slist"
#define ORBINFO "orbinfo"
#define INDEX   "index"
#define MAXINDEX        "maxindex"
#define USED_FIELD      "used_field"
#define ALL_FIELD       "all_field"
#define NUMUSEDFIELDS   "num_used_fields"
#define NUMFIELDS       "numfields"
#define NUMNETS         "num_nets"
#define NUMSTAS         "numstas"
#define NUMSTASEXP      "numstas_expected"
#define NUMSTASTOT      "numstastot"
#define NUMCHANSTOT      "numchanstot"
#define ALERT   "alert"
#define NETURL  "url"
#define NETWORKS "networks"
#define ORBURL  "orburl"
#define QUERY   "query"
#define ORB     "orb"
#define FULLNET "fullnet"
#define NUMCHANS "numchans"
#define CACHEFILE       "cachefile"


Arr * html_template;
Arr * styles;
char * currentnet;
Arr * orblist;
Arr * orbinfo;
double cachetime;
int alertmode;

typedef char* ( *TagValFP ) (Arr*);
typedef Arr*  ( *TagListFP) (Arr*);
typedef int   ( *AlertLevelFP ) (Arr*);

int get_staclasslevel ( Arr* );
int get_netclasslevel ( Arr* );
char * get_alert_name ( int priority );
int print_template_info( Arr*, char*, int );
char * select_groupstyle( Arr* );
char * calc_latency ( double time1, double time2 );
char * extract_netsta( char * srcname );
Arr * generate_headers( Tbl * field_list );
char * generate_field ( Arr * sinfo, char * field );
int generate_html( Arr * slist, Tbl * field_list );
int generate_system_error_page();
int generate_error_page();
int generate_message_page( char *, char* );
int get_numcols();
Tbl * create_field_list ( char *, long, char ** );

char * encode_uri ( char *, int );
char * decode_uri ( char * );

#endif
