#include "session_manager.h"

using namespace std;
using namespace SEISPP;

SessionManager::SessionManager(string pfname, string hname, string lname, string wdbname, string rdbname)
{
    int i;
    pf_name=pfname;
    log_file_name=lname;
    waveform_db_name=wdbname;
    result_db_name=rdbname;


    parent=NULL;
    /* new feature June 2007.  Get a match handle on orid to feed processing */
    DatascopeHandle dbhw(wdbname,false);
    dbhw.lookup("event");
    dbhw.natural_join("origin");
    list<string>matchkey;
    matchkey.push_back("orid");
    dbh=DatascopeMatchHandle(dbhw,string(""),matchkey,AttributeMap("css3.0"));

    controls=new Widget[MAX_NUM_CONTROLS];
    sensitive=new bool[MAX_NUM_CONTROLS];
    for(i=0; i<MAX_NUM_CONTROLS; i++) {
	sensitive[i]=false; //assume all buttons are diabled
	controls[i]=NULL;
    }
    try {
	log_stream.open(lname.c_str(),ios::out | ios::trunc);
    } catch (ios::failure& var)
    {
	string mess;
	mess=var.what();
	throw SeisppError(string("SessionManager constructor:  open failed on log file=")
		+ lname
		+string("\nSystem message:\n")
		+ mess);
    }
    try {
	instream.open(hname.c_str(),ios::in);
    } catch (ios::failure& var)
    {
	string mess;
	mess=var.what();
	throw SeisppError(string("SessionManager constructor:  open failed on control stream=")
		+ hname
		+string("\nSystem message:\n")
		+ mess);
    }

    xpe=NULL;
    mcc=NULL;

    state=START;
    previous_state=START;
    display_initial_sort_box=true;
    display_analysis_sort_box=true;
    evid=-1;
    orid=-1;
    using_subarrays=false;
    // Added July 2007 to produce pick markers on beam plot
    Pf *pf;
    if(pfread(const_cast<char *>(pfname.c_str()),&pf)) 
	throw SeisppError(string("session_manager(constructor):  pfread failed on ")
			+pfname);
    Metadata smcontrol(pf);
    try {
	bool turn_off_popups=smcontrol.get_bool("turn_off_informational_popups");
	if(turn_off_popups)
	{
		display_initial_sort_box=false;
		display_analysis_sort_box=false;
	}
	beammarkers.beam_color=smcontrol.get_string("beam_arrival_pick_marker_color");
	beammarkers.robust_color=smcontrol.get_string("beam_arrival_error_marker_color");
	beammarkers.title=smcontrol.get_string("beam_plot_title");
    }
    catch (MetadataGetError mde)
    {
	mde.log_error();
	throw SeisppError("Error getting beam window setup parameters");
    }
    Tbl *t;
    t=pfget_tbl(pf,"filters");
    if(t==NULL)
    {
		throw SeisppError("pf error:  required parameter filters &Tbl missing");
    }
    /* Filter 0 is always tagged as default.  We initialize it here to DEMEAN
    and make it the current filter for initialization. */
    available_filters.push_back(TimeInvariantFilter(string("DEMEAN")));
    string label0("default");
    filter_labels.push_back(label0);
    filter_index[label0]=0;
    current_filter_index=0;
    char *line;
    const string white(" \t\n");
    int current,next;
    int end_current;
    string slabel,sdesc;
    for(i=0;i<maxtbl(t);++i)
    {
	line=(char *)gettbl(t,i);
	string sline(line);
	/* skip any leading white space */
	if(current=sline.find_first_not_of(white,0) != 0)
	{
		sline.erase(0,current);
		current=0;
	}
	end_current=sline.find_first_of(white,current);
	slabel.assign(sline,current,end_current-current);
	current = sline.find_first_not_of(white,end_current);
	sdesc.assign(sline,current,sline.length()-1);
	available_filters.push_back(TimeInvariantFilter(sdesc));
	filter_labels.push_back(string(slabel));
	filter_index[slabel]=i+1;
    }
    freetbl(t,0);
    pffree(pf);
    /* These we intentionally do not set in the pf */
    beammarkers.beam_tw=TimeWindow(0.0,0.0);  // set to 0 time
    beammarkers.robust_tw=TimeWindow(-0.1,0.1);  // magic numbers could be const variable
    sort_reverse=false;
}

SessionManager::~SessionManager()
{
    delete controls;
    delete sensitive;
 
    dbh.close();
    if (xpe != NULL) delete xpe;  //believe that deletion of xpe will result in deletion of the last mcc
}

void SessionManager::record( string s)
{
    XmTextInsert(msg_w,XmTextGetLastPosition(msg_w),(char *)(s.c_str()));
    XClearWindow(XtDisplay(msg_w),XtWindow(msg_w));
    log_stream << s <<endl;
}

void SessionManager::session_state()
{
    switch(state) {
        case START:
	    state=NONE;
	    break;
	case NONE:
	    state=NEXT_EVENT;
	    break;
        case NEXT_EVENT:
	    if(using_subarrays)
	    	state=NEXT_SUBARRAY;
	    else
		state=REF;
	    break;
	case NEXT_SUBARRAY:
	    state=REF;
	    break;
	case REF:
	    state=ANALYZE;
	    break;
	case ANALYZE:
	    if(using_subarrays)
		state=NEXT_SUBARRAY;
	    else
	    	state=NEXT_EVENT;
    	    break;
	default:
	    break;
    }
    session_state(state);
}

void SessionManager::session_state(SessionState s)
{
    int i;

    previous_state=state;
    state=s;

    // Whenever we enter this routine all gui items are first disabled
    // then we only turn on the one's appropriate for a given state
    for(int i=0; i<MAX_NUM_CONTROLS; i++) {
        sensitive[i]=false; 
    }
   // These are buttons on the beam window.  They are never disabled
    sensitive[BTN_ARRIVAL]=true;
    sensitive[BTN_ARRIVAL_ERROR]=true;

    switch (s) {
	case NONE: 
	    sensitive[BTN_NEXTEV]=true;    
	    sensitive[MENU_FILE]=true;
	    sensitive[MENU_FILE_EXIT]=true;
/*
 	    sensitive[MENU_PICKS]=true;
 	    sensitive[MENU_OPTIONS_FILTER]=true;
 	    sensitive[MENU_OPTIONS_SORT]=true;
 	    sensitive[MENU_VIEW]=true;
*/
	    break;
	case NEXT_EVENT:
	    sensitive[BTN_NEXTEV]=true;
	    sensitive[BTN_REF]=true;
	    sensitive[BTN_RESTORE]=true;
	    sensitive[BTN_SUBON]=true;
	    sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
 	    sensitive[MENU_PICKS]=true;
	    sensitive[MENU_PICKS_BWIN]=true;
	    sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[BTN_PICKS_TEDIT]=true;
	    sensitive[BTN_PICK_CUTOFF]=true;
	    sensitive[MENU_PICKS_VIEW]=true;
  	    sensitive[MENU_PICKS_VIEW_ATTR]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    break;
	case NEXT_SUBARRAY:
	    sensitive[BTN_NEXTSUB]=true;
	    sensitive[BTN_REF]=true;
	    sensitive[BTN_RESTORE]=true;
	    sensitive[BTN_SUBON]=true;
	    sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
 	    sensitive[MENU_PICKS]=true;
	    sensitive[MENU_PICKS_BWIN]=true;
	    sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[BTN_PICKS_TEDIT]=true;
	    sensitive[BTN_PICK_CUTOFF]=true;
	    sensitive[MENU_PICKS_VIEW]=true;
  	    sensitive[MENU_PICKS_VIEW_ATTR]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
            sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    break;
	case REF:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
            sensitive[BTN_REF]=true;
            sensitive[BTN_ANALYZE]=true;
	    sensitive[BTN_RESTORE]=true;
	    sensitive[BTN_SUBON]=true;
            sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_PICKS_VIEW]=true;
	    sensitive[MENU_PICKS_VIEW_ATTR]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
            sensitive[MENU_VIEW]=true;
            sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    break;
	case ANALYZE:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
            sensitive[BTN_REF]=true;
            sensitive[BTN_ANALYZE]=true;
            sensitive[BTN_BEAM_PLOT]=true;
            sensitive[BTN_XCOR_PLOT]=true;
	    sensitive[BTN_RESTORE]=true;
	    sensitive[BTN_SUBON]=true;
            sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_PICKS_VIEW]=true;
            sensitive[MENU_PICKS_VIEW_ATTR]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
            sensitive[MENU_VIEW]=true;
            sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
            sensitive[MENU_VIEW_COHERENCE]=true;
	    sensitive[MENU_VIEW_PCORRELATION]=true;
	    sensitive[MENU_VIEW_SWEIGHT]=true;
	    sensitive[MENU_VIEW_SNR]=true;
	    break;
  	case SAVE:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
 	    sensitive[BTN_FILE_SAVE]=true;
            sensitive[BTN_REF]=true;
            sensitive[BTN_ANALYZE]=true;
            sensitive[BTN_BEAM_PLOT]=true;
            sensitive[BTN_XCOR_PLOT]=true;
	    sensitive[BTN_RESTORE]=true;
	    sensitive[BTN_SUBON]=true;
            sensitive[MENU_FILE]=true;
 	    sensitive[MENU_FILE_SAVE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_PICKS_VIEW]=true;
            sensitive[MENU_PICKS_VIEW_ATTR]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
            sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
            sensitive[MENU_VIEW_COHERENCE]=true;
            sensitive[MENU_VIEW_PCORRELATION]=true;
            sensitive[MENU_VIEW_SWEIGHT]=true;
	    sensitive[MENU_VIEW_SNR]=true;
	    break;
	case TRACE_EDIT:
	    sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	case PICKING_CUTOFF:
	    sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	case THINKING:
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	default:
	    break;
    }

    for(i=0; i<MAX_NUM_CONTROLS; i++) {
	if (controls[i]!=NULL) {
	    XtSetSensitive(controls[i],sensitive[i]);
	}
    }

}
void SessionManager::restore_previous_state()
{
	state=previous_state;
	session_state(state);
}


bool SessionManager::validate_setting(stringstream & ss)
{
    string sort_order;
    int scase=0;

    switch (active_setting.result_sort_order) {
	case COHERENCE:
	    sort_order="coherence";
	    scase=1;
	    break;
	case CORRELATION_PEAK:
	    sort_order="peak correlation";
	    scase=1;
	    break;
	case AMPLITUDE:
	    sort_order="amplitude";
	    scase=1;
	    break;
	case LAG:
	    sort_order="lag";
	    scase=1;
	    break;
	case WEIGHT:
	    sort_order="stack weight";
	    scase=1;
	    break;
	case SITE_LAT:
	    sort_order="site.lat";
	    scase=0;
	    break;
	case SITE_LON:
	    sort_order="site.lon";
	    scase=0;
	    break;
	case PREDARR_TIME:
	    sort_order="predicted time";
	    scase=0;
	    break;
	case SNR:
	    sort_order="signal to noise ratio";
	    scase=1;
	    break;
	default:
	    break;
    }

    if(scase==1 && state != ANALYZE && state != REF) {
	active_setting.result_sort_order=PREDARR_TIME;
	ss << "You specified an initial sort order of "<<sort_order<<endl<<
	    " which is not available before the analysis"<<endl<<
	    " of the ensemble, "
	    <<"the sort order is reset to "<<endl<<"site.lat"<<endl;
        if (display_initial_sort_box) return false;
    } else if (scase==0 && (state==ANALYZE || state==REF)) {
	active_setting.result_sort_order=WEIGHT;
	ss << "The sort order for the analysis result is set automatically "
	   << endl <<"to stack weight from the specified "<<sort_order<<endl;
	if (display_analysis_sort_box) return false;
    }
 
    return true;
}
/* These are needed to deal with CSS3.0 database ids. */
void SessionManager::set_evid(int event_id)
{
	evid=event_id;;
}
void SessionManager::set_orid(int origin_id)
{
	orid=origin_id;;
}
void SessionManager::set_hypo(Hypocenter& h)
{
	hypo=h;
}
void SessionManager::set_phase(string ph)
{
	current_phase=ph;
}
/* New Oct. 2007 to implement a decent filter menu */
TimeInvariantFilter SessionManager::get_filter(string name)
{
	map<string,int>::iterator fptr;
	fptr=filter_index.find(name);
	if(fptr==filter_index.end())
	{
		/* Assume this is always defined.  We don't
		test for this error condition intentionally.
		Beware if ported.*/
		string key("default");
		fptr=filter_index.find(key);
	}
	int i=fptr->second;
	return(available_filters[i]);
}
TimeInvariantFilter SessionManager::get_filter(int ifilt)
{
	if( (ifilt<0) || (ifilt>=available_filters.size()) )
	{
		throw SeisppError(string("SessionManager::get_filter(int)")
			+ string(" filter index number requested is out of range") );
	}
	return(available_filters[ifilt]);
}
TimeInvariantFilter SessionManager::get_filter()
{
	return(available_filters[current_filter_index]);
}
int SessionManager::current_filter()
{
	return(current_filter_index);
}
	
int SessionManager::number_filters()
{
	return(available_filters.size());
}
string SessionManager::filter_description(int ifilt)
{
	if( (ifilt<0) || (ifilt>=available_filters.size()) )
	{
		throw SeisppError(string("SessionManager::filter_description(int)")
			+ string(" filter index number requested is out of range") );
	}
	TimeInvariantFilter filt=get_filter(ifilt);
	return(filt.type_description(false));
}
string SessionManager::filter_description()
{
	TimeInvariantFilter filt=available_filters[current_filter_index];
	return(filt.type_description(false));
}
/* This method sets the current filter index using a name key.
Note it is bombproof.  If the name is not known it returns the
default filter */
void SessionManager::set_filter(string name)
{
	map<string,int>::iterator fptr;
	fptr=filter_index.find(name);
	if(fptr==filter_index.end())
	{
		/* Assume this is always defined.  We don't
		test for this error condition intentionally.
		Beware if ported.*/
		string key("default");
		fptr=filter_index.find(key);
	}
	current_filter_index=fptr->second;
}
/* Similar to above, but using an integer index.  This method is 
also bombproof silently setting the filter to default if out of 
range.  Note this is a bit of a potential maintenance issue.  This
assumes element 0 is always set as the default filter.  */
void SessionManager::set_filter(int ifilt)
{
	if( (ifilt<0) || (ifilt>=available_filters.size()) )
		current_filter_index=0;
	else
		current_filter_index=ifilt;
}
/* Changes the filter associated with the key name.  If the name is 
not found, this filter is appended to the set of available filters. */
void SessionManager::modify_filter(string name,TimeInvariantFilter& filt)
{
	map<string,int>::iterator fptr;
	fptr=filter_index.find(name);
        if(fptr==filter_index.end())
        {
		int ind=available_filters.size();
		filter_index[name]=ind;
		available_filters.push_back(filt);
	}
	else
	{
		available_filters[fptr->second]=filt;
	}
}
