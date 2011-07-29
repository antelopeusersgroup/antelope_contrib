#include <algorithm>
#include "Metadata.h"
#include "session_manager.h"

using namespace std;
using namespace SEISPP;
void constructor_warning(string attribute)
{
	cerr << "Warning (session_manager constructor):  "
	  << "plot_title_mdl requests attribute="<<attribute
	  << " but this attribute is not in ensemble_mdl"
	  << " or is not a legal type for a title"
          <<endl
	  <<"This attribute will not be displayed.  "
	  <<"Edit parameter file to fix this problem"
	  <<endl;
}

/* Similar to a find() algorithm but done with a stupid linear search.
Needed because MetadataList does not have required operators to use the generic algorithm.
Not much of an issue as these lists can never be a large enough that a linear search is a problem.
Returns true if the tag of mptr is found in mdl. */
bool mdlcheck(MetadataList& mdl, MetadataList::iterator mptr)
{
	MetadataList::iterator mloop;
	for(mloop=mdl.begin();mloop!=mdl.end();++mloop)
	{
		if(mloop->tag == mptr->tag) return true;
	}
	return false;
}

SessionManager::SessionManager(string pfname, string hname, string lname, string wdbname, string rdbname)
{
    int i;
    pf_name=pfname;
    log_file_name=lname;
    waveform_db_name=wdbname;
    result_db_name=rdbname;


    parent=NULL;
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
    try {
	/* First extract these entities using pf interface.  Not elegant to 
	mix these, but simplifies this application. */
	MetadataList ensemble_mdl=pfget_mdlist(pf,"ensemble_mdl");
	MetadataList title_mdl=pfget_mdlist(pf,"plot_title_mdl");
	MetadataList::iterator tptr;
	for(tptr=title_mdl.begin();tptr!=title_mdl.end();++tptr)
	{
		if(mdlcheck(ensemble_mdl,tptr))
		{
			switch(tptr->mdt)
			{
			case MDstring:
				generic_title_strings.push_back(tptr->tag);
				break;
			case MDint:
				generic_title_ints.push_back(tptr->tag);
				break;
			case MDreal:
				generic_title_reals.push_back(tptr->tag);
				break;
			default:
				constructor_warning(tptr->tag);
			};
		}
		else
		{
			constructor_warning(tptr->tag);
		}
	}
	Metadata smcontrol(pf);
	/* Initialize dbh as default.  This REQUIRES that it be set
	later using a handle acquired from XcorProcessingEngine.
	This proved necesarry in a reluctant change to this required
	April 2008.  Datascope is a bit too clever and when a new
	database is openned it is assigned the same database number
	even if it had been previously openned elsewhere.  This broke
	the memory management model.  As a result I was forced
	to this approach here.  This has important maintenance side
	effects that are unavoidable. */
	dbh=DatascopeMatchHandle();

        string pmodestr=smcontrol.get_string("processing_mode");
        if(pmodestr=="EventGathers")
                procmode=EventGathers;
        else if(pmodestr=="GenericGathers")
                procmode=GenericGathers;
        else
                procmode=ContinuousDB;
	current_phase=smcontrol.get_string("default_phase");

	if(procmode == ContinuousDB)
	{
		if(hname!="")
		{
		    try {
			instream.open(hname.c_str(),ios::in);
		    } catch (ios::failure& var)
			{
				string mess;
				mess=var.what();
				throw SeisppError("SessionManager constructor"
				 + string("open failed on control stream ")
				 + hname
				 + string("System message")
				 + mess);
			}
		}
	}
	else
	{
		if(hname=="")
		{
			throw SeisppError("SessionManager constructor:"
			 + string("Command line inconsistent with parameter file\n")
			 + string("The -q command line options is required for selected processing_mode parameter."));
		}
		else
		{
			queuefilename=hname;
		}	
	}
		
	bool turn_off_popups=smcontrol.get_bool("turn_off_informational_popups");
	if(turn_off_popups)
	{
		display_initial_sort_box=false;
		display_analysis_sort_box=false;
	}
	/* these parameters control the color of lines on data and beam
	plots.  */
	markers.beam_color=smcontrol.get_string("beam_window_marker_color");
	markers.robust_color=smcontrol.get_string("robust_window_marker_color");
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
    t=pfget_tbl(pf,const_cast<char *>("filters"));
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
 
    //dbh.close();
    if (xpe != NULL) delete xpe;  //believe that deletion of xpe will result in deletion of the last mcc
}

void SessionManager::record( string s)
{
    XmTextInsert(msg_w,XmTextGetLastPosition(msg_w),(char *)(s.c_str()));
    XClearWindow(XtDisplay(msg_w),XtWindow(msg_w));
    log_stream << s <<endl;
    cout << s <<endl;
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
    XcorEngineMode pmode=this->get_processing_mode();

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
	    break;
	case NEXT_EVENT:
	    sensitive[BTN_NEXTEV]=true;
	    sensitive[BTN_REF]=true;
	    sensitive[MENU_PICKS_MASTER]=true; // alternate for BTN_REF
	    sensitive[BTN_RESTORE]=true;
	    if(pmode!=GenericGathers) sensitive[MENU_OPTIONS_SUBARRAY]=true;
	    sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
 	    sensitive[MENU_PICKS]=true;
	    sensitive[MENU_PICKS_BWIN]=true;
	    sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_PICKS_TEDIT]=true;
	    sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_OPTIONS_MCC]=true;
	    sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
	    break;
	case NEXT_SUBARRAY:
	    sensitive[BTN_NEXTSUB]=true;
	    sensitive[BTN_REF]=true;
	    sensitive[MENU_PICKS_MASTER]=true; // alternate for BTN_REF
	    sensitive[BTN_RESTORE]=true;
	    if(pmode!=GenericGathers) sensitive[MENU_OPTIONS_SUBARRAY]=true;
	    sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
 	    sensitive[MENU_PICKS]=true;
	    sensitive[MENU_PICKS_BWIN]=true;
	    sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_PICKS_TEDIT]=true;
	    sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_OPTIONS_MCC]=true;
            sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
	    break;
	case REF:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
            sensitive[BTN_REF]=true;
	    sensitive[MENU_PICKS_MASTER]=true; // alternate for BTN_REF
            sensitive[BTN_ANALYZE]=true;
	    sensitive[BTN_RESTORE]=true;
	    if(pmode!=GenericGathers) sensitive[MENU_OPTIONS_SUBARRAY]=true;
            sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[MENU_EDIT]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_OPTIONS_MCC]=true;
            sensitive[MENU_VIEW]=true;
            sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
	    break;
	case ANALYZE:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
            sensitive[BTN_REF]=true;
	    sensitive[MENU_PICKS_MASTER]=true; // alternate for BTN_REF
            sensitive[BTN_ANALYZE]=true;
            sensitive[BTN_BEAM_PLOT]=true;
            sensitive[BTN_XCOR_PLOT]=true;
	    sensitive[BTN_RESTORE]=true;
	    if(pmode!=GenericGathers) sensitive[MENU_OPTIONS_SUBARRAY]=true;
            sensitive[MENU_FILE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[MENU_EDIT]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_OPTIONS_MCC]=true;
            sensitive[MENU_VIEW]=true;
            sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
            sensitive[MENU_VIEW_COHERENCE]=true;
	    sensitive[MENU_VIEW_PCORRELATION]=true;
	    sensitive[MENU_VIEW_SWEIGHT]=true;
	    sensitive[MENU_VIEW_SNR]=true;
	    sensitive[BTN_TWEEKER]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
	    break;
  	case SAVE:
            sensitive[BTN_NEXTEV]=true;
	    if(using_subarrays) sensitive[BTN_NEXTSUB]=true;
 	    sensitive[BTN_FILE_SAVE]=true;
            sensitive[BTN_REF]=true;
	    sensitive[MENU_PICKS_MASTER]=true; // alternate for BTN_REF
            sensitive[BTN_ANALYZE]=true;
            sensitive[BTN_BEAM_PLOT]=true;
            sensitive[BTN_XCOR_PLOT]=true;
	    sensitive[BTN_RESTORE]=true;
	    if(pmode!=GenericGathers) sensitive[MENU_OPTIONS_SUBARRAY]=true;
            sensitive[MENU_FILE]=true;
 	    sensitive[MENU_FILE_SAVE]=true;
            sensitive[MENU_FILE_EXIT]=true;
            sensitive[MENU_PICKS]=true;
            sensitive[MENU_PICKS_BWIN]=true;
            sensitive[MENU_PICKS_RWIN]=true;
	    sensitive[MENU_EDIT]=true;
            sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_OPTIONS]=true;
            sensitive[MENU_OPTIONS_FILTER]=true;
	    sensitive[MENU_OPTIONS_SORT]=true;
	    sensitive[MENU_OPTIONS_MCC]=true;
            sensitive[MENU_VIEW]=true;
	    sensitive[MENU_VIEW_SNAME]=true;
	    sensitive[MENU_VIEW_DISTANCE]=true;
            sensitive[MENU_VIEW_COHERENCE]=true;
            sensitive[MENU_VIEW_PCORRELATION]=true;
            sensitive[MENU_VIEW_SWEIGHT]=true;
	    sensitive[MENU_VIEW_SNR]=true;
	    sensitive[BTN_TWEEKER]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
	    break;
	case TRACE_EDIT:
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_PICKS_TEDIT]=true;
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	case PICKING_CUTOFF:
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_PICK_CUTOFF]=true;
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	case THINKING:
            sensitive[MENU_FILE_EXIT]=true;
	    break;
	case TWEEKING:
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_TWEEKER]=true;
	    break;
	case POLARITY_SWITCHING:
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_POLARITY_SWITCHER]=true;
	    break;
	case MANUAL_PICKING:
	    sensitive[MENU_EDIT]=true;
	    sensitive[BTN_MANUAL_PICKING]=true;
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
        case XCORPEAKRATIO:
            sort_order="xcor peak ratio";
            scase=1;
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
	case ESAZ:
	    sort_order="event to source azimuth";
	    scase=0;
	    break;
	case DBARRIVAL_TIME:
	    sort_order="previously measured arrival time";
	    scase=0;
	    break;
	case DISTANCE:
	    sort_order="epicentral distance";
	    scase=0;
	    break;
	default:
	    sort_order="predicted time";
	    scase=0;
    }

    if(scase==1 && state != ANALYZE && state != REF) {
	active_setting.result_sort_order=DISTANCE;
	ss << "You specified an initial sort order of "<<sort_order<<endl<<
	    " which is not available before the analysis"<<endl<<
	    " of the ensemble, "
	    <<"the sort order was reset to "<<endl<<"epicentral distance"<<endl;
        if (display_initial_sort_box) return false;
    } else if (scase==0 && (state==ANALYZE || state==REF)) {
	/* This originally was stack weight.  Switched to xcor peak as this seems normally
	to be a more useful initial sort order after analysis. */
	active_setting.result_sort_order=CORRELATION_PEAK;
	ss << "The sort order for the analysis result is set automatically "
	   << endl <<"to stack weight from the specified "<<sort_order<<endl;
	if (display_analysis_sort_box) return false;
    }
 
    return true;
}
/* These are needed to deal with CSS3.0 database ids. */
void SessionManager::set_evid(long event_id)
{
	evid=event_id;;
}
void SessionManager::set_orid(long origin_id)
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
string SessionManager::plot_title(TimeSeriesEnsemble& tse)
{
	stringstream tss;
	list<string>::iterator key;
	string sval;
	int ival;
	double rval;
	/* procmode is a private variable of the session manager so we
	don't need to use the method used to fetch it */
	switch(procmode)
	{
	case ContinuousDB:
	case EventGathers:
		tss<<current_phase
			<< " data for evid="<<evid
			<<" and orid="<<orid;
	case GenericGathers:
	default:
		/* write strings, then int, then real.  Silently skip
		all entries not stored with ensemble */
		for(key=generic_title_strings.begin();
			key!=generic_title_strings.end();++key)
		{
			try {
				sval=tse.get_string(*key);
				tss<<" "<< (*key) <<"="<<sval;
			} catch (MetadataGetError mde){};
		}
		for(key=generic_title_ints.begin();
			key!=generic_title_ints.end();++key)
		{
			try {
				ival=tse.get_int(*key);
				tss<<" "<< (*key) <<"="<<ival;
			} catch (MetadataGetError mde){};
		}
		for(key=generic_title_reals.begin();
			key!=generic_title_reals.end();++key)
		{
			try {
				rval=tse.get_double(*key);
				tss<<" "<< (*key) <<"="<<rval;
			} catch (MetadataGetError mde){};
		}
		
	};
	string result=tss.str();
	/* trim result if it is too long to no more than 80 characters */
	if(result.length()>80) result.erase(80);
	return(result);
}
