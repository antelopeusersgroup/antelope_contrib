#ifndef __SESSION_MANAGER_H
#define __SESSION_MANAGER_H

#include <sys/file.h>

/*  Standard C headers  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
/* Seismic library base include needed here before Xm includes.
* Something is wrong in Xm include files that makes this necessary.
* True on both solaris and g++ 
*/
#include "seispp.h"

/*  X headers  */
#include <X11/IntrinsicP.h>

/*  Xm headers  */
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/CascadeBG.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/LabelG.h>
#include <Xm/DialogS.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
/* Specialized files for this program */
#include "XcorAnalysisSetting.h"
#include "XcorProcessingEngine.h"
#include "display_marker.h"



using namespace std;
using namespace SEISPP;

enum SessionState {START, NONE, NEXT_EVENT, NEXT_SUBARRAY, REF, ANALYZE, SAVE,
	TRACE_EDIT,PICKING_CUTOFF,THINKING};

enum ControlIdType {BTN_NONE, BTN_FILE_SAVE, BTN_NEXTEV, BTN_NEXTSUB,BTN_REF,  
	BTN_ANALYZE, BTN_BEAM_PLOT, BTN_XCOR_PLOT, BTN_RESTORE,
	BTN_PICKS_TEDIT, BTN_PICK_CUTOFF,
	BTN_SUBON,
	MENU_FILE, MENU_FILE_SAVE,
	MENU_FILE_EXIT, MENU_PICKS, MENU_PICKS_BWIN, MENU_PICKS_RWIN, 
	MENU_PICKS_VIEW, MENU_PICKS_VIEW_ATTR, MENU_PICKS_VIEW_SETTING,
	MENU_OPTIONS, 
	MENU_OPTIONS_SORT, 
	MENU_OPTIONS_FILTER, 
      	MENU_VIEW, MENU_VIEW_SNAME, MENU_VIEW_COHERENCE,
	MENU_VIEW_PCORRELATION, MENU_VIEW_SWEIGHT,
	BTN_ARRIVAL,BTN_ARRIVAL_ERROR};

/* WARNING:  if ControlIdType is changed, this number must be greater than
or equal to number of items in that enum */
#define MAX_NUM_CONTROLS 100

class SessionManager {
public:

    Widget seismic_widget; //the main display thing...
    vector<AttributeInfoRec> attributes_info;
    Widget parent;          //parent paned window widget for both seismic and xcor widget 
    Widget *controls;   //generalization of btns and menu items
    Widget msg_w;   //generalization of msg box            
    ofstream log_stream;  //log file stream
    ifstream instream;

    XcorProcessingEngine * xpe;
    MultichannelCorrelator *mcc;
    // New june 2007.  map allows variable settings for 
    // different seismic phases.  Required to switch phases
    // during processing
    map<string,XcorAnalysisSetting> asetting_default;
    XcorAnalysisSetting active_setting;
    DisplayMarkerDataRec markers;
    DisplayMarkerDataRec beammarkers;
    int choice;  //for user differentiate similar buttons, e.g., pick beam and robust window

    // index into origin table keyed by orid
    DatascopeMatchHandle dbh;
    /*! Main constructor.

	\param pfname parameter file used to build this object.
	\param hname input file name used to drive program.  Expect to read orid and phase as 
		two tokens from each line.  This drives the analysis.
	\param lname log file name 
	\param wdbname waveform database name
	\param rdbname result database name (can be the same as wdbname)
    */
    SessionManager(string pfname, string hname, string lname, string wdbname, string rdbname);
    ~SessionManager();
    string get_waveform_db_name() {return waveform_db_name;}
    string get_result_db_name() {return result_db_name;}
    string get_pf_name() {return pf_name;}
    int get_evid(){return evid;}
    int get_orid(){return orid;}
    Hypocenter get_hypo(){return hypo;}
    string get_phase(){return current_phase;}
    void set_evid(int event_id);
    void set_orid(int origin_id);
    void set_hypo(Hypocenter& h);
    void set_phase(string ph);
    void record(string s);
    void session_state();
    void session_state(SessionState);
    void restore_previous_state();
    SessionState get_state() {return state;}
    //This is not to validate whether the setting make sense to XcorProcessingEngine, since
    //that is supposed to be done by XcorAnalysisSetting object, here just to make sure the state
    //GUI is in permits the specific setting, for example, the sort order can not be coherence
    //before the analysis taken place.
    bool validate_setting(stringstream & ss);

    bool display_initial_sort_box;
    bool display_analysis_sort_box;
    bool using_subarrays;
    /* interface to filters */
    /* Get filter tagged with keyword name */
    TimeInvariantFilter get_filter(string name);
    /* Get filter using integer index */
    TimeInvariantFilter get_filter(int ifilt);
    /* Overloaded method to get current filter */
    TimeInvariantFilter get_filter();
    int number_filters();  // Returns number of defined filters
    /* returns description of filter number ifilt*/
    string filter_description(int ifilt);
    /* returns description of current filter */
    string filter_description();
    /* Above return filter desription from filter object interface.
    This method returns a string used to label this filter in the GUI.*/
    string get_filter_label(int ifilt){return(filter_labels[ifilt]);};
    /* set current the filter to the internal membered tagged with name.
    This method silently sets current to default filter if the name
    given does not match any predefined filter tags. */
    void set_filter(string name);
    /* set current filter using the integer index ifilt */
    void set_filter(int ifilt);
    /* return index number of current filter */
    int current_filter();
    /* Modify or add a filter tagged with name */
    void modify_filter(string name, TimeInvariantFilter& filt);
private:
    bool *sensitive;
    SessionState state;
    string log_file_name;
    string waveform_db_name;
    string result_db_name;
    string pf_name;
// These are needed for save_event for CSS3.0 database manipulations
    int evid;
    int orid;
// Added to preserve previous state.  Needed to make picking functions
// more robust.
    SessionState previous_state;
// Added to get complete information in assoc table
    Hypocenter hypo;
    string current_phase;
    /* Added Oct 2007 to implement menu of available filters.
	The filters are stored using two parallel vector containers.*/
    vector<string> filter_labels;
    vector<TimeInvariantFilter> available_filters;
    /* cross reference between index numbers and names */
    map<string,int> filter_index;
    int current_filter_index;
};

#endif
