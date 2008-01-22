#include "session_manager.h"
#include "dbxcor.h"
#include "Seisw.h"
#include "SeismicPick.h"
#include "XcorProcessingEngine.h"
#include "cc_tks.h"

#include <Xm/DrawingA.h>

#include "SciPlot/SciPlot.h"
#include "SciPlot/SciPlotUtil.h"

using namespace std;
using namespace SEISPP;

/* Definitions. */
#define      APP_CLASS  "xcor"
#define MAINFORM_GRID_CNT 15
#define LOGNAME "analysis.log"


/* Pulldown menus are built from cascade buttons, so this function
** also includes pullright menus. Create the menu, the cascade button
** that owns the menu, and then the submenu items.
*/
Widget BuildMenu (Widget parent, int menu_type, char *menu_title, char menu_mnemonic,
                  Boolean tear_off, MenuItem *items)
{
        Widget   menu, cascade, widget;
        int      i;
        XmString str;
        Arg      args[8];
        int      n;

        if (menu_type == XmMENU_PULLDOWN)
                menu = XmCreatePulldownMenu (parent,(char *) "_pulldown", NULL, 0);
        else {
                n = 0;
                XtSetArg (args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
                menu = XmCreatePopupMenu (parent, (char *) "_popup", args, n);
        }

        if (tear_off)
                XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

        if (menu_type == XmMENU_PULLDOWN) {
                str = XmStringCreateLocalized (menu_title);
                n = 0;
                XtSetArg (args[n], XmNsubMenuId, menu); n++;
                XtSetArg (args[n], XmNlabelString, str); n++;
                XtSetArg (args[n], XmNmnemonic, menu_mnemonic); n++;
                cascade = XmCreateCascadeButtonGadget (parent, menu_title, args, n);
                XtManageChild (cascade);
                XmStringFree (str);
        }

        /* Now add the menu items */
        for (i = 0; items[i].label != NULL; i++) {
                /* If subitems exist, create the pull-right menu by calling this
                ** function recursively. Since the function returns a cascade
                ** button, the widget returned is used..
                */

                if (items[i].subitems) {
                        widget = BuildMenu (menu, XmMENU_PULLDOWN, items[i].label,
                                                items[i].mnemonic, tear_off, items[i].subitems);
			items[i].w=widget;
                } else {
                        widget = XtVaCreateManagedWidget (items[i].label, *items[i].class1, menu, NULL);
                        items[i].w=widget;
                }

                /* Whether the item is a real item or a cascade button with a
                ** menu, it can still have a mnemonic.
                */
                if (items[i].mnemonic)
                        XtVaSetValues (widget, XmNmnemonic, items[i].mnemonic, NULL);

                /* any item can have an accelerator, except cascade menus. But,
                ** we don't worry about that; we know better in our declarations.
                */
                if (items[i].accelerator) {
                        str = XmStringCreateLocalized (items[i].accel_text);

                        XtVaSetValues (widget, XmNaccelerator, items[i].accelerator, XmNacceleratorText,
                                       str, NULL);
                        XmStringFree (str);
                }
                if (items[i].callback) {
                        String resource;

                        if (XmIsToggleButton(widget) || XmIsToggleButtonGadget(widget))
                                resource = XmNvalueChangedCallback;
                        else
                                resource = XmNactivateCallback;

                        XtAddCallback(widget, resource, items[i].callback,(XtPointer) items[i].callback_data);
                }
        }

        return (menu_type == XmMENU_PULLDOWN) ? cascade : menu ;
}

Widget create_button(Widget parent, MenuItem btninfo)
{
  Widget w;
  XmString str;
  int n;
  Arg args[4];

  n=0;
  str = XmStringCreateLocalized (btninfo.label);
  XtSetArg (args[n], XmNlabelString, str); n++;
  XtSetArg (args[n], XmNmultiClick, XmMULTICLICK_DISCARD); n++;
  w = XmCreatePushButton (parent, btninfo.label, args, n);
  XmStringFree (str);
  XtAddCallback(w,XmNactivateCallback,btninfo.callback,(XtPointer)(btninfo.callback_data));
  XtManageChild(w);    

  return w;
}


/*
** Destroy the shell parent of the Message box, and thus the box itself
*/
void destroy_dialog (Widget dialog, XtPointer client_data, XtPointer call_data)
{
        XtDestroyWidget (XtParent (dialog)); /* The shell parent of the Message box */
}


/*
** PostDialog() -- a generalized routine that allows the programmer
** to specify a dialog type (message, information, error, help, * etc..), and the message to show.
*/
Widget PostDialog (Widget parent, int dialog_type, char *msg)
{
        Widget   dialog;
        XmString text;

        dialog = XmCreateMessageDialog (parent, (char *) "dialog", NULL, 0);
        text = XmStringCreateLocalized (msg);
        XtVaSetValues (dialog, XmNdialogType, dialog_type, XmNmessageString, text, NULL);
        XmStringFree (text);

        XtUnmanageChild (XtNameToWidget (dialog, "Cancel"));
        XtSetSensitive (XtNameToWidget (dialog, "Help"), False);
//        XtAddCallback (dialog, XmNokCallback, destroy_dialog, NULL);

        XtManageChild (dialog);

        return dialog;
}


/*******************************************************************************
 Seismic processing routines
*******************************************************************************/

void 
do_sw(Widget parent, SessionManager & sm)
{
 	int i,j;

        Pf *pf;
        if(pfread(const_cast<char *>((sm.get_pf_name()).c_str()),&pf))
        {
                cerr << "Error reading pf file = "<<sm.get_pf_name()<<endl;
                exit(-1);
        }
	Pf *pfrda;
        if(pfget(pf,(char *) "phase_processing_parameters",(void **)&pfrda) != PFARR)
                throw SeisppError("dbxcor:do_sw:  pfget failure looking for phase_processingParameters keyword");

	Tbl *t;
        t = pfkeys(pfrda);
        for(int i=0;i<maxtbl(t);++i)
        {
		char *key;
                key = static_cast<char *>(gettbl(t,i));
		try {
			string thisphase(key);
			Metadata mdphase(pfrda,thisphase);
			XcorAnalysisSetting asphase(mdphase);
			sm.asetting_default[thisphase]=asphase;
		} catch (MetadataParseError mde)
		{
			cerr << "Problems parsing parameter file for phase="<<key<<endl
				<< "dbxcor will not be able to process this phase."  <<endl
				<< "Error from constructor"<<endl;
			mde.log_error();
		}
		catch (SeisppError serr)
		{
			cerr << "Problems parsing parameter file for phase="<<key<<endl
				<< "dbxcor will not be able to process this phase."  <<endl
				<< "Error from constructor"<<endl;
			serr.log_error();
		}
        }
        freetbl(t,0);
	if(sm.asetting_default.size()<=0)
		throw SeisppError("do_sw:  total failure in setting up phase processing setup");

        try {
	// initially load the first element of the container of XcorAnalysisSetting setups.
	// Not ideal, but not easy to fix without producing other problems.
	map<string,XcorAnalysisSetting>::iterator asetptr;
	asetptr=sm.asetting_default.begin();
	sm.active_setting=asetptr->second;
	sm.set_phase(asetptr->first);
	/* This forces a reset of default filter setting */
	sm.modify_filter(string("default"),sm.active_setting.filter_param);
        sm.xpe=new XcorProcessingEngine(pf,asetptr->second,sm.get_waveform_db_name(),sm.get_result_db_name());
	sm.using_subarrays=sm.xpe->use_subarrays;

	int n=0;
	Arg args[4];
	XtSetArg(args[n],(char *) ExmNzoomFactor,100); n++;
	XtSetArg(args[n],XmNpaneMaximum,20000); n++;
	XtSetArg(args[n],XmNpaneMinimum,500); n++;
	
	sm.seismic_widget=ExmCreateSeisw(parent,(char *) "Seisw",args,n);
	XtManageChild(sm.seismic_widget);
	}
        catch (SeisppError serr)
        {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
        }

}
 
Widget get_top_shell(Widget w);
void destroy_callback(Widget w, void * client_data, void * userdata);

void disable_display(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct *)userdata;

    if (cbs->set == XmUNSET) {
	if (psm->get_state()!=ANALYZE && psm->get_state()!=SAVE) psm->display_initial_sort_box=true;
	else psm->display_analysis_sort_box=true;
    } 

    if (cbs->set == XmSET) {
        if (psm->get_state()!=ANALYZE && psm->get_state()!=SAVE) psm->display_initial_sort_box=false;
        else psm->display_analysis_sort_box=false;
    }
    
}

void message_box(SessionManager * psm, string s, Widget parent)
{
    XmString text;
    Arg args[4];
    Widget msg_box,rowcol,label,check,separator,form,ok_btn;
    int i;

    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    msg_box=XmCreateDialogShell(parent,(char *) "Information",args,i);

    i=0;
    XtSetArg (args[i], XmNpacking, XmPACK_TIGHT); i++;
    XtSetArg (args[i], XmNuserData, psm); i++;
    rowcol = XmCreateRowColumn (msg_box, (char *) "rowcolumn", args, i);


    text=XmStringCreateLocalized((char*)s.c_str());

    i=0;
    XtSetArg(args[i],XmNlabelString,text); i++;
    label=XmCreateLabel(rowcol,(char *) "info",args,i);
    XmStringFree(text);
    XtManageChild(label);

    check=XmCreateToggleButtonGadget(rowcol,(char *) "Don't show this message to me again",
			NULL,0);
    XtAddCallback (check, XmNvalueChangedCallback, disable_display, psm);
    XtManageChild(check);

    separator = XmCreateSeparatorGadget (rowcol, (char *) "sep",NULL, 0);
    XtManageChild (separator);

    //create the ok button in the action area
    i=0;
    XtSetArg(args[i],XmNfractionBase,3); i++;
    form=XmCreateForm(rowcol,(char *) "form",args,i);

    ok_btn=XmCreatePushButtonGadget(form,(char *) "OK",NULL,0);
    XtVaSetValues(ok_btn, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNshowAsDefault, True,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL);
    XtManageChild(ok_btn);
    XtAddCallback(ok_btn,XmNactivateCallback,destroy_callback,msg_box);

    XtManageChild(form);
    
    XtManageChild(rowcol);
}
// Companion to get_next_event.  Changes analysis setting to 
// that defined for phase.  Throws an error if an XcorAnalysisSetting object
// is not found for requested phase.
void modify_asetting_for_phase(SessionManager& sm,string phase)
{
	map<string,XcorAnalysisSetting>::iterator asptr;
	asptr=sm.asetting_default.find(phase);
	if(asptr==sm.asetting_default.end())
	{
		throw SeisppError(string("Phase request error:  ")
			+ string("Don't know how to processs phase=")
			+phase);
	}
	sm.active_setting=asptr->second;
	sm.xpe->change_analysis_setting(sm.active_setting);
	sm.set_phase(phase);
	/* This forces a reset of the default filter for this phase */
	sm.modify_filter(string("default"),sm.active_setting.filter_param);
}

void handle_next_event( int orid, string phase_to_analyze, Widget w, SessionManager *psm )
{
	stringstream ss;
	int evid;
        double lat,lon,depth,otime;
        const string method("tttaup");
        const string model("iasp91");
        char yrmonday[15],day[6],hrminsec[20];
   	int i;

	try {

	psm->record(string("Loading data for next event.... Please wait\n"));
	Metadata mdfinder;

	//mcc is associated with the XcorProcessingEngine, it seems
	//that XcorProcessingEngine's analyze() method does clean up itself,
	//so here if we delete mcc, later we might get a seg fault when
	//next time XcorProcessingEngine tries to do another analyze...
//        if (psm->mcc != NULL) delete psm->mcc;

/*
	if(dbgetv(psm->db,0,"evid",&evid,
		"orid",&orid,
		"lat",&lat,
		"lon",&lon,
		"depth",&depth,
		"time",&otime,0)!=dbINVALID)
*/
	if(orid>=0)
	{
		const string base_error("handle_next_event:  ");
		mdfinder.put("orid",orid);
		list<int> recs=psm->dbh.find(mdfinder);
		if(recs.size()<=0)
		{
			ss << orid << " not found in input database"<<endl
				<< "Trying to read next orid from control stream"<<endl;
        	        psm->record(ss.str());
			return;
		}
		else if(recs.size()>1)
			throw SeisppError(base_error
				+ string("event(x)origin has duplicate orids\n")
				+ string("Run dbverify and restart"));
		psm->dbh.db.record= *(recs.begin());
		if(dbgetv(psm->dbh.db,0,"evid",&evid,
			"lat",&lat,
			"lon",&lon,
			"depth",&depth,
			"time",&otime,0)==dbINVALID)
		{
			throw SeisppError(base_error
				+string("error reading origin data from input db"));
		}
			
		psm->set_evid(evid);
		psm->set_orid(orid);
		// Reset analysis setting if the phase name to fetch changes
		if(psm->get_phase()!=phase_to_analyze)
		{	
		    try {
			modify_asetting_for_phase(*psm,phase_to_analyze);
		    } catch (SeisppError serr) {
			serr.log_error();
			ss << "Do no know how to handle phase = "<<phase_to_analyze<<endl;
			psm->record(ss.str());
			return;
		    }
		}

                Hypocenter h(rad(lat),rad(lon),depth,otime,method,model);
		psm->set_hypo(h);
                ss << "Loading data for event: "
                        << lat<<","
                        << lon<<","
                        << depth <<","
                        << strtime(otime)<<endl;
		psm->record(ss.str());
		psm->session_state(THINKING);
                psm->xpe->load_data(h);
		ss << "Data loaded" <<endl;
		psm->record(ss.str());
		if(psm->using_subarrays)
		{
			// After a read always reset this variable to 
			// start at top of the list of subarrays
			psm->xpe->current_subarray=0;
			psm->session_state(NEXT_SUBARRAY);
			ss << "Displaying data for subarray="
				<< psm->xpe->current_subarray<<endl;
		}
		else
		{
			psm->session_state(NEXT_EVENT);
			ss << "Displaying data for this event"<<endl;
		}
		TimeSeriesEnsemble * tse=psm->xpe->get_waveforms_gui();
		ss << "Ensemble has " << tse->member.size()
			<<" seismograms"<<endl;
		psm->record(ss.str());

		Metadata data_md=psm->xpe->get_data_md();
		stringstream ts;
		if(psm->using_subarrays)
		{
		    ts << psm->xpe->current_subarray_name
			<< " "
			<< phase_to_analyze 
			<< " data for evid="<<evid
			<<", orid="<<orid
			<<".  Location:  "
			<<lat <<","<<lon<<","<<depth<<","<<strtime(otime);      
		}
		else
		{
		    ts << phase_to_analyze << " data for evid="<<evid
			<<", orid="<<orid
			<<".  Location:  "
			<<lat <<","<<lon<<","<<depth<<","<<strtime(otime);      
		}
		data_md.put("title",ts.str());

		psm->active_setting=psm->asetting_default[phase_to_analyze];
                stringstream vs;
                if (!psm->validate_setting(vs) && w != NULL) 
				message_box(psm, vs.str(), w);

    		psm->xpe->change_analysis_setting(psm->active_setting);

                try {
        	    psm->xpe->sort_ensemble();
    		} catch (SeisppError serr) {
		    serr.log_error();
        	    ss << "Fatal error occured! Sort was unsuccessful."<<endl;
        	    psm->record(ss.str());
        	    return;
    		}

		//make sure the attributes are appropriate, no coherence display before the
		//analysis
		for(i=0; i<psm->attributes_info.size(); i++) {
		    if (psm->attributes_info[i].enabled && 
			!psm->attributes_info[i].available_before_analysis) {
			psm->attributes_info[i].enabled=false;
			XtUnmanageChild(psm->attributes_info[i].w);
		    }
		}

    	        ss << "Ensemble is sorted"<<endl;
		tse=psm->xpe->get_waveforms_gui();

		psm->markers.beam_tw=psm->active_setting.beam_tw;
		psm->markers.robust_tw=psm->active_setting.robust_tw;
		psm->markers.beam_color=string("red");
		psm->markers.robust_color=string("green");
  		psm->markers.title=ts.str();

		XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble, (XtPointer)(tse),
		    ExmNseiswMetadata, (XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),
		    NULL);

		psm->record(ss.str());
		psm->record(string("Done\n"));
	}  

	} catch (SeisppError serr) {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
		exit(-1);
	}
}

void get_next_event(Widget w, void * client_data, void * userdata)
{
	int orid;
	string phase_to_analyze;

	try {

	SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

	if(psm->instream.good())
	{
		const string base_error("get_next_event:  ");
		psm->instream >> orid;
		psm->instream >> phase_to_analyze;

		handle_next_event( orid, phase_to_analyze, w, psm );
	}

	} catch (SeisppError serr) {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
		exit(-1);
	}
}

void sort_picked(Widget w, void * client_data, void * userdata)
{
    Widget pane=XtParent(XtParent(w));

    int n=(int)client_data;

    XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct *)userdata;

    if (cbs->set == XmUNSET) return; 
    
    XtVaSetValues(pane,XmNuserData,n,NULL);
}

void destroy_callback(Widget w, void * client_data, void * userdata)
{
    Widget destroyed=(Widget)client_data;

    XtDestroyWidget(destroyed);
}

void apply_sort_order(Widget w, void * client_data, void * userdata)
{
    TimeSeriesEnsemble * tse;
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget pane=XtParent(XtParent(w));
    stringstream ss;

    int n;

    XtVaGetValues(pane,XmNuserData,&n,NULL);

    switch (n) {
	case COHERENCE:
	    ss << "Result sort order set to coherence"<<endl;
	    psm->active_setting.result_sort_order=COHERENCE;
	    break;
        case CORRELATION_PEAK:
	    ss << "Result sort order set to correlation peak"<<endl;
	    psm->active_setting.result_sort_order=CORRELATION_PEAK;
	    break;
	case AMPLITUDE:
	    ss << "Result sort order set to amplitude"<<endl;
	    psm->active_setting.result_sort_order=AMPLITUDE;
	    break;
	case LAG:
	    ss << "Result sort order set to lag"<<endl;
	    psm->active_setting.result_sort_order=LAG;
	    break;
	case WEIGHT:
	    ss << "Result sort order set to stack weight"<<endl;
	    psm->active_setting.result_sort_order=WEIGHT;
	    break;
        case SITE_LAT:
	    ss << "Ensemble sort order set to lat"<<endl;
	    psm->active_setting.result_sort_order=SITE_LAT;
	    break;
	case SITE_LON:
            ss << "Ensemble sort order set to lon"<<endl;
            psm->active_setting.result_sort_order=SITE_LON;
            break;
	case PREDARR_TIME:
	    ss << "Ensemble sort order set to predicted time"<<endl;
	    psm->active_setting.result_sort_order=PREDARR_TIME;
	    break;
	case ESAZ:
	    ss << "Ensemble sort order set to esac"<<endl;
	    psm->active_setting.result_sort_order=ESAZ;
	    break;
	case DISTANCE:
	    ss << "Ensemble sort order set to distance"<<endl;
	    psm->active_setting.result_sort_order=DISTANCE;
	    break;
	case DBARRIVAL_TIME:
	    if(psm->xpe->arrival_times_are_loaded())
	    {
	        ss << "Ensemble sort order set to arrival time from database"<<endl;
	    	psm->active_setting.result_sort_order=DBARRIVAL_TIME;
	    }
	    else
	    {
		ss << "Cannot set ensemble order to database arrival time"<<endl
			<<"Make another selection or change parameter file to set load_arrivals true"
			<<endl;
		psm->record(ss.str());
		return;
	    }
	    break;
	case ARRIVAL_TIME:
	    ss << "Ensemble sort order set to current measured arrival times"<<endl;
	    psm->active_setting.result_sort_order=ARRIVAL_TIME;
	    break;
	case SNR:
	    ss << "Ensemble sort order set to signal to noise ratio"<<endl;
	    psm->active_setting.result_sort_order=SNR;
	    break;
	default:
	    ss << "ERROR: unknown result sort order"<<endl;
	    break;
    }
    psm->record(ss.str());

    psm->xpe->change_analysis_setting(psm->active_setting);


    Metadata data_md=psm->xpe->get_data_md();

    try {
    	psm->xpe->sort_ensemble();
    } catch (SeisppError serr) {
	serr.log_error();
	ss << "Fatal error occured! Sort was unsuccessful."<<endl;
	psm->record(ss.str());
	XtDestroyWidget(XtParent(XtParent(XtParent(w))));
	return;
    }

    tse=psm->xpe->get_waveforms_gui();

    data_md.put("title",psm->markers.title);

    XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble,(XtPointer)(tse),
       ExmNseiswMetadata,(XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),NULL);

    ss << "Ensemble is sorted according to designated order"<<endl;

    psm->record(ss.str());

}

Widget get_top_shell(Widget w)
{
    while(w && !XtIsWMShell(w)) {
	w=XtParent(w);
    }

    return w;
}
void apply_sort_normal(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    psm->active_setting.sort_reverse=false;
    apply_sort_order(w,client_data,userdata);
}
void apply_sort_reverse(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    psm->active_setting.sort_reverse=true;
    apply_sort_order(w,client_data,userdata);
}

void toggled(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm;
    int i=(int)client_data;

    XtVaGetValues(XtParent(XtParent(w)),XmNuserData,&psm,NULL);

    XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct *)userdata;

    if (cbs->set == XmUNSET) psm->attributes_info[i].enabled=false;
    if (cbs->set == XmSET) psm->attributes_info[i].enabled=true;
}

void do_attr_window(Widget w, void * client_data, void * userdata);

void pick_attributes(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget rowcol, check_box, attr_box, attr_dialog, form, ok_btn, cancel_btn;
    Widget separator;
    Arg args[10];
    int i,checked[10];
    Dimension h;

    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    attr_dialog=XmCreateDialogShell(get_top_shell(w),(char *) "Attributes to View",args,i);

    i=0;
    XtSetArg (args[i], XmNpacking, XmPACK_TIGHT); i++;
    XtSetArg (args[i], XmNuserData, psm); i++;
    rowcol = XmCreateRowColumn (attr_dialog, (char *) "rowcolumn", args, i);

    i = 0;
    XtSetArg (args[i], XmNpacking, XmPACK_COLUMN); i++;
    XtSetArg (args[i], XmNnumColumns, 2); i++;
    check_box = XmCreateRowColumn (rowcol, (char *) "Check Box", args, i);

    for (i = 0; i < psm->attributes_info.size(); i++) {
        attr_box = XmCreateToggleButtonGadget (check_box, 
	    (char *)(psm->attributes_info[i].display_name.c_str()), NULL, 0);
        if (psm->attributes_info[i].enabled) 
	    XtVaSetValues(attr_box,XmNset,XmSET,NULL);
        XtAddCallback (attr_box, XmNvalueChangedCallback, toggled, (XtPointer) i);
        XtManageChild (attr_box);
	if (psm->get_state() != ANALYZE && psm->get_state() != SAVE && 
	    !psm->attributes_info[i].available_before_analysis) {
	    XtSetSensitive(attr_box,False);
	}
    }

    XtManageChild(check_box);
 
    separator = XmCreateSeparatorGadget (rowcol, (char *) "sep",NULL, 0);
    XtManageChild (separator);

    //create the ok and cancel buttons in the action area
    i=0;
    XtSetArg(args[i],XmNfractionBase,5); i++;
    form=XmCreateForm(rowcol,(char *) "form",args,i);

    ok_btn=XmCreatePushButtonGadget(form,(char *) "Apply",NULL,0);
    XtVaSetValues(ok_btn, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNshowAsDefault, True,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL);
    XtManageChild(ok_btn);
    XtAddCallback(ok_btn,XmNactivateCallback,do_attr_window,psm);

    cancel_btn=XmCreatePushButtonGadget(form,(char *) "Cancel",NULL,0);
    XtVaSetValues (cancel_btn, XmNsensitive, True,
                               XmNtopAttachment, XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment, XmATTACH_POSITION,
                               XmNleftPosition, 3,
                               XmNrightAttachment, XmATTACH_POSITION,
                               XmNrightPosition, 4,
                               XmNshowAsDefault, False,
                               XmNdefaultButtonShadowThickness, 1,
                               NULL);
    XtManageChild (cancel_btn);
    XtAddCallback(cancel_btn,XmNactivateCallback,destroy_callback,attr_dialog);

    XtManageChild (form);
    XtManageChild(rowcol);

}

void pick_sort_options(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget radio_box, sort_dialog, pane, form, ok_btn, ok_reverse_btn, cancel_btn;
    XmString str_coherence, str_correlation_peak, str_amplitude, str_lag, str_weight;
    XmString str_lat,str_lon,str_time;
    Arg args[10];
    int i, picked, selected;
    Dimension h;
    
    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    sort_dialog=XmCreateDialogShell(get_top_shell(w),(char *) "Sort Options",args,i);

    selected=psm->active_setting.result_sort_order;

    //paned window under the dialog shell
    i=0;
    XtSetArg(args[i],XmNsashWidth,1); i++;
    XtSetArg(args[i],XmNsashHeight,1); i++;
    XtSetArg(args[i],XmNuserData,selected); i++;
    pane=XmCreatePanedWindow(sort_dialog,(char *) "pane",args,i);

    radio_box=XmCreateRadioBox(pane,(char *) "Sort Options",NULL,0);

    Widget wtemp;
    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "lat",NULL,0);
    picked=SITE_LAT;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "lon",NULL,0);
    picked=SITE_LON;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Predicted Time",NULL,0);
    picked=PREDARR_TIME;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Database Arrival Time",NULL,0);
    picked=DBARRIVAL_TIME;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }


    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "source azimuth",NULL,0);
    picked=ESAZ;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }
    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Distance",NULL,0);
    picked=DISTANCE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }


    SessionState state=psm->get_state();

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "measured amplitude",NULL,0);
    picked=AMPLITUDE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Computed lag",NULL,0);
    picked=LAG;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Coherence",NULL,0);
    picked=COHERENCE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }
   
    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Peak Correlation",NULL,0);
    picked=CORRELATION_PEAK;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Amplitude",NULL,0);
    picked=AMPLITUDE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Signal to Noise Ratio",NULL,0);
    picked=SNR;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
        XtVaSetValues(wtemp,XmNset,XmSET,NULL);
        XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Lag",NULL,0);
    picked=LAG;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Measured Arrival Time",NULL,0);
    picked=ARRIVAL_TIME;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Stack Weight",NULL,0);
    picked=WEIGHT;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    XtManageChild(radio_box);


     //create the sort, sort reverse,  and cancel buttons in the action area
     i=0;
     XtSetArg(args[i],XmNfractionBase,7); i++;
     form=XmCreateForm(pane,(char *) "form",args,i);

     ok_btn=XmCreatePushButtonGadget(form,(char *) "Sort normal",NULL,0);
     XtVaSetValues(ok_btn, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNshowAsDefault, True,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL);
     XtManageChild(ok_btn);
     XtAddCallback(ok_btn,XmNactivateCallback,apply_sort_normal,psm);

     ok_reverse_btn=XmCreatePushButtonGadget(form,(char *) "Sort reverse",NULL,0);
     XtVaSetValues(ok_reverse_btn, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 3,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 4,
                        XmNshowAsDefault, True,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL);
     XtManageChild(ok_reverse_btn);
     XtAddCallback(ok_reverse_btn,XmNactivateCallback,apply_sort_reverse,psm);

     cancel_btn=XmCreatePushButtonGadget(form,(char *) "Cancel",NULL,0);
     XtVaSetValues (cancel_btn, XmNsensitive, True,
                               XmNtopAttachment, XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment, XmATTACH_POSITION,
                               XmNleftPosition, 5,
                               XmNrightAttachment, XmATTACH_POSITION,
                               XmNrightPosition, 6,
                               XmNshowAsDefault, False,
                               XmNdefaultButtonShadowThickness, 1,
                               NULL);
     XtManageChild (cancel_btn);
     XtAddCallback(cancel_btn,XmNactivateCallback,destroy_callback,sort_dialog);

     XtManageChild (form);

     XtVaGetValues(cancel_btn,XmNheight,&h,NULL);
     XtVaSetValues(form,XmNpaneMaximum,h,XmNpaneMinimum,h,NULL);

     XtManageChild(pane);

}

void apply_filter(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    stringstream ss;
    /* Here we get the filter number through the userDataresource of the 
	radio button parent*/
    Widget pane=XtParent(XtParent(w));
    int n;
    XtVaGetValues(pane,XmNuserData,&n,NULL);
    psm->set_filter(n);
    /* For now we apply filters recursively to accumulate results
	in a filter chain.  This is done because restore data can 
	be used to back up to a starting point.  Comment out the 
	following line to change to always start from base data. */
    /* psm->xpe->restore_original_ensemble();*/
    TimeInvariantFilter cf=psm->get_filter(n);

       try {

	   //Here we don't do redisplay since we assume that after this box disappear,
	   //an automatic redisplay is enabled.
	   psm->xpe->filter_data(cf);
	   psm->xpe->sort_ensemble();

       } catch (SeisppError serr) {
           ss   << "Filter_data failed using "
                << psm->get_filter_label(psm->current_filter())
		<<":"<<psm->filter_description()
		 <<", try again"<<endl;
  	   serr.log_error();
	   psm->record(ss.str());
       }
       XClearArea(XtDisplay(psm->seismic_widget),XtWindow(psm->seismic_widget),
		0,0,0,0,true);
}

void pick_filter_options(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget radio_box, filter_dialog, pane, form, ok_btn, cancel_btn;
    Arg args[10];
    int i, picked, selected;
    Dimension h;
    
    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    filter_dialog=XmCreateDialogShell(get_top_shell(w),(char *) "Filter Options",args,i);

    selected=psm->current_filter();

    //paned window under the dialog shell
    i=0;
    XtSetArg(args[i],XmNsashWidth,1); i++;
    XtSetArg(args[i],XmNsashHeight,1); i++;
    XtSetArg(args[i],XmNuserData,selected); i++;
    pane=XmCreatePanedWindow(filter_dialog,(char *) "pane",args,i);

    radio_box=XmCreateRadioBox(pane,(char *) "Filter Options",NULL,0);

    Widget wtemp;
    int nfilters=psm->number_filters();
    for(i=0;i<nfilters;++i)
    {
	string label;
	/* Name is expanded for the default name for clarity.
	Others use tag from parameter file */
	if(i==0)
	{
		label=psm->get_filter_label(i)
			+string(":")
			+psm->filter_description(i);
	}
	else
		label=psm->get_filter_label(i);
	wtemp=XmCreateToggleButtonGadget(radio_box,
		const_cast<char *>(label.c_str()),NULL,0);
	XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(i));
	XtManageChild(wtemp);
	if(i==selected){
		psm->set_filter(label);
		XtVaSetValues(wtemp,XmNset,XmSET,NULL);
		XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
	}
    }
    XtManageChild(radio_box);


     //create the ok and cancel buttons in the action area
     i=0;
     XtSetArg(args[i],XmNfractionBase,5); i++;
     form=XmCreateForm(pane,(char *) "form",args,i);

     ok_btn=XmCreatePushButtonGadget(form,(char *) "Apply",NULL,0);
     XtVaSetValues(ok_btn, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_POSITION,
                        XmNleftPosition, 1,
                        XmNrightAttachment, XmATTACH_POSITION,
                        XmNrightPosition, 2,
                        XmNshowAsDefault, True,
                        XmNdefaultButtonShadowThickness, 1,
                        NULL);
     XtManageChild(ok_btn);
     XtAddCallback(ok_btn,XmNactivateCallback,apply_filter,psm);

     cancel_btn=XmCreatePushButtonGadget(form,(char *) "Cancel",NULL,0);
     XtVaSetValues (cancel_btn, XmNsensitive, True,
                               XmNtopAttachment, XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
                               XmNleftAttachment, XmATTACH_POSITION,
                               XmNleftPosition, 3,
                               XmNrightAttachment, XmATTACH_POSITION,
                               XmNrightPosition, 4,
                               XmNshowAsDefault, False,
                               XmNdefaultButtonShadowThickness, 1,
                               NULL);
     XtManageChild (cancel_btn);
     XtAddCallback(cancel_btn,XmNactivateCallback,destroy_callback,filter_dialog);

     XtManageChild (form);

     XtVaGetValues(cancel_btn,XmNheight,&h,NULL);
     XtVaSetValues(form,XmNpaneMaximum,h,XmNpaneMinimum,h,NULL);

     XtManageChild(pane);

}
void report_edit(Widget w, void * client_data, void * userdata)
{
    SeismicPick * spick;
    TimeSeriesEnsemble * tse;
    stringstream ss;
    Dimension edit_enable;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(w,ExmNseiswPick,&spick,ExmNseiswEnsemble,&tse,
	ExmNeditEnable,&edit_enable,NULL);
    int tmp=spick->trace_number;

    if (edit_enable==1) {
      if (spick->type == POINT) {
        if (tse->member[tmp].live) ss<<"Restored ";
	else ss << "Deleted ";
        ss<<"trace #"<<tmp+1<<endl;
      } else ss << "Wrong pick type "<<spick->type<<endl;
    } else {
      if (spick->type == POINT) {
        if (tse->member[tmp].live) ss<<"Restored ";
        else ss << "Cutoff ";
        ss<<"from trace #"<<tmp+1<<endl;
      } else ss << "Wrong pick type "<<spick->type<<endl;      	
    }

    psm->record(ss.str());
}

void toggle_edit(Widget w, void * client_data, void * userdata)
{
    XmString str;    
    Dimension edit_enable;
    Arg args[8];
    int n;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(psm->seismic_widget,ExmNeditEnable, &edit_enable, NULL);
  
    if (edit_enable==0) {
	edit_enable=1;
	str = XmStringCreateLocalized ((char *) "Stop Trace Edit");
	psm->session_state(TRACE_EDIT);
    } else {
	edit_enable=0; 
	str = XmStringCreateLocalized ((char *) "Trace Edit");
	psm->restore_previous_state();
    }   

    XtVaSetValues(w, XmNlabelString, str, NULL);
    XmStringFree (str);

    XtVaSetValues(psm->seismic_widget,ExmNeditEnable,edit_enable,NULL);
    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
    if (edit_enable==1) {
        XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,report_edit,psm);
    }
}

void pick_cutoff(Widget w, void * client_data, void * userdata)
{
    
    XmString str;
    Dimension edit_enable;
    Arg args[8];
    int n;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(psm->seismic_widget,ExmNeditEnable, &edit_enable, NULL);

    if (edit_enable==0) {
        edit_enable=2;
        str = XmStringCreateLocalized ((char *) "Stop Picking Cutoff");
	psm->session_state(PICKING_CUTOFF);
    } else {
        edit_enable=0;
        str = XmStringCreateLocalized ((char *) "Pick Cutoff");
	psm->restore_previous_state();
    }

    XtVaSetValues(w, XmNlabelString, str, NULL);
    XmStringFree (str);

    XtVaSetValues(psm->seismic_widget,ExmNeditEnable,edit_enable,NULL);
    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
    if (edit_enable==2) {
        XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,report_edit,psm);
    }
}

void pick_arrival_callback(Widget w, void * client_data, void * userdata)
{
    SeismicPick * spick;    
    stringstream ss;
    double tmp; 

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
    tmp=(double)(spick->get_point().time);
   
    if (spick->type == POINT) {
 	ss << "Time shift is "<<(double)tmp<<endl;
	psm->xpe->shift_arrivals((double)tmp);
	psm->active_setting.phase_time_pick=(double)tmp;
    } else {
	ss << "Wrong pick type "<<spick->type<<endl;
    }
    psm->beammarkers.beam_tw=TimeWindow(tmp,tmp);
    XtVaSetValues(w,ExmNdisplayMarkers,(XtPointer)(&(psm->beammarkers)),NULL);

    XtRemoveAllCallbacks(w,ExmNbtn2Callback);

    psm->record(ss.str());

    psm->session_state(SAVE);
}

void pick_phase_time(Widget w, void * client_data, void * userdata)
{
    Widget beam_widget=reinterpret_cast<Widget>(client_data);
    SessionManager * psm;
    
    XtVaGetValues(XtParent(w),XmNuserData,&psm,NULL);
    XtRemoveAllCallbacks(beam_widget,ExmNbtn2Callback);
    XtAddCallback(beam_widget,ExmNbtn2Callback,pick_arrival_callback,psm);
}

void dismiss_callback(Widget w, void * client_data, void * userdata)
{
	Widget parent=reinterpret_cast<Widget>(client_data);
	XtDestroyWidget(parent);
}
void pick_rt_callback(Widget w, void * client_data, void * userdata)
{
    SeismicPick * spick;
    stringstream ss;
    int tmp;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
    tmp=spick->get_trace_number();

    if (spick->type == POINT) {
        ss << "Reference trace is "<<tmp+1<<endl;
	psm->active_setting.rt_set=true;
	psm->active_setting.reference_trace=tmp;
    } else {
        ss << "Wrong pick type "<<spick->type<<endl;
    }

    psm->session_state(REF);

    psm->record(ss.str());

    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
}

void pick_ref_trace(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
    XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,pick_rt_callback,psm);
}


void pick_window_callback(Widget w, void * client_data, void * userdata)
{
    SeismicPick * spick;
    stringstream ss;
    TimeWindow tmp;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    int choice=psm->choice;  

    XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
    tmp=spick->get_window();

    if (spick->type == WINDOW) {
        if (choice==1) {
            ss << "Beam window is selected to be "<<tmp.start<< " to "
	  	<<tmp.end<<endl;
	    psm->active_setting.bw_set=true;
	    psm->active_setting.beam_tw=tmp;
	    psm->markers.beam_tw=tmp;
	} else if (choice==2) {
	    ss << "Robust window is selected to be "<<tmp.start<< " to "
                <<tmp.end<<endl;
	    psm->active_setting.rw_set=true;
	    psm->active_setting.robust_tw=tmp;
	    psm->markers.robust_tw=tmp;
	}
	XtVaSetValues(psm->seismic_widget,ExmNdisplayMarkers,(XtPointer)(&(psm->markers)),NULL);
    } else {
        ss << "Wrong pick type "<<spick->type<<endl;
    }

    psm->record(ss.str());

    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn3Callback);
}


void pick_bwindow(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn3Callback);
    psm->choice=1;
    XtAddCallback(psm->seismic_widget,ExmNbtn3Callback,pick_window_callback,psm);
}

void pick_rwindow(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn3Callback);
    psm->choice=2;
    XtAddCallback(psm->seismic_widget,ExmNbtn3Callback,pick_window_callback,psm);
}


void do_analyze(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    stringstream vs;
    if (!psm->validate_setting(vs)) message_box(psm, vs.str(), w);

    psm->record(string("Analyzing.... Please wait\n"));
    psm->session_state(THINKING);

    try {
        psm->xpe->change_analysis_setting(psm->active_setting);
    	psm->mcc=psm->xpe->analyze();
        psm->xpe->sort_ensemble();
    } catch (SeisppError serr) {
	serr.log_error();
	psm->record(string("Fatal error encountered during analysis...\n"));
	psm->restore_previous_state();
	return;
    }
    psm->restore_previous_state();

    Metadata data_md=psm->xpe->get_data_md();
    TimeSeriesEnsemble * tse=psm->xpe->get_waveforms_gui();

    data_md.put("title",psm->markers.title);

    XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble,(XtPointer)(tse),
       ExmNseiswMetadata,(XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),NULL);

    psm->session_state(ANALYZE);

    psm->record(string("Done\n"));
}
/*  This next two funtions are a confusing adaptation of the window plotting
method used for the main display.  We use the "robust_tw" time window to 
hold the estimate of the arrival uncertainty.  This is used in beam plot
callback below to set deltim in the Metadata area of the beam trace.  
This pair of procedures are callbacks to the pick error button added by glp
July 2007.
*/
void pick_error_window_callback(Widget w, void * client_data, void * userdata)
{
    SeismicPick * spick;
    TimeWindow tmp;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
    tmp=spick->get_window();
    psm->beammarkers.robust_tw=tmp;
    XtVaSetValues(w,ExmNdisplayMarkers,(XtPointer)(&(psm->beammarkers)),NULL);
    double deltim;
    deltim=(tmp.start - tmp.end)/2.0;
    TimeSeriesEnsemble *tseptr=psm->xpe->get_waveforms_gui();
    tseptr->put("deltim",deltim);
    XtRemoveAllCallbacks(w,ExmNbtn3Callback);
}

void pick_arrival_error(Widget w, void * client_data, void * userdata)
{
    
    Widget beam_widget=reinterpret_cast<Widget>(client_data);
    SessionManager * psm;

    XtVaGetValues(XtParent(w),XmNuserData,&psm,NULL);
    XtRemoveAllCallbacks(beam_widget,ExmNbtn3Callback);
    XtAddCallback(beam_widget,ExmNbtn3Callback,pick_error_window_callback,psm);
}
/* This global seems unavoidable to avoid memory leaks.  Within the block
below it holds the data for the array beam display.  In an X program which 
passes around all these opaque pointers, we need manage the memory locally
and I see now alternative here. */
TimeSeriesEnsemble *beam_tse(NULL);
/* This procedure creates the beam plot window and manages it */
void do_beam_plot(Widget w, void * client_data, void * userdata)
{
    Widget rshell_beam, beam_widget, pane, form, btn_arrival;     
    XWindowAttributes xwa;
    Dimension h;
    int n;
    Arg args[10];

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    rshell_beam=XtVaCreatePopupShell ("Beam Plot",topLevelShellWidgetClass, get_top_shell(w),
               XmNtitle,"Beam Plot",XmNallowShellResize,True,XmNwidth,800,XmNheight,800,
                XmNdeleteResponse,XmUNMAP,NULL);
    pane = XtVaCreateWidget("Beam Plot",
	xmPanedWindowWidgetClass, rshell_beam,
	NULL);
    

    TimeSeries beam=psm->mcc->ArrayBeam();
    /* beam_tse is a global for this program.  It has to be that
    way to avoid a memory leak every time this function was entered.
    I so no straightforward alternative. */
    if(beam_tse!=NULL) delete beam_tse;
    beam_tse=new TimeSeriesEnsemble(0,beam.ns);
    beam_tse->member.push_back(beam);

    Metadata beam_display_md=psm->xpe->get_beam_md();
    beam_display_md.put("title",psm->markers.title);

    n=0;
    /*Previous had this.  This disables callbacks for MB3 
    XtSetArg(args[n],(char *) ExmNdisplayOnly,1); n++;
    **************************************/
    XtSetArg(args[n],XmNpaneMinimum,500); n++;
    XtSetArg(args[n],XmNmarginHeight,100);n++;
    // Don't think this is really necessary or desirable 
    //XtSetArg(args[n],(char *) ExmNcleanupData, static_cast<XtPointer>(beam_tse));n++;
    
    beam_widget=ExmCreateSeisw(pane,(char *) "Beam Plot",args,n);

    XtManageChild(beam_widget);
    /* the Seisw widget always initializes with a NULL data pointer. 
    The following is necessary to get the data to display and to 
    show pick markers */
    
    XtVaSetValues(beam_widget,
		ExmNseiswEnsemble,static_cast<XtPointer>(beam_tse),
		ExmNseiswMetadata,&beam_display_md,
		ExmNdisplayMarkers,(XtPointer)(&(psm->beammarkers)),NULL);

/*  Old single button version.  
    n=0;
    XtSetArg(args[n],XmNuserData,psm); n++;
    form=XmCreateForm(pane,(char *) "form",args,n);   
    btn_arrival=XmCreatePushButtonGadget(form,(char *) "Pick Arrival",NULL,0);
    XtVaSetValues(btn_arrival, XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNleftAttachment, XmATTACH_FORM,
                        NULL);
    XtManageChild(btn_arrival);
    XtAddCallback(btn_arrival,XmNactivateCallback,pick_phase_time,beam_widget);
    
    XtVaGetValues(btn_arrival,XmNheight,&h,NULL);
    XtVaSetValues(form,XmNpaneMaximum,h,XmNpaneMinimum,h,NULL);
    
    XtManageChild(form);
    XtManageChild(pane);
 New derived from main for multiple buttons on bottom of display.*/
	n=0;
	XtSetArg(args[n],XmNuserData,psm); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNtopWidget, pane); n++;
	XtSetArg(args[n],XmNpaneMinimum,50); n++;
	XtSetArg(args[n],XmNpaneMaximum,100); n++;
	/* These attach the button widgets to the top and bottom of the parent form 
	XtSetArg(args[n],XmNtopAttachment,XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	*/

	Widget tlrc=XmCreateRowColumn(pane,(char *) "btnrow",args,n);
	XtVaSetValues(tlrc,XmNorientation,XmHORIZONTAL,
          XmNentryAlignment,XmALIGNMENT_BEGINNING,XmNpacking,XmPACK_COLUMN,NULL);
	MenuItem btninfo;

	btninfo.label=(char *)("Pick Arrival");
	btninfo.callback=pick_phase_time;
	btninfo.callback_data=beam_widget;
	psm->controls[BTN_ARRIVAL]=create_button(tlrc,btninfo);


	btninfo.label=(char *)("Pick Error");
	btninfo.callback=pick_arrival_error;
	btninfo.callback_data=beam_widget;
	psm->controls[BTN_ARRIVAL_ERROR]=create_button(tlrc,btninfo);
	// Manually create this callback to provide for a "Dismiss" 
	// button.  Added Sept 2007 by glp
/*
	n=0;
	XmString str;
	const string dstr("Dismiss");
	str=XmStringCreateLocalized(const_cast<char *>(dstr.c_str()));
	XtSetArg (args[n], XmNlabelString, str); n++;
	XtSetArg (args[n], XmNmultiClick, XmMULTICLICK_DISCARD); n++;
	Widget w_dismiss=XmCreatePushButton (tlrc,const_cast<char *>(dstr.c_str()),
							args,n);
	XmStringFree(str);
	XtAddCallback(w_dismiss,XmNactivateCallback,dismiss_callback,
			(XtPointer)(rshell_beam));
	XtManageChild(w_dismiss);
*/
			
	XtManageChild(tlrc);
	XtManageChild(pane);
/* End new stuff */

    XtPopup (rshell_beam, XtGrabNone);
}


void update_attributes_display(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    int i,index=-100;
    int begin, end;
    DisplayAttributes ai;
    TimeSeriesEnsemble * tse;
    Display *dpy=XtDisplay(w);
    Window win=XtWindow(w);
    stringstream ss;

    XGCValues *values=NULL;
    GC gc;
    XFontStruct *fa;
    XColor scolor,ecolor;
    XWindowAttributes wa;
    Colormap cmap;
    string name;
    char tmp[15];
    int twidth;

    for(i=0; i<psm->attributes_info.size(); i++) {
	if (w==psm->attributes_info[i].w) {
	    index=i;
	    break;
	}
    }

    XtVaGetValues(psm->seismic_widget,ExmNdisplayAttributes,&ai,ExmNseiswEnsemble,&tse,NULL);
    if(ai==NULL)
    {
	ss << "update_attributes_display:  failure fetching X resource displayAttributes "
		<< "cannot refresh attribute displays" <<endl;
	psm->record(ss.str());
	throw SeisppError(string("Nil pointer returned for displayAttributes"));
    }

    begin=MAX((ai->x2begb-(int)ai->x2begb==0.0) ? (int)ai->x2begb : (int)ai->x2begb+1,1);
    end=MIN((int)ai->x2endb,tse->member.size());
    /* March 13, 2007:  Kind of a hack fix by glp.  The begin calculation
	above is unreliable.  It makes assumptions that aren't valid
	and I found the program crashed because situations occured when
	end was less than begin.  For now I'll fix this by plotting
	all when this happens. */
    if(begin>end)
    {
	cerr << "DEBUG:  resetting range for attribute plot"<<endl
		<<"begin="<<begin<<" end="<<end;
	begin=0;
    }


    if (psm->attributes_info[index].use_graph) {
                double *x1, *x2;
                int height, line;

                x1=new double[end-begin+1];
                x2=new double[end-begin+1];

                name=psm->attributes_info[index].name;
                for(i=begin; i<=end; i++) {
		   // Make this more bombproof against get_double 
		   // throwing an exception 
		   try {
                      x1[i-begin]=tse->member[i-1].get_double(name.c_str());
		   } catch (MetadataGetError mderr)
		   {
			cerr << "dbxcor(update_attributes_display) warning:"<<endl;
			mderr.log_error();
			cerr << "Setting attribute to 0.0"<<endl;
			x1[i-begin]=0.0;
		    }
                    x2[i-begin]=i;
                }

                height=MAX(0, -ai->str_origin[end-begin]+ai->str_origin[0]);

/*
		if (psm->attributes_info[index].graph_widget != NULL) {
		    XtUnmanageChild(psm->attributes_info[index].graph_widget);
		    XtDestroyWidget(psm->attributes_info[index].graph_widget);
		    psm->attributes_info[index].graph_widget=NULL;
		}

                Widget plotWidget=XtVaCreateManagedWidget(name.c_str(), 
 		    sciplotWidgetClass,w, 
 		    XtNheight, 250, XtNwidth, 250, 
 		    XtNplotTitle, "", 
 		    XtNxLabel, "Values", 
 		    XtNyLabel, "Trace #", 
 		    XtNchartType, XtCARTESIAN, 
 		    XtNshowLegend, false, 
 		    NULL);
		psm->attributes_info[index].graph_widget=plotWidget;

		SciPlotListCreateFromDouble(plotWidget,end-begin+1,x1,x2,"");
*/
		SciPlotListDelete(psm->attributes_info[index].graph_widget,
				psm->attributes_info[index].line);

		psm->attributes_info[index].line=SciPlotListCreateFromDouble(
			psm->attributes_info[index].graph_widget,
			end-begin+1,x1,x2,(char *) "");

		SciPlotListSetStyle(psm->attributes_info[index].graph_widget,
		    psm->attributes_info[index].line, 255, XtMARKER_CIRCLE,
		    255, XtLINE_NONE);

		if (SciPlotQuickUpdate(psm->attributes_info[index].graph_widget))
		    SciPlotUpdate(psm->attributes_info[index].graph_widget);

                delete x1;
                delete x2;
       } else {

	    XtCreateWindow(w,(unsigned int) InputOutput, (Visual *) CopyFromParent,0,NULL);
	    win=XtWindow(w);

	    XClearWindow(dpy,win);

	    gc = XCreateGC(dpy,win,(unsigned long)0,values);
	    fa = XLoadQueryFont(dpy,"fixed");
	    if (fa==NULL) {
                ss<<"Cannot load/query fix font"<<endl;
		psm->record(ss.str());
                return;
            } else XSetFont(dpy,gc,fa->fid);

            XGetWindowAttributes(dpy,win,&wa);
            cmap = wa.colormap;
	    if (XAllocNamedColor(dpy,cmap,"red",&scolor,&ecolor))
                XSetForeground(dpy,gc,ecolor.pixel);
            else
                XSetForeground(dpy,gc,1L);

	    if (psm->attributes_info[index].type==ATTR_STR) {
	      for(i=begin; i<=end; i++) {
		name=tse->member[i-1].get_string((psm->attributes_info[index].name).c_str());
		snprintf(tmp,14,"%d: %s",i,name.c_str());
		XDrawString(dpy,win,gc,0,ai->str_origin[i-begin],tmp,strlen(tmp));
	      }
	    } else {
	      for(i=begin; i<=end; i++) {
		snprintf(tmp,14,"%d: %f",i,
		    tse->member[i-1].get_double((psm->attributes_info[index].name).c_str()));
                XDrawString(dpy,win,gc,0,ai->str_origin[i-begin],tmp,strlen(tmp));
              }
	    }    
 	    twidth=XTextWidth(fa,tmp,strlen(tmp));
	    XtVaSetValues(w,XmNpaneMinimum,twidth,XmNpaneMaximum,twidth,NULL);

	    XFreeGC(dpy,gc);
      }
    
}

void refresh_attr_win(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget wdgt;
    int i;

    for(i=0; i<psm->attributes_info.size(); i++) {
        if ((wdgt=psm->attributes_info[i].w) != NULL) {
	    if ((XtIsManaged(wdgt)==True) && psm->attributes_info[i].enabled) {
		try {
	 		update_attributes_display(wdgt,client_data,NULL);
		} catch (SeisppError serr)
		{
			/* there may be a way to handle this error, but for now
			we force an exit.  Debugging found this led to inconsistency
			that ultimately produced a seg fault.  For now only 
			alterative is to exit cleanly with an error. */
			serr.log_error();
			exit(-1);
		}
	    }
	}
    }
    
}


void viewmenu_toggle(Widget w, void * client_data, void *userdata, int index)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget wdgt;

    if (psm->attributes_info.size() > index) {
        XmToggleButtonCallbackStruct *cbs=(XmToggleButtonCallbackStruct *)userdata;

        if (cbs->set == XmUNSET) psm->attributes_info[index].enabled=false;
        if (cbs->set == XmSET) psm->attributes_info[index].enabled=true;
    }
    do_attr_window(w,client_data,userdata);
}

void viewmenu_toggle_0(Widget w, void * client_data, void * userdata)
{
	viewmenu_toggle(w,client_data,userdata,0);
}
void viewmenu_toggle_1(Widget w, void * client_data, void * userdata)
{
        viewmenu_toggle(w,client_data,userdata,1);
}
void viewmenu_toggle_2(Widget w, void * client_data, void * userdata)
{
        viewmenu_toggle(w,client_data,userdata,2);
}
void viewmenu_toggle_3(Widget w, void * client_data, void * userdata)
{
        viewmenu_toggle(w,client_data,userdata,3);
}
void viewmenu_toggle_4(Widget w, void * client_data, void * userdata)
{
        viewmenu_toggle(w,client_data,userdata,4);
}
void viewmenu_toggle_5(Widget w, void * client_data, void * userdata)
{
        viewmenu_toggle(w,client_data,userdata,5);
}

void do_attr_window(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    int i,pos_indx=0;
    Widget wdgt;

    for(i=0; i<psm->attributes_info.size(); i++) {
	if ((wdgt=psm->attributes_info[i].w) != NULL) {
	    if ((XtIsManaged(wdgt)==False) && psm->attributes_info[i].enabled) {

		if (psm->attributes_info[i].use_graph) {
		    if (psm->attributes_info[i].graph_widget != NULL) {
			XtUnmanageChild(psm->attributes_info[i].graph_widget);
		 	XtDestroyWidget(psm->attributes_info[i].graph_widget);
			psm->attributes_info[i].graph_widget=NULL;
		    }

                    Widget plotWidget=XtVaCreateManagedWidget(psm->attributes_info[i].name.c_str(),
                        sciplotWidgetClass,wdgt,
                        XtNheight, 250, XtNwidth, 250,
                        XtNplotTitle, "",
                        XtNxLabel, psm->attributes_info[i].name.c_str(),
                        XtNyLabel, "Trace #",
                        XtNchartType, XtCARTESIAN,
			XtNshowLegend, False,
                        NULL);

		    double x[1],y[1];
		    x[0]=y[0]=1.0;

		    psm->attributes_info[i].line=SciPlotListCreateFromDouble(plotWidget,1,x,y,(char *) "");

		    psm->attributes_info[i].graph_widget=plotWidget;
		} else {
		    XtDestroyWidget(psm->attributes_info[i].w);
		    wdgt=psm->attributes_info[i].w=XmCreateDrawingArea(psm->parent,(char *) "attribute",NULL,0);
		}

		XtManageChild(wdgt);
		XtVaSetValues(wdgt,XmNpaneMinimum,100,XmNpositionIndex,pos_indx,NULL);

		//for drawing area, we need to have expose function
		if (!psm->attributes_info[i].use_graph)		
		    XtAddCallback(wdgt,XmNexposeCallback,update_attributes_display,psm);

		pos_indx++;
	    }

	    if ((XtIsManaged(wdgt)==True) && !psm->attributes_info[i].enabled) {
		XtUnmanageChild(wdgt);
	    }
	}
    }

    XtAddCallback(psm->seismic_widget,ExmNdisplayAttrCallback,refresh_attr_win,psm);

//    XtDestroyWidget(get_top_shell(w));
} 

//This is just some preliminary list of attributes the time series need 
//to display, we need to have a GUI to let user add one later.
void init_attr(SessionManager & sm, AttributeInfoRec * air)
{
    int i;

    //station name is one of the attributes we want to display
/*
    AttributeInfoRec temp_air1={NULL,"sta",ATTR_STR,false,NULL,-1,false,"Station Name",true};
    sm.attributes_info.push_back(temp_air1);
    AttributeInfoRec temp_air2={NULL,"coherence",ATTR_DOUBLE,true,NULL,-1,false, "Coherence",false};
    sm.attributes_info.push_back(temp_air2);
    AttributeInfoRec temp_air3={NULL,"peak_xcor", ATTR_DOUBLE,true,NULL,-1,false, "Peak Cross-correlation",false};
    sm.attributes_info.push_back(temp_air3);
    AttributeInfoRec temp_air4={NULL,"stack_weight", ATTR_DOUBLE,true,NULL,-1,false, "Stack Weight",false};
    sm.attributes_info.push_back(temp_air4);
*/
/*
    sm.attributes_info.push_back(air[0]);
    sm.attributes_info.push_back(air[1]);
    sm.attributes_info.push_back(air[2]);
    sm.attributes_info.push_back(air[3]);
*/
    for(i=0; i<sm.attributes_info.size(); i++) {
	sm.attributes_info[i].w=XmCreatePanedWindow(sm.parent,(char *) "attribute",NULL,0);
    }    
}

void do_xcor_plot(Widget w, void * client_data, void * userdata)
{
    Widget rshell_xcor, xcor_widget;
    XWindowAttributes xwa;

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    rshell_xcor=XtVaCreatePopupShell ("Correlation Plot",topLevelShellWidgetClass, get_top_shell(w),
                XmNtitle,"Correlation Plot",XmNallowShellResize,True,XmNwidth,800,XmNheight,800,
                XmNdeleteResponse,XmUNMAP,NULL);

    Metadata xcor_display_md=psm->xpe->get_xcor_md();
    xcor_display_md.put("title",psm->markers.title);

    int n=0;
    Arg args[4];
    XtSetArg(args[n],(char *) ExmNdisplayOnly,1); n++;
    xcor_widget=ExmCreateSeisw(rshell_xcor,(char *) "Correlation Plot",args,n);

    XtManageChild(xcor_widget);
    XtVaSetValues(xcor_widget,XmNpaneMinimum,500,
	ExmNseiswEnsemble,&(psm->mcc->xcor),
	ExmNseiswMetadata,&xcor_display_md,
	NULL);

    XtPopup (rshell_xcor, XtGrabNone);
/*
    if (XtIsRealized (rshell_xcor) && XGetWindowAttributes
            (XtDisplay (rshell_xcor), XtWindow (rshell_xcor), &xwa) &&
            xwa.map_state == IsViewable)
            XtPopdown (rshell_xcor);
    else
            XtPopup (rshell_xcor, XtGrabNone);
*/
}


void save_event(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    stringstream ss;

    if (psm->get_state() != SAVE) {
 	psm->record("Illegal save request\n");
	return;
    }   

    int evid,orid;
    evid=psm->get_evid();
    orid=psm->get_orid();
    Hypocenter h=psm->get_hypo();
    try {
          psm->xpe->save_results(evid,orid,h);
	  // Do not reset session_state when subarrays are used
	  // That disables ability to process remaining data.
	  // We handle state change for subarrays by monitoring
	  // current_subarray and switching when the last subarray
	  // is the one being analyzed.
	  if(psm->using_subarrays)
	  {
		// special case for mistake of only one subarray
		if(psm->xpe->number_subarrays() <= 1)
			psm->session_state(NONE);
		else if(psm->xpe->current_subarray
				>= (psm->xpe->number_subarrays()-1) )
			psm->session_state(NONE);
		else
			psm->session_state(NEXT_SUBARRAY);

	  }
	  else
		psm->session_state(NONE);
    } catch (SeisppError serr) {
          ss << "Problems in save_results routine"<<endl;
          serr.log_error();
          ss << "Try again or exit"<<endl;
	  psm->record(ss.str());
    }

}
void restore_data(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    stringstream ss;
    // This method restores semi-raw data.  Resampled to uniform sample
    // rate, but not original.  May require a forced refresh of display here.
    psm->xpe->restore_original_ensemble();
    // For now we have to do a complete reset to this state.
    // NEXT is equivalent to fresh reading of data.
    if(psm->using_subarrays)
	psm->session_state(NEXT_SUBARRAY);
    else
	psm->session_state(NEXT_EVENT);
    ss << "Restoring original data"<<endl;
    psm->record(ss.str());
    XClearArea(XtDisplay(psm->seismic_widget),XtWindow(psm->seismic_widget),
		0,0,0,0,true);
}

/* Callback for next subarray button */
void load_next_subarray(Widget w, void * client_data, void * userdata)
{
	SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
	stringstream ss;
	// this method loads the next subarray data using next subarray in the list
	int lastsub=psm->xpe->number_subarrays()-1;
	if((psm->xpe->current_subarray) <= lastsub)
	{
		// Recover from the error condition when the subarray index
		// is wrong.  This should not happen if the gui is functioning
		// correctly, but best to handle it instead of aborting here.
		try {
			psm->xpe->next_subarray();
		} catch (SeisppError serr)
		{
			serr.log_error();
			ss << "Error trying to load subarray number "
				<< psm->xpe->current_subarray<<endl;
			ss << serr.message<<endl;
			ss << "Resetting to allow reading next event"<<endl;
			psm->session_state(NEXT_EVENT);
		}
		// Update subarray in title string.  This is minor maintenance
		// issue.  This depends on the subarray name being the first word in
		// the current title.
		string oldtitle(psm->markers.title);
		int sstart=oldtitle.find_first_of(" ",0);
		oldtitle.erase(0,sstart);
		psm->markers.title=psm->xpe->current_subarray_name+oldtitle;
		Metadata data_md=psm->xpe->get_data_md();
		data_md.put("title",psm->markers.title);
		XtVaSetValues(psm->seismic_widget,
			ExmNseiswMetadata, (XtPointer)(&data_md),
			ExmNdisplayMarkers,&(psm->markers),NULL);
		// next_subarray increments current_subarray counter
		// If at the end we need to disable next_subarray
		if((psm->xpe->current_subarray)>=lastsub)
			psm->session_state(NEXT_EVENT);
		else
		{
			psm->session_state(NEXT_SUBARRAY);
			ss << "Loaded data for subarray "
				<< psm->xpe->current_subarray<<endl;
		}
	}
	else
	{
		psm->session_state(NEXT_EVENT);
		ss << "No more subarrays for this event.  Push button to read next event"<<endl;
	}
        psm->record(ss.str());
	// This forces a redraw 
    	Display *dpy=XtDisplay(w);
    	Window win=XtWindow(w);
	XClearWindow(dpy,win);
}
/* Callback to toggle subarray mode on and off.  This is an on off
switch mode for this toggle. */
void subarray_toggle(Widget w, void *client_data, void *userdata)
{
	XmString str;

	SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
	if(psm->using_subarrays)
	{
		string label("Enable Subarrays");
		str = XmStringCreateLocalized (const_cast<char *>(label.c_str()));
		psm->using_subarrays=false;
		psm->xpe->use_subarrays=false;
	}
	else
	{
		string label("Disable Subarrays");
		str = XmStringCreateLocalized (const_cast<char *>(label.c_str()));
		psm->using_subarrays=true;
		psm->xpe->use_subarrays=true;
	}
	XtVaSetValues(w, XmNlabelString, str, NULL);
	XmStringFree (str);
	/* Not really necessary when turned off, but better to set this
	explicitly */
	psm->xpe->current_subarray=0;
	// This restores the original data in either direction.
	restore_data(w,client_data,userdata);
	// Call the routine to load subbarray 1 as the active ensemble 
	load_next_subarray(w,client_data,userdata);
}


void exit_gui(Widget w, void * uesless1, void * useless2)
{
    exit(0);
}


void usage(char *use)
{
	/* Old format.  
        cerr << argv[0]<<" dbin dbout dbevent [-pf pffile -v] " <<endl;
	**********************************
	new usage.  Allow db to be - and in that situation retrieve the 
	parent db from the dbview pointer.  By default dbout=db but allow
	alternative output file through the -o option (existing capability
	I think.)  -f will allow fifo input.  In that situation pass
	orid and phase. */
        //cerr << argv[0]<<" db [-o dbout -f infile -pf pffile -v] " <<endl;
	cerr << "dbxcor "<< use <<endl;
        exit(-1);
}

void set_menu_controls(MenuItem * file_menu, MenuItem * picks_menu, MenuItem * options_menu, 
      	MenuItem * view_menu, SessionManager &sm) 
{
    sm.controls[MENU_FILE_SAVE]=file_menu[0].w;
    sm.controls[MENU_FILE_EXIT]=file_menu[1].w;
    sm.controls[MENU_PICKS_BWIN]=picks_menu[0].w;
    sm.controls[MENU_PICKS_RWIN]=picks_menu[1].w;
//    sm.controls[MENU_PICKS_VIEW]=picks_menu[4].w;
//    sm.controls[MENU_PICKS_VIEW_ATTR]=view_submenu[0].w;
//    sm.controls[MENU_PICKS_VIEW_SETTING]=view_submenu[1].w;
    sm.controls[MENU_OPTIONS_SORT]=options_menu[0].w;
    sm.controls[MENU_OPTIONS_FILTER]=options_menu[1].w;
    sm.controls[MENU_VIEW_SNAME]=view_menu[0].w;
    sm.controls[MENU_VIEW_DISTANCE]=view_menu[1].w;
    sm.controls[MENU_VIEW_COHERENCE]=view_menu[2].w;
    sm.controls[MENU_VIEW_PCORRELATION]=view_menu[3].w;
    sm.controls[MENU_VIEW_SWEIGHT]=view_menu[4].w;
    sm.controls[MENU_VIEW_SNR]=view_menu[5].w;
}

bool SEISPP::SEISPP_verbose=false;
/*******************************************************************************
main: Set up the application
*******************************************************************************/
int 
main (int argc, char **argv)
{
  Display	*display;
  Widget       	shell, flrc, slrc, tlrc, menu_bar, menu_picks, menu_options, menu_file, menu_view, menu_settings;
  Widget        btn_next,btn_edit,btn_arrival,btn_ref,btn_bwin,btn_rwin;
  Widget        btn_analyze,btn_beam_plot,btn_xcor_plot;
  Widget 	paned_win, second_paned_win;
  XtAppContext 	AppContext;
  XmString	str;
  MenuItem      btninfo;
  TkSend	*tks=NULL;
  int i,n=0;
  Arg args[18];
  /* Note this struct is defined in the seisw widget display_marker.h */
  AttributeInfoRec air[6]={ {NULL,"sta",ATTR_STR,false,NULL,-1,false,"Station Name",true},
			    {NULL,"distance_deg",ATTR_DOUBLE,true,NULL,-1,false, "Epicentral distance",true},
			    {NULL,"coherence",ATTR_DOUBLE,true,NULL,-1,false, "Coherence",false},
			    {NULL,"peak_xcor", ATTR_DOUBLE,true,NULL,-1,false, "Peak Cross-correlation",false},
			    {NULL,"stack_weight", ATTR_DOUBLE,true,NULL,-1,false, "Stack Weight",false},
			    {NULL,"signal_to_noise_ratio", ATTR_DOUBLE,true,NULL,-1,false, "Signal to Noise Ratio",false}
			};
  char *use=(char *) "db [-appname name -o dbout -f infile -pf pffile -V -v]";
  char *author=(char *) "Peng Wang and Gary Pavlis";
  char *email=(char *) "pewang@indiana.edu,pavlis@indiana.edu";
  char *loc=(char *) "Indiana University";
  char *rev=(char *) "$Revision 1.13$";

  if(argc<2) 
  {
    cbanner(rev,use,author,loc,email);
    usage(use);
  }
  string waveform_db_name(argv[1]);
  if(waveform_db_name=="-V")
  {
     cbanner(rev,use,author,loc,email);
     exit(1);
  }
  string result_db_name(argv[1]);
  //string hypodb(argv[3]);
  string infile("");
  string pfname("dbxcor");
  string appname("dbxcor");
  for(i=2;i<argc;++i) {
      string argtest(argv[i]);
      if(argtest=="-pf") {
           ++i;
           pfname=string(argv[i]);
      } else if(argtest=="-appname") {
           ++i;
           appname=string(argv[i]);
      } else if (argtest=="-o") {
	   ++i;
	   result_db_name=string(argv[i]);
      } else if (argtest=="-f") {
	   ++i;
	   appname=string("");
	   infile=string(argv[i]);
      } else if(argtest=="-V") {
	  cbanner(rev,use,author,loc,email);
          exit(1);
      } else if(argtest=="-v") {
           SEISPP::SEISPP_verbose=true;
      } else {
           usage(use);
      }
  }
  string logstr(LOGNAME);

  try {
      SessionManager sm(pfname,infile,logstr,waveform_db_name,result_db_name);
  sm.attributes_info.push_back(air[0]);
  sm.attributes_info.push_back(air[1]);
  sm.attributes_info.push_back(air[2]);
  sm.attributes_info.push_back(air[3]);
  sm.attributes_info.push_back(air[4]);
  sm.attributes_info.push_back(air[5]);

 /* Do standard Motif application start-up. */
/*Original from from Peng.  Leads to seg faults in Solaris */
/*
  XtToolkitInitialize();
  AppContext = XtCreateApplicationContext();
  if ((display = XtOpenDisplay (AppContext, NULL, argv[0], APP_CLASS,
				NULL, 0, &argc, argv)) == NULL)
    {
      fprintf (stderr,"\n%s:  Can't open display\n", argv[0]);
      exit(1);
    }
  

  shell = XtVaAppCreateShell(argv[0], APP_CLASS, applicationShellWidgetClass,
			     display, XmNallowShellResize, True, NULL);
*/
/*  Alternative from one book.  Did not work in Solaris 
  shell = XtVaOpenApplication(&AppContext, APP_CLASS, NULL,0,
	&argc,argv,NULL,applicationShellWidgetClass,NULL,XmNallowShellResize, True,0);
*/
 /* Here is another initialization method. */
  XtSetLanguageProc(NULL, NULL, NULL);
  shell = XtVaAppInitialize(&AppContext, APP_CLASS,NULL,0,&argc,argv, NULL,NULL);
			     

  //create first level form widget
  flrc=XmCreateForm(shell,(char *) APP_CLASS,NULL,0);

  //first row of the flrc
  /* Create the menu bar */
  menu_bar = XmCreateMenuBar(flrc, (char *) "menuBar", NULL, 0);
  XtVaSetValues (menu_bar, XmNtopAttachment,  XmATTACH_FORM, XmNleftAttachment, XmATTACH_FORM, 
	XmNrightAttachment, XmATTACH_FORM, NULL);

  MenuItem file_menu[]={
    {(char *) "Save",&xmPushButtonGadgetClass,'s',(char *) "Ctrl<Key>S",NULL,save_event,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Exit",&xmPushButtonGadgetClass,'x',(char *) "Ctrl<Key>C",NULL,exit_gui,(XtPointer)0,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem view_menu[]={
    {(char *)air[0].display_name.c_str(),&xmToggleButtonWidgetClass,'n',NULL,NULL,viewmenu_toggle_0,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[1].display_name.c_str(),&xmToggleButtonWidgetClass,'d',NULL,NULL,viewmenu_toggle_1,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[2].display_name.c_str(),&xmToggleButtonWidgetClass,'c',NULL,NULL,viewmenu_toggle_2,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[3].display_name.c_str(),&xmToggleButtonWidgetClass,'p',NULL,NULL,viewmenu_toggle_3,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[4].display_name.c_str(),&xmToggleButtonWidgetClass,'w',NULL,NULL,viewmenu_toggle_4,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[5].display_name.c_str(),&xmToggleButtonWidgetClass,'s',NULL,NULL,viewmenu_toggle_5,&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem picks_menu[]={
    {(char *) "Beam Window",&xmPushButtonGadgetClass,'B',NULL,NULL,pick_bwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Robust Window",&xmPushButtonGadgetClass,'R',NULL,NULL,pick_rwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
//    {"View",&xmPushButtonGadgetClass,'V',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)view_submenu},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem options_menu[]={
    {(char *) "Sort Options",&xmPushButtonGadgetClass,'r',NULL,NULL,pick_sort_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Filter Options",&xmPushButtonGadgetClass,'l',NULL,NULL,pick_filter_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  sm.controls[MENU_FILE]=menu_file=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "File",'F',false,file_menu);
  sm.controls[MENU_PICKS]=menu_picks=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "Picks",'P',false,picks_menu);
  sm.controls[MENU_OPTIONS]=menu_options=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "Options",'O',false,options_menu);
  sm.controls[MENU_VIEW]=menu_view=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "View",'V',false,view_menu);

  set_menu_controls(file_menu,picks_menu,options_menu,view_menu,sm);

  /* Menubar is done -- manage it */
  XtManageChild (menu_bar);

  //second row of flrc
  //create second level form widget
  n=0;
  XtSetArg (args[n], XmNfractionBase, MAINFORM_GRID_CNT); n++;
  slrc = XmCreateForm (flrc, (char *) "form",   args, n);
  XtVaSetValues (slrc,
                 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
                 XmNtopAttachment,  XmATTACH_WIDGET,
		 XmNtopWidget, menu_bar,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
  
  /*create paned window 
  This is an outer paned window split with the seismic display
  on top and the message log portion at the bottom.  Attachment
  parameters tie the display to the upper left corner.
  ***************************************************************/
  paned_win=XtVaCreateWidget((char *)"pane",
	xmPanedWindowWidgetClass,slrc,
	XmNorientation,			XmVERTICAL,
	XmNseparatorOn,			True,
	XmNtopAttachment,		 XmATTACH_POSITION,
	XmNtopPosition,		 	0,
	XmNleftAttachment,		 XmATTACH_POSITION,
	XmNleftPosition,		 0,
	XmNrightAttachment,		 XmATTACH_POSITION,
	XmNrightPosition,		 MAINFORM_GRID_CNT,
	XmNbottomAttachment,		 XmATTACH_POSITION,
	XmNbottomPosition,		 MAINFORM_GRID_CNT-1,
	NULL);


  //Use the second paned window to ensure that when attributes are 
  //displayed, they are laid out horizontally.
  second_paned_win=XtVaCreateWidget((char *)"panel",
	xmPanedWindowWidgetClass, paned_win,
	XmNorientation,		XmHORIZONTAL,
	XmNseparatorOn,		False,
	XmNspacing,		0,
	XmNallowResize,		True,
	XmNwidth,		800,
	XmNpaneMaximum,		20000,
	XmNpaneMinimum,		500,
	NULL);
  sm.parent=second_paned_win;

  init_attr(sm, air);

  //create and manage seisw widget
  do_sw(second_paned_win, sm);

  XtManageChild(second_paned_win);

  //create and manage the message area
  n = 0;
  XtSetArg (args[n], XmNrows, 5); n++;
  XtSetArg (args[n], XmNeditable, False); n++;
  XtSetArg (args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, second_paned_win); n++;
  XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM); n++;
  XtSetArg (args[n], XmNrightAttachment, XmATTACH_FORM); n++;
  XtSetArg(args[n],XmNpaneMaximum,500); n++;
  XtSetArg(args[n],XmNpaneMinimum,100); n++;
  sm.msg_w = XmCreateScrolledText (paned_win, (char *) "msg_w", args, n);
  XtManageChild (sm.msg_w);

  //create the third level row column widget 
  n=0;

/*
  XtSetArg (args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNtopPosition, MAINFORM_GRID_CNT-1); n++;
  XtSetArg (args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNbottomPosition, MAINFORM_GRID_CNT); n++;
  XtSetArg (args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNleftPosition, 0); n++;
  XtSetArg (args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNrightPosition, MAINFORM_GRID_CNT); n++;
*/
  XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
  XtSetArg(args[n], XmNtopWidget, paned_win); n++;
//  XtSetArg (args[n], XmNleftAttachment, XmATTACH_FORM); n++;
//  XtSetArg (args[n], XmNbottomAttachment, XmATTACH_FORM); n++;

  tlrc=XmCreateRowColumn(slrc,(char *) "third",args,n);

  XtVaSetValues(tlrc,XmNorientation,XmHORIZONTAL,
        XmNentryAlignment,XmALIGNMENT_BEGINNING,XmNpacking,XmPACK_TIGHT,NULL);


  if( ! appname.compare( "" ) ) {
  	btninfo.label=(char *) "Get Next Event";
  	btninfo.callback=get_next_event;
  	btninfo.callback_data=&sm;
  	sm.controls[BTN_NEXTEV]=create_button(tlrc,btninfo);  
  } else {
	Tks_SetVerbose();
	Display *display=XtDisplay(shell);

	tks = Tks_Create( display, 0, 4 );

	if( tks == NULL ) {
      		fprintf (stderr,"\n%s:  Tks_Create() error\n", argv[0]);
		exit(1);
	}

	if( ! Tks_SetAppName( tks, const_cast<char *>(appname.c_str()) ) ) {
      		fprintf (stderr,"\n%s:  Tks_SetAppName() error\n", argv[0]);
		exit(1);
	}
  }

  btninfo.label=(char *) "Enable subarrays";
  btninfo.callback=subarray_toggle;
  btninfo.callback_data=&sm;
  sm.controls[BTN_SUBON]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Load Next Subarray";
  btninfo.callback=load_next_subarray;
  btninfo.callback_data=&sm;
  sm.controls[BTN_NEXTSUB]=create_button(tlrc,btninfo);  

  btninfo.label=(char *) "Pick Ref Trace";
  btninfo.callback=pick_ref_trace;
  sm.controls[BTN_REF]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Analyze";
  btninfo.callback=do_analyze;
  sm.controls[BTN_ANALYZE]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Trace Edit";
  btninfo.callback=toggle_edit;
  sm.controls[BTN_PICKS_TEDIT]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Plot Beam";
  btninfo.callback=do_beam_plot;
  sm.controls[BTN_BEAM_PLOT]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Plot Correlation";
  btninfo.callback=do_xcor_plot;
  sm.controls[BTN_XCOR_PLOT]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Restore Data";
  btninfo.callback=restore_data;
  sm.controls[BTN_RESTORE]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Pick Cutoff";
  btninfo.callback=pick_cutoff;
  sm.controls[BTN_PICK_CUTOFF]=create_button(tlrc,btninfo);

// Alternate save as button.  Under file menu also
  btninfo.label=(char *) "Save";
  btninfo.callback=save_event;
  sm.controls[BTN_FILE_SAVE]=create_button(tlrc,btninfo);

  XtManageChild(tlrc);

  XtManageChild(paned_win);

  XtManageChild(slrc);
  XtManageChild(flrc);

  XtRealizeWidget(shell);

  sm.session_state();

  if(tks!=NULL)
  {
  do {
	XEvent event;
	char	*msg = 0;
	int	replyrequest = 0;
	XtAppNextEvent(AppContext,&event);
	int	orid;
	Tbl	*parts;
	if( !Tks_GetmsgEventProc( tks, &event, &msg, &replyrequest ) ) {
      		fprintf (stderr,"\n%s:  Tks_GetmsgEventProc() error\n", argv[0]);
		Tks_ClearAppName( tks, const_cast<char *>(appname.c_str()) );
		exit(1);
	} else if( msg ) {
		if( replyrequest ) {
		  if( ! Tks_Reply( tks, msg ) ) {
      			fprintf (stderr,"\n%s:  Tks_Reply() error\n", argv[0]);
			Tks_ClearAppName( tks, const_cast<char *>(appname.c_str()) );
			exit(1);
		  }
		}
		parts = split( msg, ' ' );
		if( maxtbl( parts ) != 2 ) {
      			fprintf (stderr,"\n%s: error parsing Tk input msg '%s'\n", 
				argv[0], msg);
			Tks_ClearAppName( tks, const_cast<char *>(appname.c_str()) );
			exit(1);
		} 
		orid = atoi( (char *) gettbl( parts, 0 ) );
		string phase_to_analyze( (char *) gettbl( parts, 1 ) );
		handle_next_event( orid, phase_to_analyze, NULL, &sm );
		freetbl( parts, 0 );
		free( msg );
		msg = 0;
	}
	XtDispatchEvent(&event);
  } while( XtAppGetExitFlag(AppContext) == FALSE );
  }
  else
	XtAppMainLoop(AppContext);
  } catch (SeisppError serr)
  {
	serr.log_error();
	exit(-1);
  }
  catch (...)
  {
	cerr << "dbxcor:  something threw an unhandled exception"<<endl;
  }
}
