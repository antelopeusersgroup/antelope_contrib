#include "session_manager.h"
#include "dbxcor.h"
#include "Seisw.h"
#include "SeismicPick.h"
#include "XcorProcessingEngine.h"
#include "tks.h"

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
		} catch (MetadataParseError& mde)
		{
			cerr << "Problems parsing parameter file for phase="<<key<<endl
				<< "dbxcor will not be able to process this phase."  <<endl
				<< "Error from constructor"<<endl;
			mde.log_error();
		}
		catch (SeisppError& serr)
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
	/* Set the initial value of analysis setting */
	map<string,XcorAnalysisSetting>::iterator asetptr;
	string default_phase=sm.get_phase();
	asetptr=sm.asetting_default.find(default_phase);
	if(asetptr==sm.asetting_default.end())
	{
		asetptr=sm.asetting_default.begin();
		cerr << "do_sw(WARNING):  default phase = "<<default_phase 
			<< " is not defined in phase processing setup"
			<< endl
			<< "Initializing with first item found.  "
			<<"This may produce unpredictable results "
			<< "if you are not running under smartpick"
			<<endl;
	}
	sm.active_setting=asetptr->second;
	sm.set_phase(asetptr->first);

	/* This forces a reset of default filter setting */
	sm.modify_filter(string("default"),sm.active_setting.filter_param);
        sm.xpe=new XcorProcessingEngine(pf,asetptr->second,sm.get_waveform_db_name(),
		sm.get_result_db_name(),sm.get_queuefile_name());
	/*Although the session manager holds the name of the database that
	is to be used, at this point in the execution of this program
	the session manager is incomplete.  We must make the match
	handle, dbh, valid or the program will crash.  This is very
	bad programming logic as it violates creation is initialization
	rules, but I had no choice for making this work with the
	new memory management scheme used in the DatascopeHandle object.
	WATCH OUT:  this is a maintenance issue. */
	DatabaseHandle *rootdbh=sm.xpe->get_db(string("waveformdb"));
	DatascopeHandle *rootdsh=dynamic_cast<DatascopeHandle *>(rootdbh);
	DatascopeHandle dbhw(*rootdsh);
	dbhw.lookup("event");
	dbhw.natural_join("origin");
	list<string>matchkey;
	matchkey.push_back("orid");
	AttributeMap amtmp("css3.0");
	sm.dbh=DatascopeMatchHandle(dbhw,string(""),matchkey,amtmp);

	sm.using_subarrays=sm.xpe->use_subarrays;

	int n=0;
	Arg args[4];
	XtSetArg(args[n],(char *) ExmNzoomFactor,100); n++;
	XtSetArg(args[n],XmNpaneMaximum,20000); n++;
	XtSetArg(args[n],XmNpaneMinimum,500); n++;

	sm.seismic_widget=ExmCreateSeisw(parent,(char *) "Seisw",args,n);
	XtManageChild(sm.seismic_widget);
	}
        catch (SeisppError& serr)
        {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
		exit(-1);
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

string subarray_title(string starting, string subarray_name)
{
	string result("SA->");
	result=result+subarray_name+" "+starting;
	if(result.length()>80) result.erase(80);
	return(result);
}
void load_default_trace_attributes(TimeSeriesEnsemble& d)
{
    vector<TimeSeries>::iterator dptr;
    const double coh0(0.0);
    const double corr0(0.0);
    const double snr0(-1.0);
    const double sw0(1.0);
    const double snrdb0(-10.0);
    for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
    {
        dptr->put(coherence_keyword,coh0);
        dptr->put(stack_weight_keyword,sw0);
        dptr->put(snr_keyword,snr0);
        dptr->put(snrdb_keyword,snrdb0);
        dptr->put(peakxcor_keyword,corr0);
    }
}

void handle_next_event( long orid, string phase_to_analyze, Widget w, SessionManager *psm )
{
	stringstream ss;
	long evid;
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

	if(orid>=0)
	{
		const string base_error("handle_next_event:  ");
		mdfinder.put("orid",orid);
		list<long> recs=psm->dbh.find(mdfinder);
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
			"time",&otime,NULL)==dbINVALID)
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
		    } catch (SeisppError& serr) {
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
                // Load defaults for trace attributes that might have live plots 
                load_default_trace_attributes(*tse);

		Metadata data_md=psm->xpe->get_data_md();
		string title;
		title=psm->plot_title(*tse);
		if(psm->using_subarrays) title=subarray_title(title,psm->xpe->current_subarray_name);
		data_md.put("title",title);

		psm->active_setting=psm->asetting_default[phase_to_analyze];
                stringstream vs;
                if (!psm->validate_setting(vs) )
		{
			if(w != NULL)
				message_box(psm, vs.str(), w);
			else
				psm->record(vs.str());
		}

    		psm->xpe->change_analysis_setting(psm->active_setting);

                try {
        	    psm->xpe->sort_ensemble();
    		} catch (SeisppError& serr) {
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
  		psm->markers.title=title;

		XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble, (XtPointer)(tse),
		    ExmNseiswMetadata, (XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),
		    NULL);

		psm->record(ss.str());
		psm->record(string("Done\n"));
	}

	} catch (SeisppError& serr) {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
		exit(-1);
	}
}
void handle_next_ensemble(string phase_to_analyze,Widget w,SessionManager *psm)
{
	const string base_error("dbxcor::handle_next_ensemble:  ");
        const string method("tttaup");
        const string model("iasp91");
	stringstream ss;
	if((psm->xpe)==NULL)
		throw SeisppError(base_error
		 + string("XcorProcessingEngine has not been initiated or was cleared")
		 + string("\nCannot continue"));
	try {
		DatabaseHandle *dbh;
		TimeSeriesEnsemble *tse;
		if(psm->get_phase()!=phase_to_analyze)
		{
		    try {
			modify_asetting_for_phase(*psm,phase_to_analyze);
		    } catch (SeisppError& serr) {
			serr.log_error();
			ss << "Do not know how to handle phase = "<<phase_to_analyze<<endl;
			psm->record(ss.str());
			return;
		    }
		}
		dbh=psm->xpe->get_db(string("waveformdb"));
		/* NONE is the only possible state after pushing a save button.
		We assume if the state is anything else we should mark it skipped.
		The load_data method using the queue file in the Xcor engine
		needs to mark what it did to the previous ensemble before
		loading the next one. */
		int tsesize;
		DatascopeHandle *dsdbh=dynamic_cast<DatascopeHandle *>(dbh);
		do {
			if(psm->get_state() == NONE)
			{
                                psm->record(string("Calling load_data method"));
				psm->session_state(THINKING);
				psm->xpe->load_data(*dbh,FINISHED);
			}
			else
			{
                                psm->record(string("Skipping this ensemble without saving"));
				psm->session_state(THINKING);
				psm->xpe->load_data(*dbh,SKIPPED);
			}
			tse=psm->xpe->get_waveforms_gui();
			tsesize=tse->member.size();
			if(tsesize<=1)
			{
				ss << "Skipping input ensemble with only one memeber"<<endl;
				psm->record(ss.str());
			}
		}
		while((tsesize<=1) && (dsdbh->db.record<dsdbh->number_tuples()));
		/* Exit the program on this condition as it means there is no more data to process */
		if(dsdbh->db.record>=dsdbh->number_tuples())
		{
			cout << "dbxcor queue is empty.  Exiting"<<endl;
			exit(0);
		}
		ss << "Data loaded with ensemble size=" <<tsesize<<endl;
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
		XcorEngineMode xem=psm->get_processing_mode();
		Hypocenter h;
		if( (xem==ContinuousDB) || (xem==EventGathers) )
		{
			double slat,slon,sdepth,stime;
			try {
				/* Frozen names here are not good, but
				for now we'll use them */
				slat=tse->get_double("source_lat");
				slon=tse->get_double("source_lon");
				sdepth=tse->get_double("source_depth");
				stime=tse->get_double("source_time");
				h=Hypocenter(rad(slat),rad(slon),sdepth,stime,method,model);
				psm->set_hypo(h);
                		ss << "Data loaded for event: "
		                        << slat<<","
		                        << slon<<","
		                        << sdepth <<","
					<< strtime(stime) <<endl;
			}
			catch (...)
			{
				ss << "Problem loading hypocenter.  Previous value retained.  Expect trouble."<<endl;
			}
		}
		ss << "Ensemble has " << tse->member.size()
			<<" seismograms"<<endl;
		psm->record(ss.str());

		Metadata data_md=psm->xpe->get_data_md();
		string title;
		title=psm->plot_title(*tse);
		if(psm->using_subarrays) title=subarray_title(title,psm->xpe->current_subarray_name);
		data_md.put("title",title);

		psm->active_setting=psm->asetting_default[phase_to_analyze];
                stringstream vs;
                if (!psm->validate_setting(vs) )
		{
			if(w != NULL)
				message_box(psm, vs.str(), w);
			else
				psm->record(vs.str());
		}

    		psm->xpe->change_analysis_setting(psm->active_setting);

                try {
        	    psm->xpe->sort_ensemble();
    		} catch (SeisppError& serr) {
		    serr.log_error();
        	    ss << "Fatal error occured! Sort was unsuccessful."<<endl;
        	    psm->record(ss.str());
        	    return;
    		}

		//make sure the attributes are appropriate, no coherence display before the
		//analysis
		for(int i=0; i<psm->attributes_info.size(); i++) {
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
  		psm->markers.title=title;

		XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble, (XtPointer)(tse),
		    ExmNseiswMetadata, (XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),
		    NULL);

		psm->record(ss.str());
		psm->record(string("Done\n"));
	} catch (SeisppError& serr) {
                serr.log_error();
                cerr << "Fatal error in loading data:  exiting"<<endl;
		exit(-1);
	}
}

void get_next_event(Widget w, void * client_data, void * userdata)
{
	int orid;
	string phase_to_analyze;

	try {

	SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
	XcorEngineMode mode=psm->get_processing_mode();
	if(mode==ContinuousDB)
	{
	    if(psm->instream.good())
	    {
		psm->instream >> orid;
		psm->instream >> phase_to_analyze;
		handle_next_event( orid, phase_to_analyze, w, psm );
	    }
	}
	else
	{
	    phase_to_analyze=psm->get_phase();
	    handle_next_ensemble(phase_to_analyze,w,psm);
	}

	} catch (SeisppError& serr) {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
		exit(-1);
	}
}

void sort_picked(Widget w, XtPointer client_data, XtPointer userdata)
{
    Widget pane=XtParent(XtParent(w));

    long int ln;
    ln=reinterpret_cast<long>(client_data);
    int n=ln;

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
        case XCORPEAKRATIO:
            ss << "Result sort order set to xcor peak ratio metric "<<endl;
            psm->active_setting.result_sort_order=XCORPEAKRATIO;
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
    } catch (SeisppError& serr) {
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

void toggled(Widget w, XtPointer client_data, XtPointer userdata)
{
    SessionManager * psm;
    long int ln;
    ln=reinterpret_cast<long>(client_data);
    int i=ln;

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

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *) "Correlation 1st/2nd Peak Ratio",NULL,0);
    picked=XCORPEAKRATIO;
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
void select_mccmode1(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    psm->active_setting.mccmode=CORRELATE_AND_STACK;
}
void select_mccmode2(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    psm->active_setting.mccmode=CORRELATE_ONLY;
}
void select_mccmode3(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    psm->active_setting.mccmode=STACK_ONLY;
}
/* Very similar callback for setting MultichannelCorrelator mode. */
void pick_mcc_options(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget radio_box, mcc_dialog, pane, form, ok_btn, ok_reverse_btn, cancel_btn;
    Arg args[10];
    int i, picked, selected;
    Dimension h;

    selected=psm->active_setting.mccmode;

    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    XtSetArg(args[i],XmNuserData,selected); i++;
    mcc_dialog=XmCreateDialogShell(get_top_shell(w),(char *) "MCC Mode Options",args,i);

    radio_box=XmCreateRadioBox(mcc_dialog,(char *) "MCC Mode Options",NULL,0);

    Widget wtemp;
    wtemp=XmCreateToggleButtonGadget(radio_box,(char *)"Correlate and Stack",NULL,0);
    picked=CORRELATE_AND_STACK;
    XtAddCallback(wtemp,XmNvalueChangedCallback,select_mccmode1,(XtPointer)(psm));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *)"Correlate Only",NULL,0);
    picked=CORRELATE_ONLY;
    XtAddCallback(wtemp,XmNvalueChangedCallback,select_mccmode2,(XtPointer)(psm));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,(char *)"Stack Only",NULL,0);
    picked=STACK_ONLY;
    XtAddCallback(wtemp,XmNvalueChangedCallback,select_mccmode3,(XtPointer)(psm));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    XtManageChild(radio_box);
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

       } catch (SeisppError& serr) {
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
    } catch (SeisppError& serr) {
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
    deltim=(tmp.end - tmp.start)/2.0;
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
and I see no alternative here. */
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
/*  Comment out for now.  Crashes for unknown reason.
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
		   } catch (SeisppError& mderr)
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

                delete [] x1;
                delete [] x2;
       } else {

	    XtCreateWindow(w,(unsigned int) InputOutput, (Visual *) CopyFromParent,0,NULL);
	    win=XtWindow(w);

	    XClearArea(dpy,win,0,0,0,0,true);

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
		} catch (SeisppError& serr)
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

    long evid,orid;
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
	  else if(psm->get_processing_mode()==ContinuousDB)
		psm->session_state(NONE);
	  else
	  {
		  /* This XcorProcessingEngine method clears data already saved and
		   * posts leftovers to the copy displayed by this program.  This is
		   * used to allow recursive processing on the same gather. We intentionally
		   * do not change sessions_state in this case.:w
		   *
		   */
		  int newsize=psm->xpe->clear_already_processed();
		  ss << "Clearing processed data and refreshing edited gather."
			<< "  New gather has "<<newsize<<" members."<<endl;
		  psm->record(ss.str());
		  if(newsize<=0) 
			psm->session_state(NONE);
		  else
		  {
			psm->session_state(NEXT_EVENT);
			/* need to update the display widget or bad things 
			can happen.  We do not mess with the display
			markers in this case since we are not changing
			anything but the contents of the ensemble displayed */
			TimeSeriesEnsemble *tsetmp=psm->xpe->get_waveforms_gui();
			/* This seems necessary, although it should not be.
			seg faults without setting this */
			Metadata data_md=psm->xpe->get_data_md();
			/* DEBUG test:  setting display markers because
			there are hints this is causing a seg fault */
			XtVaSetValues(psm->seismic_widget,
				ExmNseiswMetadata, (XtPointer)(&data_md),
				ExmNdisplayMarkers,&(psm->markers),
				ExmNseiswEnsemble,(XtPointer)(tsetmp),NULL);
			/* Not sure this is necessary, but could not hurt.
			This is duplicate of code in handle_next_event */
			for(int i=0; i<psm->attributes_info.size(); i++) 
			{
                    	  if (psm->attributes_info[i].enabled &&
                              !psm->attributes_info[i].available_before_analysis) 
			  {
                        	psm->attributes_info[i].enabled=false;
                        	XtUnmanageChild(psm->attributes_info[i].w);
                          }
                        }

			
		  }
	  }
    } catch (SeisppError& serr) {
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
    TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
    load_default_trace_attributes(*tse);
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
			TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
			string title=psm->plot_title(*tse);
			title=subarray_title(title,psm->xpe->current_subarray_name);
			Metadata data_md=psm->xpe->get_data_md();
			data_md.put("title",title);
			psm->markers.title=title;
			XtVaSetValues(psm->seismic_widget,
			  ExmNseiswMetadata, (XtPointer)(&data_md),
			  ExmNdisplayMarkers,&(psm->markers),NULL);
		} catch (SeisppError& serr)
		{
			serr.log_error();
			ss << "Error trying to load subarray number "
				<< psm->xpe->current_subarray<<endl;
			ss << serr.message<<endl;
			ss << "Resetting to allow reading next event"<<endl;
			psm->session_state(NEXT_EVENT);
		}
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
	XClearArea(dpy,win,0,0,0,0,true);
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
/* This is a new set of gizmos added summer 2008.  Capability added is to ability to
repair cycle skip errors interactively and ability to interactively change polarity of any
trace in a gather */

/*  I know it is evil to have a global variable like this, but it
solves a practical problem here that makes is useful. Otherwise
we'd have to pass this thing through the widget, which I consider
more dangerous than this. */
static TimeSeriesEnsemble *tweeker_tse(NULL);
/* This is the callback assigned to MB2 for the tweeker window.  It aims to pick
a time, post the result to the parent ensemble, and then realign the data with the
measured pick time */
const int XcorTraceNumber(2);  /* must match position of xcor in trace stack*/
void handle_tweeker_time(Widget w, void *client_data, void *userdata)
{
	SessionManager *psm=reinterpret_cast<SessionManager *> (client_data);
	stringstream ss;
	SeismicPick *spick;
	XtVaGetValues(psm->tweeker_widget,ExmNseiswPick,&spick,NULL);
	double tpicked=spick->get_point().time;
	/* Dogmatically insist on picking XCOR trace */
	int trace_picked=spick->get_trace_number();
	if(trace_picked==XcorTraceNumber)
	{
		/* The pick sign needs to be reversed for use below.  I
		could change the code below, but this makes the parallel
		to the template routines that do this for an ensemble
		clearer. */
		tpicked *= 1.0;
		/* assume this won't produce an exception.  Ok in dbxcor, but
		don't do this if this is recycled. */
		int i=tweeker_tse->member[0].get_int("member_number");
		ss << "Ensemble member "<< i << " pick time shift ="<<tpicked<<endl;
		psm->record(ss.str());
		/* Post this lag to the gather data and xcor members.  Then apply

		to this trace only the same algorithm used in MoveoutTimeShift(xcor)
		and LagShift (data).  We don't use those because they change the whole
		ensemble and we want to change just the ensemble member we just
		picked */
		TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
		tse->member[i].put(moveout_keyword,tpicked);
		psm->mcc->xcor.member[i].put(moveout_keyword,tpicked);
		psm->mcc->xcor.member[i].t0 -= tpicked;
		double tshift=tse->member[i].get_double(arrival_time_key);
		tse->member[i].rtoa(tshift);
		tshift+=tpicked;
		tse->member[i].ator(tshift);
		tse->member[i].put(arrival_time_key,tshift);
		TimeSeriesEnsemble *wintse;
		/* Do the same to the xcor and data trace being
		display (do not shift the beam window).  Here we
		use the equivalent of MoveoutTimeShift because we
		don't care about preserving time zero for the
		data held by this widget. */
		tweeker_tse->member[0].t0 -= tpicked;
		tweeker_tse->member[XcorTraceNumber].t0 -= tpicked;

		// We need to refresh the parent display to have the shift take effect.
		XClearArea(XtDisplay(psm->seismic_widget),XtWindow(psm->seismic_widget),
			0,0,0,0,true);
		// Same for this display
		XClearArea(XtDisplay(psm->tweeker_widget),XtWindow(psm->tweeker_widget),
			0,0,0,0,true);
	}
	else
	{
		/* Land here if user picks on anything but xcor trace */
		ss << "Error:  you must pick on the cross-correlation trace (top of display)"<<endl;
		psm->record(ss.str());
	}
}
void pick_tweeker_time(Widget w, void * client_data, void * userdata)
{
    SessionManager *psm=reinterpret_cast<SessionManager *> (client_data);

    XtRemoveAllCallbacks(psm->tweeker_widget,ExmNbtn2Callback);
    XtAddCallback(psm->tweeker_widget,ExmNbtn2Callback,handle_tweeker_time,psm);
}
/* This is initiated by a Btn2 click on a trace in the parent display when the
tweeking mode button is pushed.  */
void pick_tweekdata_callback(Widget w, void *client_data, void *userdata)
{
	Widget rshell, pane;
	SessionManager *psm=reinterpret_cast<SessionManager *> (client_data);
	SeismicPick *spick;
	stringstream ss;

	/* First use pick object to grab the trace number to be processed from the main display.*/
	XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
	/*MAINTENANCE WARNING:  use of generic i here is a bit error prone because
	i is commonly used as a generic counter.  Nice for shorthand, but be careful */
	int i=spick->get_trace_number();
	/* We immediately try extract trace and associated cross-correlation function.
	We do this immediately becasue this allow us to report an error and return
	immediately if the the trace is marked dead - something we can't allow */
	TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
	TimeSeries d=tse->member[i];
	if(!d.live)
	{
		ss << "Cycle Skip Repair:  selected trace number "
			<< i << " is marked dead.  Try another."<<endl;
		psm->record(ss.str());
		return;
	}
	if(tweeker_tse!=NULL) delete tweeker_tse;
	tweeker_tse=new TimeSeriesEnsemble(0,d.ns);
	/* put the data at the top and the xcor trace at the bottom */
	tweeker_tse->member.push_back(d);
	TimeSeries beam=psm->xpe->get_beam();
	tweeker_tse->member.push_back(beam);
	TimeSeries xcordata=psm->mcc->xcor.member[i];;
	tweeker_tse->member.push_back(xcordata);
	/* Autoscale by peak amplitude */
	MeasureEnsemblePeakAmplitudes<TimeSeriesEnsemble,TimeSeries>(*tweeker_tse,gain_keyword);
	/* Note the 3rd arg set true means use 1/amplitude as scale, which
	is the correct form here. */
	ScaleEnsemble<TimeSeriesEnsemble,TimeSeries>(*tweeker_tse,gain_keyword,true);
	/* Safest to push this to all members of the ensemble. Since it this only has
	three members a small overhead for robustness.  This is an underhanded way to
	pass the original ensemble member downstream */
	for(int ii=0;ii<3;++ii) tweeker_tse->member[ii].put("member_number",i);
	ss << "Creating display to pick time shift for trace number "<< i<<endl;
	psm->record(ss.str());

	rshell=XtVaCreatePopupShell ("Cycle Skip Repair",topLevelShellWidgetClass, get_top_shell(w),
                XmNtitle,"Fix Cycle Skip Window",XmNallowShellResize,True,XmNwidth,800,XmNheight,800,
                XmNdeleteResponse,XmUNMAP,NULL);
	/* We need a paned window to allow us to put some control buttons on the bottom of the
	window */
	pane = XtVaCreateWidget("Cycle Skip Repair",xmPanedWindowWidgetClass, rshell,NULL);

	/* Now we want to put the seisw widget display in the top part of the pane.  To do
	this we first create the widget similar to the code in do_beam_plot.
	Size is generous in case mod are needed.  Note used in two places
	in this function. */
	Arg args[10];
	int n=0;
	XtSetArg(args[n],XmNpaneMinimum,500); n++;
	XtSetArg(args[n],XmNmarginHeight,50);n++;
	/* WARNING:  this may leak memory.  Not sure if seisw destructor clears
	debris like this in an assigment to a generic widget. */
	psm->tweeker_widget = ExmCreateSeisw(pane,(char *)"Seisw",args,n);
	XtManageChild(psm->tweeker_widget);
	/* Widget is created, now set additional entitites through
	the Set method.  Two items used:  metadata and marker data.
	Use beam display Metadata as a starting point and then
	change a few items. */
	Metadata display_md=psm->xpe->get_beam_md();
	stringstream sstitle;
	sstitle<<"Manual Edit Window for Ensemble member="<<i;
	display_md.put("title",sstitle.str());
	/* Similar for display markers */
	static DisplayMarkerDataRec tweeker_markers;
	tweeker_markers = psm->beammarkers;
	tweeker_markers.beam_tw.start=0.0;
	tweeker_markers.beam_tw.end=0.0;
	tweeker_markers.robust_tw.start=0.0;
	tweeker_markers.robust_tw.end=0.0;
	tweeker_markers.title="Manual Picker";

	/* This actually loads data for the display and initiates the plot */
	XtVaSetValues(psm->tweeker_widget,
		ExmNseiswEnsemble,static_cast<XtPointer>(tweeker_tse),
		ExmNseiswMetadata,&display_md,
		ExmNdisplayMarkers,(XtPointer)(&tweeker_markers), NULL);
	/* This builds the area for the button at the bottom of the display */
	n=0;
	XtSetArg(args[n],XmNuserData,psm); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNtopWidget, pane); n++;
	XtSetArg(args[n],XmNpaneMinimum,50); n++;
	XtSetArg(args[n],XmNpaneMaximum,100); n++;
	Widget tlrc=XmCreateRowColumn(pane,(char *) "btnrow",args,n);
        XtVaSetValues(tlrc,XmNorientation,XmHORIZONTAL,
          XmNentryAlignment,XmALIGNMENT_BEGINNING,XmNpacking,XmPACK_COLUMN,NULL);
        MenuItem btninfo;
	btninfo.label=(char *)("Pick Xcor Lag");
	btninfo.callback=pick_tweeker_time;
	btninfo.callback_data=psm;
	psm->controls[BTN_TWEEKER_ARRIVAL]=create_button(tlrc,btninfo);
	XtManageChild(tlrc);
	XtManageChild(pane);
	XtPopup(rshell,XtGrabNone);
}
/* This callback is tied to the parent Seisw widget when the user pushes the button that
initiates interactive repair of cycle skips.  It's primary purpose is to select a member number for
the parent ensemble and pass it to the do_fix_cycle_skip_plot method */
void enable_cycle_skip_picking(Widget w, void *client_data, void *userdata)
{
    SessionManager *psm=reinterpret_cast<SessionManager *>(client_data);
    /* This acts like the trace edit button.  It disables all other widgets
    but the main seismic display widget and the button that toggles the
    tweeker feature.  Note that when this procedure exits the previous state
    of the session_manager is restored. */
    XmString str;
    SessionState current_state=psm->get_state();
    if(current_state==TWEEKING)
    {
	psm->restore_previous_state();
        str = XmStringCreateLocalized ((char *) "Enable Manual Picking");
        XtVaSetValues(w, XmNlabelString, str, NULL);
    }
    else
    {
        psm->session_state(TWEEKING);
        str = XmStringCreateLocalized ((char *) "Stop Manual Picking");
        XtVaSetValues(w, XmNlabelString, str, NULL);
        XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
        XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,pick_tweekdata_callback,psm);
    }
}
/* This is a callback to be driven by a single button.  Picks with MB2 when
a polarity button is pushed will flip the polarity of the trace selected. */
void polarity_switch_callback(Widget w, void *client_data, void *userdaa)
{
	SeismicPick *spick;
	stringstream ss;

	SessionManager *psm=reinterpret_cast<SessionManager *>(client_data);
	TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
	/* This enables the picker to get trace number and simultaneously
	returns a pointer to the parent ensemble.  We need the later because
	we have to alter the data */
	XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
	int trace_picked=spick->get_trace_number();
	if((trace_picked<0) || (trace_picked>=tse->member.size()) )
	{
		ss << "Trace number "<<trace_picked
			<< " picked for polarity reversal is outside range of ensemble"
			<<endl;
	}
	else
	{
		for(int i=0;i<tse->member[trace_picked].ns;++i)
			tse->member[trace_picked].s[i] *= -1.0;
		ss << "Switched polarity of trace number "<<trace_picked<<endl;
		/* We post this flag to metadata as we may want it downstream */
		tse->member[trace_picked].put("polarity_reversed",true);
		/* This forces a redraw of the main display */
		XClearArea(XtDisplay(w),XtWindow(w),
		    0,0,0,0,true);
	}
	psm->record(ss.str());
}


/* This small routine enables a mode for flipping the polarity of a trace the user
points to and clicks with MB2.  It works like the widget for trace editing in many
ways, but is implemented totally differently.  Peng implemented trace editing in
the widget, but here we make this a different callback used only by the dbxcor interface.
*/
void enable_polarity_switching(Widget w, void *client_data, void *userdata)
{
    Widget data_display_widget=reinterpret_cast<Widget>(client_data);
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    XmString str;
    SessionState current_state=psm->get_state();
    if(current_state==POLARITY_SWITCHING)
    {
	psm->restore_previous_state();
        str = XmStringCreateLocalized ((char *) "Polarity Edit");
        XtVaSetValues(w, XmNlabelString, str, NULL);
    }
    else
    {
        psm->session_state(POLARITY_SWITCHING);
        str = XmStringCreateLocalized ((char *) "Stop Polarity Edit");
        XtVaSetValues(w, XmNlabelString, str, NULL);
	XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
	XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,polarity_switch_callback,psm);
    }
}
/* This an the next procedure implement a manual pick function on the main display.
enable_display_mpicker is the callback hooked to a button on the main display.
main_display_manual_picker is a callback enabled for mb2 on the main display
by the former. These are very similar to the polarity editor callbacks above.*/
void main_display_manual_picker(Widget w, void *client_data,void *userdata)
{
	SeismicPick *spick;
	stringstream ss;

	SessionManager *psm=reinterpret_cast<SessionManager *>(client_data);
	TimeSeriesEnsemble *tse=psm->xpe->get_waveforms_gui();
	
	/*enable the picker to get the pick time */
	XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
	double time_picked=spick->get_point().time;
	int trace_picked=spick->get_trace_number();

	if((trace_picked<0) || (trace_picked>=tse->member.size()) )
	{
		ss << "ERROR:  Trace number "<<trace_picked
			<< " picked is outside range of ensemble"
			<<endl;
	}
	else
	{
		int i=trace_picked;  //convenient shorthand
		/* Apply pick times and update display */
		tse->member[i].put(moveout_keyword,time_picked);
		/*
		psm->mcc->xcor.member[i].put(moveout_keyword,time_picked);
		psm->mcc->xcor.member[i].t0 -= time_picked;
		*/
		double tshift=tse->member[i].get_double(arrival_time_key);
		tse->member[i].rtoa(tshift);
		tshift+=time_picked;
		tse->member[i].ator(tshift);
		tse->member[i].put(arrival_time_key,tshift);
		/*WARNING WARNING WARNING:  this is currently 
		set as a const string in XcorProcessingEngine.cc.
		If that line is ever changed this will break. */
		tse->member[i].put("processed",true);
		ss << "Shifting pick for trace number "
			<< i << " by "
			<< tshift <<" seconds."
			<<endl;
		XClearArea(XtDisplay(w),XtWindow(w),
		    0,0,0,0,true);
	}
	psm->record(ss.str());
}
void enable_display_mpicker(Widget w, void *client_data, void *userdata)
{
    Widget data_display_widget=reinterpret_cast<Widget>(client_data);
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    XmString str;
    SessionState current_state=psm->get_state();
    if(current_state==MANUAL_PICKING)
    {
	psm->restore_previous_state();
        str = XmStringCreateLocalized ((char *) "Manual Picking");
        XtVaSetValues(w, XmNlabelString, str, NULL);
    }
    else
    {
        psm->session_state(MANUAL_PICKING);
        str = XmStringCreateLocalized ((char *) "Stop Manual Picking");
        XtVaSetValues(w, XmNlabelString, str, NULL);
	XtRemoveAllCallbacks(psm->seismic_widget,ExmNbtn2Callback);
	XtAddCallback(psm->seismic_widget,ExmNbtn2Callback,main_display_manual_picker,psm);
    }
}
	

void exit_gui(Widget w, void * uesless1, void * useless2)
{
    exit(0);
}


void usage(char *use)
{
	cerr << "dbxcor "<< use <<endl;
        exit(-1);
}

/* This function associates a named member of the controls array to widgets 
attached to the top menu.  This is a pretty ugly way to do this and a serious
maintenance problem as it is disconnected from where the control array is defined
in multiple ways.  Any change to the user interface that alters this relationship
requires a change here.  i.e. add or remove an item from the menu bar and you MUST
make a change here */
void set_menu_controls(MenuItem * file_menu, MenuItem * edit_menu,
	MenuItem * picks_menu, MenuItem * options_menu,
      		MenuItem * view_menu, SessionManager &sm)
{
    sm.controls[MENU_FILE_SAVE]=file_menu[0].w;
    sm.controls[MENU_FILE_EXIT]=file_menu[1].w;
    sm.controls[MENU_PICKS_BWIN]=picks_menu[0].w;
    sm.controls[MENU_PICKS_RWIN]=picks_menu[1].w;
    sm.controls[MENU_PICKS_MASTER]=picks_menu[2].w;
    sm.controls[MENU_OPTIONS_SORT]=options_menu[0].w;
    sm.controls[MENU_OPTIONS_FILTER]=options_menu[1].w;
    sm.controls[MENU_OPTIONS_MCC]=options_menu[2].w;
    sm.controls[MENU_OPTIONS_SUBARRAY]=options_menu[3].w;
    sm.controls[MENU_VIEW_SNAME]=view_menu[0].w;
    sm.controls[MENU_VIEW_DISTANCE]=view_menu[1].w;
    sm.controls[MENU_VIEW_COHERENCE]=view_menu[2].w;
    sm.controls[MENU_VIEW_PCORRELATION]=view_menu[3].w;
    sm.controls[MENU_VIEW_SWEIGHT]=view_menu[4].w;
    sm.controls[MENU_VIEW_SNR]=view_menu[5].w;
    /* edit_menu was built from items that used to be Buttons.
    Moved to menu bar Dec. 2008 by glp to improve efficiency of gui.
    Intentionally retained controls tabs with  BTN to avoid 
    some potential symbol collisions.*/
    sm.controls[BTN_PICKS_TEDIT]=edit_menu[0].w;
    sm.controls[BTN_PICK_CUTOFF]=edit_menu[1].w;
    sm.controls[BTN_RESTORE]=edit_menu[2].w;
    sm.controls[BTN_POLARITY_SWITCHER]=edit_menu[3].w;
    sm.controls[BTN_MANUAL_PICKING]=edit_menu[4].w;
    sm.controls[BTN_TWEEKER]=edit_menu[5].w;
}

bool SEISPP::SEISPP_verbose=false;
/*******************************************************************************
main: Set up the application
*******************************************************************************/
int
main (int argc, char **argv)
{
  Display	*display;
  Widget       	shell, flrc, slrc, tlrc; 
  Widget	menu_bar, menu_edit, menu_picks, menu_options, menu_file, menu_view;
  Widget 	paned_win, second_paned_win;
  XtAppContext 	AppContext;
  XmString	str;
  MenuItem      btninfo;
  TkSend	*tks=NULL;
  int i,n=0;
  Arg args[18];
  /* Note this struct is defined in the seisw widget display_marker.h */
  // The last entry used to sometimes be false to forbid that an attribute be displayed in a graphic
  // before analysis was completed.  This is now superceded by initializing these after all calls
  // to load_data above to defaults.  This is used for coherence, peak_xcor, stack_weight, ad signal_to_noise_ratio
  /*
  AttributeInfoRec air[6]={ {NULL,"sta",ATTR_STR,false,NULL,-1,false,"Station Name",true},
			    {NULL,"distance_deg",ATTR_DOUBLE,true,NULL,-1,false, "Epicentral distance",true},
			    {NULL,"coherence",ATTR_DOUBLE,true,NULL,-1,false, "Coherence",true},
			    {NULL,"peak_xcor", ATTR_DOUBLE,true,NULL,-1,false, "Peak Cross-correlation",true},
			    {NULL,"stack_weight", ATTR_DOUBLE,true,NULL,-1,false, "Stack Weight",true},
			    {NULL,"signal_to_noise_ratio", ATTR_DOUBLE,true,NULL,-1,false, "Signal to Noise Ratio",true}
			};
                        */

  AttributeInfoRec air[6]={ {NULL,"sta",ATTR_STR,false,NULL,-1,false,"Station Name",true},
			    {NULL,"distance_deg",ATTR_DOUBLE,true,NULL,-1,false, "Epicentral distance",true},
			    {NULL,SEISPP::coherence_keyword,ATTR_DOUBLE,true,NULL,-1,false, "Coherence",true},
			    {NULL,SEISPP::peakxcor_keyword, ATTR_DOUBLE,true,NULL,-1,false, "Peak Cross-correlation",true},
			    {NULL,SEISPP::stack_weight_keyword, ATTR_DOUBLE,true,NULL,-1,false, "Stack Weight",true},
			    {NULL,snrdb_keyword, ATTR_DOUBLE,true,NULL,-1,false, "Signal to Noise Ratio",true}
			};
  char *use=(char *) "db [-appname name -o dbout [-q queuefile | -i infile ] -pf pffile -V -v]";
  char *author=(char *) "Peng Wang and Gary Pavlis";
  char *email=(char *) "pewang@indiana.edu,pavlis@indiana.edu";
  char *loc=(char *) "Indiana University";
  char *rev=(char *) "$Revision 1.13$";
  ios::sync_with_stdio();
  elog_init(argc,argv);

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
  string readfile("");
  string pfname("dbxcor");
  string appname("dbxcor");
  enum InMode {TKSEND, INFILE, QFILE};
  InMode inputmode(TKSEND);
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
      } else if (argtest=="-i") {
	   if(inputmode==QFILE)
	   {
		cerr << "Input argument error.  Must use either -i, -q, or neither."<<endl;
	  	cbanner(rev,use,author,loc,email);
		exit(-1);
	   }
	   ++i;
	   appname=string("");
	   readfile=string(argv[i]);
      } else if (argtest=="-q") {
	   if(inputmode==INFILE)
	   {
		cerr << "Input argument error.  Must use either -i, -q, or neither."<<endl;
	  	cbanner(rev,use,author,loc,email);
		exit(-1);
	   }
	   ++i;
	   appname=string("");
	   readfile=string(argv[i]);
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
      SessionManager sm(pfname,readfile,logstr,waveform_db_name,result_db_name);
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
    {(char *) "Save",&xmPushButtonGadgetClass,'s',(char *) "Ctrl<Key>S",(char *)"Ctrl+S",save_event,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Exit",&xmPushButtonGadgetClass,'x',(char *) "Ctrl<Key>C",(char *)"Ctrl+C",exit_gui,(XtPointer)0,NULL,(MenuItem *)NULL},
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
    {(char *) "Beam Window",&xmPushButtonGadgetClass,'B',(char *)"<Key>B",(char *)"B",pick_bwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Robust Window",&xmPushButtonGadgetClass,'R',(char *)"<Key>R",(char *)"R",pick_rwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *)"Reference Trace",&xmPushButtonGadgetClass,'M',(char *)"<Key>M",(char *)"M",
    	pick_ref_trace,(XtPointer)&sm,NULL,(MenuItem *)NULL},
//    {"View",&xmPushButtonGadgetClass,'V',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)view_submenu},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem edit_menu[]={
    {(char *) "Trace Edit",&xmPushButtonGadgetClass,'T',(char *)"<Key>T",(char *)"T",toggle_edit,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Pick Cutoff",&xmPushButtonGadgetClass,'C',(char *)"<Key>C",(char *)"C",pick_cutoff,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Restore Data",&xmPushButtonGadgetClass,'D',(char *)"<Key>D",(char *)"D",restore_data,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Enable Polarity Editing",&xmPushButtonGadgetClass,'P',(char *)"<Key>P",(char *)"P",enable_polarity_switching,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Enable Manual Picking",&xmPushButtonGadgetClass,'M',NULL,NULL,enable_display_mpicker,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Enable Cycle Skip Picker",&xmPushButtonGadgetClass,'C',NULL,NULL,enable_cycle_skip_picking,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem options_menu[]={
    {(char *) "Sort Options",&xmPushButtonGadgetClass,'r',NULL,NULL,pick_sort_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "Filter Options",&xmPushButtonGadgetClass,'l',NULL,NULL,pick_filter_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *) "MCC Options",&xmPushButtonGadgetClass,'m',NULL,NULL,pick_mcc_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {(char *)"Enable Subarrays",&xmPushButtonGadgetClass,'s',NULL,NULL,
    	subarray_toggle,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  sm.controls[MENU_FILE]=menu_file=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "File",'F',false,file_menu);
  sm.controls[MENU_EDIT]=menu_edit=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "Edit",'P',false,edit_menu);
  sm.controls[MENU_PICKS]=menu_picks=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "Picks",'P',false,picks_menu);
  sm.controls[MENU_OPTIONS]=menu_options=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "Options",'O',false,options_menu);
  sm.controls[MENU_VIEW]=menu_view=BuildMenu(menu_bar,XmMENU_PULLDOWN,(char *) "View",'V',false,view_menu);

  set_menu_controls(file_menu,edit_menu,picks_menu,options_menu,view_menu,sm);

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

  /*create and manage seisw widget.  Note this function alters
  the session manager (sm) by creating a copy of the database
  handle derived from the XcorProcessingEngine.  This violates
  the rule of creation is initialization for an object, but was
  unavoidable. */
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
  	btninfo.label=(char *) "Next Gather";
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

  btninfo.label=(char *) "Plot Beam";
  btninfo.callback=do_beam_plot;
  sm.controls[BTN_BEAM_PLOT]=create_button(tlrc,btninfo);

  btninfo.label=(char *) "Plot Correlation";
  btninfo.callback=do_xcor_plot;
  sm.controls[BTN_XCOR_PLOT]=create_button(tlrc,btninfo);

// Alternate save as button.  Under file menu also
  btninfo.label=(char *) "Save";
  btninfo.callback=save_event;
  sm.controls[BTN_FILE_SAVE]=create_button(tlrc,btninfo);

  XtManageChild(tlrc);

  XtManageChild(paned_win);

  XtManageChild(slrc);
  XtManageChild(flrc);

  XtRealizeWidget(shell);

  sm.session_state(NONE);

  if(tks!=NULL)
  {
  do {
	XEvent event;
	char	*msg = 0;
	int	replyrequest = 0;
	XtAppNextEvent(AppContext,&event);
	long	orid;
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
		orid = atol( (char *) gettbl( parts, 0 ) );
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
  } catch (SeisppError& serr)
  {
	serr.log_error();
	exit(-1);
  }
  catch (...)
  {
	cerr << "dbxcor:  something threw an unhandled exception"<<endl;
  }
}
