
#include "session_manager.h"
#include "dbxcor.h"
#include "Seisw.h"

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
                menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
        else {
                n = 0;
                XtSetArg (args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
                menu = XmCreatePopupMenu (parent, "_popup", args, n);
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
//        return menu;
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

        dialog = XmCreateMessageDialog (parent, "dialog", NULL, 0);
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

        try {
        Metadata global_md(pf);
        AnalysisSetting asetting(global_md);
        sm.asetting_default=asetting;
        sm.xpe=new XcorProcessingEngine(pf,asetting,sm.get_waveform_db_name(),sm.get_result_db_name());
	sm.using_subarrays=sm.xpe->use_subarrays;

        sm.fp=fopen((sm.get_event_file_name()).c_str(),"r");
        if (sm.fp==NULL) throw SeisppError("Failed to open event file\n");

	int n=0;
	Arg args[4];
	XtSetArg(args[n],ExmNzoomFactor,100); n++;
        XtSetArg(args[n],XmNpaneMaximum,20000); n++;
	sm.seismic_widget=ExmCreateSeisw(parent,"Seisw",args,n);
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
    msg_box=XmCreateDialogShell(parent,"Information",args,i);

    i=0;
    XtSetArg (args[i], XmNpacking, XmPACK_TIGHT); i++;
    XtSetArg (args[i], XmNuserData, psm); i++;
    rowcol = XmCreateRowColumn (msg_box, "rowcolumn", args, i);


    text=XmStringCreateLocalized((char*)s.c_str());

    i=0;
    XtSetArg(args[i],XmNlabelString,text); i++;
    label=XmCreateLabel(rowcol,"info",args,i);
    XmStringFree(text);
    XtManageChild(label);

    check=XmCreateToggleButtonGadget(rowcol,"Don't show this message to me again",
			NULL,0);
    XtAddCallback (check, XmNvalueChangedCallback, disable_display, psm);
    XtManageChild(check);

    separator = XmCreateSeparatorGadget (rowcol, "sep",NULL, 0);
    XtManageChild (separator);

    //create the ok button in the action area
    i=0;
    XtSetArg(args[i],XmNfractionBase,3); i++;
    form=XmCreateForm(rowcol,"form",args,i);

    ok_btn=XmCreatePushButtonGadget(form,"OK",NULL,0);
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

void get_next_event(Widget w, void * client_data, void * userdata)
{
	stringstream ss;
	int orid,evid;
        double lat,lon,depth,otime;
        const string method("tttaup");
        const string model("iasp91");
        char yrmonday[15],day[6],hrminsec[20];
   	int i;

	try {

	SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

	psm->record(string("Loading data for next event.... Please wait\n"));

	//mcc is associated with the XcorProcessingEngine, it seems
	//that XcorProcessingEngine's analyze() method does clean up itself,
	//so here if we delete mcc, later we might get a seg fault when
	//next time XcorProcessingEngine tries to do another analyze...
//        if (psm->mcc != NULL) delete psm->mcc;

	if (fscanf(psm->fp,"%d%d%lf%lf%lf%s%s%s",&evid,&orid,
                        &lat,&lon,&depth,
                        yrmonday,day,hrminsec)!=EOF) {
		psm->set_evid(evid);
		psm->set_orid(orid);

                string datestring=string(yrmonday)+" "+string(hrminsec);
                otime=str2epoch(const_cast<char*>(datestring.c_str()));
                Hypocenter h(rad(lat),rad(lon),depth,otime,method,model);
                ss << "Loading data for event: "
                        << lat<<","
                        << lon<<","
                        << depth <<","
                        << datestring<<endl;
                psm->xpe->load_data(h);
		ss << "Data loaded" <<endl;
		if(psm->using_subarrays)
		{
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
		Metadata data_md=psm->xpe->get_data_md();
		stringstream ts;
		ts << lat <<","<<lon<<","<<depth<<","<<datestring;      
		data_md.put("title",ts.str());

		psm->active_setting=psm->asetting_default;
                stringstream vs;
                if (!psm->validate_setting(vs)) message_box(psm, vs.str(), w);

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

	} else {
	    ss << "End of file reached in the event file!"<<endl;
	    exit(0);
	}
	} catch (SeisppError serr) {
                serr.log_error();
                cerr << "Fatal error:  exiting"<<endl;
	}

}

/* 
 * We comment this out since we don't need a separate button down there
 * to do the actual sort, as soon as we click the "OK" button in the sort
 * options menu, we need to do this. So we merge this function with
 * apply_sort_order
void do_sort(Widget w, void * client_data, void * userdata)
{
    TimeSeriesEnsemble * tse;
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    Metadata data_md=psm->xpe->get_data_md();

    psm->xpe->sort_ensemble();
    tse=psm->xpe->get_waveforms_gui();

    data_md.put("title",psm->markers.title);

    XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble,(XtPointer)(tse),
	ExmNseiswMetadata,(XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),NULL);
        
}
*/

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
	default:
	    ss << "ERROR: unknown result sort order"<<endl;
	    break;
    }

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

    XtDestroyWidget(XtParent(XtParent(XtParent(w))));
}

Widget get_top_shell(Widget w)
{
    while(w && !XtIsWMShell(w)) {
	w=XtParent(w);
    }

    return w;
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
    attr_dialog=XmCreateDialogShell(get_top_shell(w),"Attributes to View",args,i);

    i=0;
    XtSetArg (args[i], XmNpacking, XmPACK_TIGHT); i++;
    XtSetArg (args[i], XmNuserData, psm); i++;
    rowcol = XmCreateRowColumn (attr_dialog, "rowcolumn", args, i);

    i = 0;
    XtSetArg (args[i], XmNpacking, XmPACK_COLUMN); i++;
    XtSetArg (args[i], XmNnumColumns, 2); i++;
    check_box = XmCreateRowColumn (rowcol, "Check Box", args, i);

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
 
    separator = XmCreateSeparatorGadget (rowcol, "sep",NULL, 0);
    XtManageChild (separator);

    //create the ok and cancel buttons in the action area
    i=0;
    XtSetArg(args[i],XmNfractionBase,5); i++;
    form=XmCreateForm(rowcol,"form",args,i);

    ok_btn=XmCreatePushButtonGadget(form,"Apply",NULL,0);
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

    cancel_btn=XmCreatePushButtonGadget(form,"Cancel",NULL,0);
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
    Widget radio_box, sort_dialog, pane, form, ok_btn, cancel_btn;
    XmString str_coherence, str_correlation_peak, str_amplitude, str_lag, str_weight;
    XmString str_lat,str_lon,str_time;
    Arg args[10];
    int i, picked, selected;
    Dimension h;
    
    i=0;
    XtSetArg(args[i],XmNdeleteResponse,XmUNMAP); i++;
    sort_dialog=XmCreateDialogShell(get_top_shell(w),"Sort Options",args,i);

    selected=psm->active_setting.result_sort_order;

    //paned window under the dialog shell
    i=0;
    XtSetArg(args[i],XmNsashWidth,1); i++;
    XtSetArg(args[i],XmNsashHeight,1); i++;
    XtSetArg(args[i],XmNuserData,selected); i++;
    pane=XmCreatePanedWindow(sort_dialog,"pane",args,i);

    radio_box=XmCreateRadioBox(pane,"Sort Options",NULL,0);

    Widget wtemp;
    wtemp=XmCreateToggleButtonGadget(radio_box,"lat",NULL,0);
    picked=SITE_LAT;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"lon",NULL,0);
    picked=SITE_LON;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Predicted Time",NULL,0);
    picked=PREDARR_TIME;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"source azimuth",NULL,0);
    picked=ESAZ;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"measured amplitude",NULL,0);
    picked=AMPLITUDE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Computed lag",NULL,0);
    picked=LAG;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }
    SessionState state=psm->get_state();

    wtemp=XmCreateToggleButtonGadget(radio_box,"Coherence",NULL,0);
    picked=COHERENCE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }
   
    wtemp=XmCreateToggleButtonGadget(radio_box,"Peak Correlation",NULL,0);
    picked=CORRELATION_PEAK;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Amplitude",NULL,0);
    picked=AMPLITUDE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Lag",NULL,0);
    picked=LAG;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Distance",NULL,0);
    picked=DISTANCE;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    wtemp=XmCreateToggleButtonGadget(radio_box,"Stack Weight",NULL,0);
    picked=WEIGHT;
    XtAddCallback(wtemp,XmNvalueChangedCallback,sort_picked,(XtPointer)(picked));
    XtManageChild(wtemp);
    if (state != ANALYZE && state != SAVE) XtSetSensitive(wtemp,False);
    if (picked==selected) {
	XtVaSetValues(wtemp,XmNset,XmSET,NULL);
	XtVaSetValues(radio_box,XmNinitialFocus,wtemp,NULL);
    }

    XtManageChild(radio_box);


     //create the ok and cancel buttons in the action area
     i=0;
     XtSetArg(args[i],XmNfractionBase,5); i++;
     form=XmCreateForm(pane,"form",args,i);

     ok_btn=XmCreatePushButtonGadget(form,"Apply",NULL,0);
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
     XtAddCallback(ok_btn,XmNactivateCallback,apply_sort_order,psm);

     cancel_btn=XmCreatePushButtonGadget(form,"Cancel",NULL,0);
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
    char * filter;
    XmSelectionBoxCallbackStruct *cbs=(XmSelectionBoxCallbackStruct *)userdata;

    filter=(char*)XmStringUnparse(cbs->value,XmFONTLIST_DEFAULT_TAG,
				XmCHARSET_TEXT,XmCHARSET_TEXT,NULL,0,XmOUTPUT_ALL);

    if (string(filter)=="") ;
    else {
       try {

	   //Here we don't do redisplay since we assume that after this box disappear,
	   //an automatic redisplay is enabled.
	   psm->xpe->filter_data(TimeInvariantFilter(string(filter)));
	   psm->xpe->sort_ensemble();

       } catch (SeisppError serr) {
           ss   << "Filter_data failed using "
                << filter <<", try again"<<endl;
  	   serr.log_error();
	   psm->record(ss.str());
       }
    }

    XtDestroyWidget(XtParent(w));
}

void pick_filter_options(Widget w, void * client_data, void * userdata)
{
    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);
    Widget dialog;
    XmString text=XmStringCreateLocalized("Enter additional filter string(Antelope convention):");
    XmString content=XmStringCreateLocalized("BW 0.5 5 2 5");
    Arg args[6];
    int n=0;

    XtSetArg(args[n],XmNselectionLabelString,text); n++;
    XtSetArg(args[n],XmNautoUnmanage,False); n++;
    XtSetArg(args[n],XmNtextString,content); n++;
    dialog=XmCreatePromptDialog(get_top_shell(w),"Filter Option",args,n);
    XmStringFree(text);

    XtManageChild(XtNameToWidget(dialog, "Apply"));
    XtUnmanageChild(XtNameToWidget(dialog, "OK"));
    XtUnmanageChild(XtNameToWidget(dialog, "Help"));

    XtAddCallback(dialog,XmNapplyCallback,apply_filter,psm);
    XtAddCallback(dialog,XmNcancelCallback,destroy_callback,dialog);

    XtManageChild(dialog);
        
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
	str = XmStringCreateLocalized ("Stop Trace Edit");
	psm->session_state(TRACE_EDIT);
    } else {
	edit_enable=0; 
	str = XmStringCreateLocalized ("Trace Edit");
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
        str = XmStringCreateLocalized ("Stop Picking Cutoff");
	psm->session_state(PICKING_CUTOFF);
    } else {
        edit_enable=0;
        str = XmStringCreateLocalized ("Pick Cutoff");
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

    try {
        psm->xpe->change_analysis_setting(psm->active_setting);
    	psm->mcc=psm->xpe->analyze();
        psm->xpe->sort_ensemble();
    } catch (SeisppError serr) {
	serr.log_error();
	psm->record(string("Fatal error encountered during analysis...\n"));
	return;
    }

    Metadata data_md=psm->xpe->get_data_md();
    TimeSeriesEnsemble * tse=psm->xpe->get_waveforms_gui();

    data_md.put("title",psm->markers.title);

    XtVaSetValues(psm->seismic_widget,ExmNseiswEnsemble,(XtPointer)(tse),
       ExmNseiswMetadata,(XtPointer)(&data_md),ExmNdisplayMarkers,&(psm->markers),NULL);

    psm->session_state(ANALYZE);

    psm->record(string("Done\n"));
}


void do_beam_plot(Widget w, void * client_data, void * userdata)
{
    Widget rshell_beam, beam_widget, pane, form, btn_arrival;     
    XWindowAttributes xwa;
    Dimension h;
    int n;
    Arg args[10];

    SessionManager * psm=reinterpret_cast<SessionManager *>(client_data);

    rshell_beam=XtVaCreatePopupShell ("Beam Plot",topLevelShellWidgetClass, get_top_shell(w),
                XmNtitle,"Beam Plot",XmNallowShellResize,True,XmNwidth,800,XmNheight,200,
                XmNdeleteResponse,XmUNMAP,NULL);

    n=0;
    XtSetArg(args[n],XmNwidth,400); n++;
    XtSetArg(args[n],XmNheight,100); n++;
    pane=XmCreatePanedWindow(rshell_beam,"Pane",args,n);

    TimeSeries beam=psm->mcc->ArrayBeam();
    TimeSeriesEnsemble *beam_tse=new TimeSeriesEnsemble(1,beam.ns);
    beam_tse->member[0]=beam;

    Metadata beam_display_md=psm->xpe->get_beam_md();
    beam_display_md.put("title",psm->markers.title);

    n=0;
    XtSetArg(args[n],ExmNdisplayOnly,1); n++;
    XtSetArg(args[n],ExmNseiswEnsemble,static_cast<XtPointer>(beam_tse));n++;
    XtSetArg(args[n],ExmNseiswMetadata,&beam_display_md); n++;
    XtSetArg(args[n],ExmNcleanupData, static_cast<XtPointer>(beam_tse));n++;
    beam_widget=ExmCreateSeisw(pane,"Beam Plot",args,n);

    XtManageChild(beam_widget);
    XtVaSetValues(beam_widget,XmNpaneMinimum,100,NULL);

    n=0;
    XtSetArg(args[n],XmNuserData,psm); n++;
    form=XmCreateForm(pane,"form",args,n);   
    btn_arrival=XmCreatePushButtonGadget(form,"Pick Arrival",NULL,0);
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

    XtPopup (rshell_beam, XtGrabNone);

    /* See if the dialog is realized and is visible.  If so, pop it down */
/*
    if (XtIsRealized (rshell_beam) && XGetWindowAttributes
            (XtDisplay (rshell_beam), XtWindow (rshell_beam), &xwa) &&
            xwa.map_state == IsViewable)
            XtPopdown (rshell_beam);
    else
            XtPopup (rshell_beam, XtGrabNone);
*/
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

    begin=MAX((ai->x2begb-(int)ai->x2begb==0.0) ? (int)ai->x2begb : (int)ai->x2begb+1,0);
    end=MIN((int)ai->x2endb,tse->member.size());


    if (psm->attributes_info[index].use_graph) {
                double *x1, *x2;
                int height, line;

                x1=new double[end-begin+1];
                x2=new double[end-begin+1];

                name=psm->attributes_info[index].name;
                for(i=begin; i<=end; i++) {
                    x1[i-begin]=tse->member[i-1].get_double(name.c_str());
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
			end-begin+1,x1,x2,"");

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
	 	update_attributes_display(wdgt,client_data,NULL);
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

		    psm->attributes_info[i].line=SciPlotListCreateFromDouble(plotWidget,1,x,y,"");

		    psm->attributes_info[i].graph_widget=plotWidget;
		} else {
		    XtDestroyWidget(psm->attributes_info[i].w);
		    wdgt=psm->attributes_info[i].w=XmCreateDrawingArea(psm->parent,"attribute",NULL,0);
		}

		XtManageChild(wdgt);
		XtVaSetValues(wdgt,XmNpaneMinimum,50,XmNpositionIndex,pos_indx,NULL);

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
	sm.attributes_info[i].w=XmCreatePanedWindow(sm.parent,"attribute",NULL,0);
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
    XtSetArg(args[n],ExmNdisplayOnly,1); n++;
    XtSetArg(args[n],ExmNseiswEnsemble,&(psm->mcc->xcor));n++;
    XtSetArg(args[n],ExmNseiswMetadata,&xcor_display_md); n++;
    xcor_widget=ExmCreateSeisw(rshell_xcor,"Correlation Plot",args,n);

    XtManageChild(xcor_widget);
    XtVaSetValues(xcor_widget,XmNpaneMinimum,100,NULL);

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
    try {
          psm->xpe->save_results(evid,orid);
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
    ss << "Resetting original data"<<endl;
    psm->record(ss.str());
}

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
		ss << "No more subarrays for this event.  Push button to read next event";
	}
        psm->record(ss.str());
}


void exit_gui(Widget w, void * uesless1, void * useless2)
{
    exit(0);
}


void usage(char ** argv)
{
        cerr << argv[0]<<" dbin dbout hypofile [-pf pffile] " <<endl;
        exit(-1);
}

void set_menu_controls(MenuItem * file_menu, MenuItem * picks_menu, MenuItem * options_menu, 
      	MenuItem * settings_menu, MenuItem * view_menu, SessionManager &sm) 
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
    sm.controls[MENU_SETTINGS_PF]=settings_menu[0].w;
    sm.controls[MENU_VIEW_SNAME]=view_menu[0].w;
    sm.controls[MENU_VIEW_COHERENCE]=view_menu[1].w;
    sm.controls[MENU_VIEW_PCORRELATION]=view_menu[2].w;
    sm.controls[MENU_VIEW_SWEIGHT]=view_menu[3].w;
}


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
  int i,n=0;
  Arg args[18];
  AttributeInfoRec air[4]={ {NULL,"sta",ATTR_STR,false,NULL,-1,false,"Station Name",true},
			    {NULL,"coherence",ATTR_DOUBLE,true,NULL,-1,false, "Coherence",false},
			    {NULL,"peak_xcor", ATTR_DOUBLE,true,NULL,-1,false, "Peak Cross-correlation",false},
			    {NULL,"stack_weight", ATTR_DOUBLE,true,NULL,-1,false, "Stack Weight",false}
			};

  if(argc<4) usage(argv);
  string waveform_db_name(argv[1]);
  string result_db_name(argv[2]);
  string hypofile(argv[3]);
  string pfname("dbxcor");
  for(i=4;i<argc;++i) {
      string argtest(argv[i]);
      if(argtest=="-pf") {
           ++i;
           pfname=string(argv[i]);
      } else {
           usage(argv);
      }
  }
  string logstr(LOGNAME);

  SessionManager sm(pfname,hypofile,logstr,waveform_db_name,result_db_name);
  sm.attributes_info.push_back(air[0]);
  sm.attributes_info.push_back(air[1]);
  sm.attributes_info.push_back(air[2]);
  sm.attributes_info.push_back(air[3]);

 /* Do standard Motif application start-up. */
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
			     

  //create first level form widget
  flrc=XmCreateForm(shell,APP_CLASS,NULL,0);

  //first row of the flrc
  /* Create the menu bar */
  menu_bar = XmCreateMenuBar(flrc, "menuBar", NULL, 0);
  XtVaSetValues (menu_bar, XmNtopAttachment,  XmATTACH_FORM, XmNleftAttachment, XmATTACH_FORM, 
	XmNrightAttachment, XmATTACH_FORM, NULL);

  MenuItem file_menu[]={
    {"Save",&xmPushButtonGadgetClass,'s',"Ctrl<Key>S",NULL,save_event,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {"Exit",&xmPushButtonGadgetClass,'x',"Ctrl<Key>C",NULL,exit_gui,(XtPointer)0,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

/*
  MenuItem view_submenu[]={
    {"Attributes",&xmPushButtonGadgetClass,'A',NULL,NULL,pick_attributes,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {"Analysis Settings",&xmPushButtonGadgetClass,'e',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };
*/
  MenuItem view_menu[]={
/*
    {"Attributes",&xmPushButtonGadgetClass,'A',NULL,NULL,pick_attributes,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {"Analysis Settings",&xmPushButtonGadgetClass,'e',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)NULL},
*/

    {(char *)air[0].display_name.c_str(),&xmToggleButtonWidgetClass,'n',NULL,NULL,viewmenu_toggle_0,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[1].display_name.c_str(),&xmToggleButtonWidgetClass,'c',NULL,NULL,viewmenu_toggle_1,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[2].display_name.c_str(),&xmToggleButtonWidgetClass,'p',NULL,NULL,viewmenu_toggle_2,&sm,NULL,(MenuItem *)NULL},
    {(char *)air[3].display_name.c_str(),&xmToggleButtonWidgetClass,'s',NULL,NULL,viewmenu_toggle_3,&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem picks_menu[]={
    {"Beam Window",&xmPushButtonGadgetClass,'B',NULL,NULL,pick_bwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {"Robust Window",&xmPushButtonGadgetClass,'R',NULL,NULL,pick_rwindow,(XtPointer)&sm,NULL,(MenuItem *)NULL},
//    {"View",&xmPushButtonGadgetClass,'V',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)view_submenu},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  MenuItem options_menu[]={
    {"Sort Options",&xmPushButtonGadgetClass,'r',NULL,NULL,pick_sort_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {"Filter Options",&xmPushButtonGadgetClass,'l',NULL,NULL,pick_filter_options,(XtPointer)&sm,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };
  MenuItem settings_menu[]={
    {"Pf File",&xmPushButtonGadgetClass,'P',NULL,NULL,NULL,(XtPointer)0,NULL,(MenuItem *)NULL},
    {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}
  };

  sm.controls[MENU_FILE]=menu_file=BuildMenu(menu_bar,XmMENU_PULLDOWN,"File",'F',false,file_menu);
  sm.controls[MENU_PICKS]=menu_picks=BuildMenu(menu_bar,XmMENU_PULLDOWN,"Picks",'P',false,picks_menu);
  sm.controls[MENU_OPTIONS]=menu_options=BuildMenu(menu_bar,XmMENU_PULLDOWN,"Option",'O',false,options_menu);
  sm.controls[MENU_VIEW]=menu_view=BuildMenu(menu_bar,XmMENU_PULLDOWN,"View",'V',false,view_menu);
  sm.controls[MENU_SETTINGS]=menu_settings=BuildMenu(menu_bar,XmMENU_PULLDOWN,"Setting",'t',false,settings_menu);

  set_menu_controls(file_menu,picks_menu,options_menu,settings_menu,view_menu,sm);

  /* Menubar is done -- manage it */
  XtManageChild (menu_bar);

  //second row of flrc
  //create second level form widget
  n=0;
  XtSetArg (args[n], XmNfractionBase, MAINFORM_GRID_CNT); n++;
  slrc = XmCreateForm (flrc, "form",   args, n);
  XtVaSetValues (slrc,
                 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
                 XmNtopAttachment,  XmATTACH_WIDGET,
		 XmNtopWidget, menu_bar,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
  
  //create paned window 
  n=0;
  XtSetArg(args[n],XmNorientation,XmVERTICAL); n++;
  XtSetArg(args[n],XmNseparatorOn,True); n++;
//  XtSetArg(args[n],XmNallowResize,False); n++;
  XtSetArg (args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNtopPosition, 0); n++;
  XtSetArg (args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNleftPosition, 0); n++;
  XtSetArg (args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNrightPosition, MAINFORM_GRID_CNT); n++;
  XtSetArg (args[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
  XtSetArg (args[n], XmNbottomPosition, MAINFORM_GRID_CNT-1); n++;

  paned_win=XmCreatePanedWindow(slrc,"pane", args, n);

  //Use the second paned window to ensure that when attributes are 
  //displayed, they are laid out horizontally.
  n=0;
  XtSetArg(args[n],XmNorientation,XmHORIZONTAL); n++;
  XtSetArg(args[n],XmNseparatorOn,False); n++;
  XtSetArg(args[n],XmNspacing,0); n++;
  XtSetArg(args[n],XmNallowResize,True); n++;
  XtSetArg(args[n],XmNwidth,2000); n++;
  second_paned_win=XmCreatePanedWindow(paned_win, "pane1", args, n); 
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
  sm.msg_w = XmCreateScrolledText (paned_win, "msg_w", args, n);
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

  tlrc=XmCreateRowColumn(slrc,"third",args,n);

  XtVaSetValues(tlrc,XmNorientation,XmHORIZONTAL,
        XmNentryAlignment,XmALIGNMENT_BEGINNING,XmNpacking,XmPACK_TIGHT,NULL);


  btninfo.label="Get Next Event";
  btninfo.callback=get_next_event;
  btninfo.callback_data=&sm;
  sm.controls[BTN_NEXTEV]=create_button(tlrc,btninfo);  

  btninfo.label="Load Next Subarray";
  btninfo.callback=load_next_subarray;
  btninfo.callback_data=&sm;
  sm.controls[BTN_NEXTSUB]=create_button(tlrc,btninfo);  

  btninfo.label="Pick Ref Trace";
  btninfo.callback=pick_ref_trace;
  sm.controls[BTN_REF]=create_button(tlrc,btninfo);

  btninfo.label="Analyze";
  btninfo.callback=do_analyze;
  sm.controls[BTN_ANALYZE]=create_button(tlrc,btninfo);

  btninfo.label="Trace Edit";
  btninfo.callback=toggle_edit;
  sm.controls[BTN_PICKS_TEDIT]=create_button(tlrc,btninfo);

  btninfo.label="Plot Beam";
  btninfo.callback=do_beam_plot;
  sm.controls[BTN_BEAM_PLOT]=create_button(tlrc,btninfo);

  btninfo.label="Plot Correlation";
  btninfo.callback=do_xcor_plot;
  sm.controls[BTN_XCOR_PLOT]=create_button(tlrc,btninfo);

  btninfo.label="Restore Data";
  btninfo.callback=restore_data;
  sm.controls[BTN_RESTORE]=create_button(tlrc,btninfo);

  btninfo.label="Pick Cutoff";
  btninfo.callback=pick_cutoff;
  sm.controls[BTN_PICK_CUTOFF]=create_button(tlrc,btninfo);

// Alternate save as button.  Under file menu also
  btninfo.label="Save";
  btninfo.callback=save_event;
  sm.controls[BTN_FILE_SAVE]=create_button(tlrc,btninfo);

  XtManageChild(tlrc);

  XtManageChild(paned_win);

  XtManageChild(slrc);
  XtManageChild(flrc);

  XtRealizeWidget(shell);

  sm.session_state();

  XtAppMainLoop(AppContext);
}
