// Needed only for debug.  Eventually should go
#include <iostream>
#include <X11/Xthreads.h>
#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include "SeismicPlot.h"
/* This is needed here because this class is also defined in this file.  
   This was necessary to allow both classes to use the BuildMenu procedure. */
#include "TraceEditPlot.h"
using namespace std;
using namespace SEISPP;
#define APP_CLASS "seismicplot"
// this constant cloned from dbxcor
#define MAINFORM_GRID_CNT 15
/* This is cloned from dbxcor.  It is used as a compact data structure to sent to 
   a procedure immediately below (BuildMenu) that constructs a widget based on this specification*/
typedef struct _menu_item
{
        char              *label;          /* the label for the item */
        WidgetClass       *class1;          /* pushbutton, label, separator... */
        char               mnemonic;       /* mnemonic; NULL if none */
        char              *accelerator;    /* accelerator; NULL if none */
        char              *accel_text;     /* to be converted to compound string */
        void             (*callback)(Widget,void *,void *);    /* routine to call; NULL if none */
        XtPointer          callback_data;  /* client_data for callback() */
        Widget           w;
        struct _menu_item *subitems;       /* pullright menu items, if not NULL */
} MenuItem;

/* Pulldown menus are built from cascade buttons, so this function
** also includes pullright menus. Create the menu, the cascade button
** that owns the menu, and then the submenu items.

This is a copy from dbxcor.  I think it originally came from some X cookbook. 
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
void continue_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    SeismicPlot *plot_handle=reinterpret_cast<SeismicPlot *>(client_data);
    plot_handle->ExitDisplay();
}
/* This is a private method used by both constructors.  It provides
   a convenient way to standardize this without requiring a copy
   operation which would be deadly here. */
void SeismicPlot::initialize_widgets(Metadata& md)
{
    try{
        ThreeComponentMode=md.get_bool("ThreeComponentMode");
        comp0=NULL;  comp1=NULL;  comp2=NULL;
        argc=1;
        argv[0]=strdup("SeismicPlotWidget");
        XtSetLanguageProc(NULL, NULL, NULL);
        XtToolkitThreadInitialize();
        toplevel = XtVaAppInitialize(&AppContext, (char *)"seismicplot",NULL,0,
                                &argc,argv, NULL,NULL);
        main_w=XmCreateForm(toplevel,(char *) "seismicplot",NULL,0);
        EventLoopIsActive=false;
        menu_bar=XmCreateMenuBar(main_w,(char *)"menuBar",NULL,0);
        XtVaSetValues (menu_bar, XmNtopAttachment,  XmATTACH_FORM, XmNleftAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM, NULL);
        MenuItem file_menu[]={
            {(char *) "Exit Event Loop",&xmPushButtonGadgetClass,'x',
                (char *)"<Key>X",(char *)"X",
                continue_callback,this,NULL,(MenuItem *)NULL},
                 {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}};
        menu_file=BuildMenu(menu_bar,XmMENU_PULLDOWN,
                (char *)"Control",'C',false,file_menu);
        XtManageChild(menu_bar);
        /* We create a secondary form which will hold the 3 seismic components within it in 
           a paned window.  We'll create that pair now one after the other. */
        Arg args[4];
        int n=0;
        XtSetArg(args[n],XmNfractionBase,MAINFORM_GRID_CNT); n++;
        slrc = XmCreateForm(main_w, (char *) "form", args,n);
        XtVaSetValues (slrc,
                 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
                 XmNtopAttachment,  XmATTACH_WIDGET,
		 XmNtopWidget, menu_bar,
                 XmNbottomAttachment, XmATTACH_FORM,
                 NULL);
        XtManageChild(slrc);
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
        	XmNbottomPosition,		MAINFORM_GRID_CNT-1,
        	NULL);
        XtManageChild(paned_win);
        /* Now build empty seisw widgets */
        n=0;
        XtSetArg(args[n],(char *) ExmNdisplayOnly,1); n++;
        XtSetArg(args[n],(char *) ExmNzoomFactor,100); n++;
        XtSetArg(args[n],XmNpaneMaximum,1000); n++;
        XtSetArg(args[n],XmNpaneMinimum,100); n++;
        seisw[0]=ExmCreateSeisw(paned_win,(char *)"Seisw0",args,n);
        XtManageChild(seisw[0]);
        if(ThreeComponentMode)
        {
            seisw[1]=ExmCreateSeisw(paned_win,(char *)"Seisw1",args,n);
            XtManageChild(seisw[1]);
            seisw[2]=ExmCreateSeisw(paned_win,(char *)"Seisw2",args,n);
            XtManageChild(seisw[2]);
        }
        else
        {
            seisw[1]=NULL;
            seisw[2]=NULL;
        }
        XtManageChild(main_w);
        XtRealizeWidget(toplevel);
    } catch (...){throw;}
}
/* Default constructor gets its parameter information from the
standard antelope parameter file location under a frozen name 
(SeismicPlot.pf).  */
SeismicPlot::SeismicPlot()
{
    cerr << "Entered default constructor"<<endl;
    Pf *pf;
    const string pfglobal_name("SeismicPlot");
    if(pfread(const_cast<char *>(pfglobal_name.c_str()),&pf))
        throw SeisppError("SeismicPlot constructor:  pfread failed for "
                + pfglobal_name);
    try {
	Metadata md(pf);
	pffree(pf);
        this->initialize_widgets(md);
        EventLoopIsActive=false;
        block_till_exit_pushed=true;   // default 
    } catch (...) {throw;}
}
/* Construct from a Metadata object. */
SeismicPlot::SeismicPlot(Metadata& md) : Metadata(md)
{
    try {
        this->initialize_widgets(md);
        EventLoopIsActive=false;
        block_till_exit_pushed=true;
    } catch(...) {throw;}
}
/* Destructor  is not trivial here */
SeismicPlot::~SeismicPlot()
{
    cerr << "Entering destructor"<<endl;
    /* This may not be necessary, but probably prudent to avoid a deadlock */
    if(EventLoopIsActive) this->ExitDisplay();
    if(comp0!=NULL) delete comp0;
    if(comp1!=NULL) delete comp1;
    if(comp2!=NULL) delete comp2;
    /* As I read it destroy the top level widget and destroy all the children. 
       Hopefully this will not create a seg fault */
    /* Using this creates problems.  Web research suggests this is probably 
       because motif widgets are built with their own destructors.  With this
       the program seg faults when this destructor is called. 
    XtDestroyWidget(toplevel);
    */
}
void SeismicPlot::plot(TimeSeriesEnsemble& d,bool block_for_event)
{
    try{
        const string base_error("SeismicPlot::plot method:  ");
        /* The Seisw widget only works with relative time.   Abort
           if any seismograms are on an absolute time base */
        for(int i=0;i<d.member.size();++i)
            if(d.member[i].tref==absolute)
                throw SeisppError(base_error
                        + "input TimeSeriesEnsemble has absolute time set\n"
                        + "Implementation only supports relative time.\n"
                        + "Convert all times to relative with ator");
	XtAppLock(AppContext);
        /* Make an internal copy of d managed by the object to be 
           consistent with 3C data.  We intentionally do not manage
           the comp1 and comp2 pointers.   */
        if(comp0!=NULL) delete comp0;
        comp0=new TimeSeriesEnsemble(d);
        XtVaSetValues(seisw[0],ExmNseiswEnsemble, (XtPointer) (comp0),
            ExmNseiswMetadata,(XtPointer)(dynamic_cast<Metadata*>(this)), NULL);
	XtAppUnlock(AppContext);
        if(block_for_event) 
            block_till_exit_pushed=true;
        else
            block_till_exit_pushed=false;
        cerr <<"starting event handler"<<endl;
        if(!EventLoopIsActive) 
            this->launch_Xevent_thread_handler();
    }
    catch(...){throw;}
}
void SeismicPlot::refresh()
{
    try {
	XtAppLock(AppContext);
        int i;
        for(i=0;i<3;++i)
        {
            if(seisw[i]!=NULL) XtVaSetValues(seisw[i],ExmNseiswMetadata, 
                    (XtPointer)(dynamic_cast<Metadata*>(this)), NULL);
        }
        XtAppUnlock(AppContext);
    }catch(...) {throw;}
}
/* TimeSeries and ThreeComponentSeismograms are plotted in window 1
   using the same method as above.  Do this by simply creating a temporary
   ensemble object and calling that method */
void SeismicPlot::plot(TimeSeries& d,bool block_for_event)
{
    try {
        if(d.tref == absolute)
            throw SeisppError(string("SeismicPlot::plot method for ")
                    + "TimeSeries object:  "
                    + "Implementation only accepts data with relative time");
        TimeSeriesEnsemble e;
        e.member.push_back(d);
        this->plot(e,block_for_event);
    } catch(...){throw;}
}
void SeismicPlot::plot(ThreeComponentSeismogram& d,bool block_for_event)
{
    try {
        if(d.tref == absolute)
            throw SeisppError(string("SeismicPlot::plot method for ")
                    + "ThreeComponentSeismogram object:  "
                    + "Implementation only accepts data with relative time");
        ThreeComponentEnsemble dtmp;
        dtmp.member.push_back(d);
        /*this assumes dtmp will be copied to 3 components "comp" in private 
          are of this object.  This is a potential maintenance issue if that implementation
          changes to be aware */
        this->plot(dtmp,block_for_event);
    }catch(...){throw;}
}
void SeismicPlot::plot(ThreeComponentEnsemble& d,bool block_for_event)
{
    try {
        const string base_error("SeismicPlot::plot method:  ");
        /* The Seisw widget only works with relative time.   Abort
           if any seismograms are on an absolute time base */
        for(int i=0;i<d.member.size();++i)
            if(d.member[i].tref==absolute)
                throw SeisppError(base_error
                        + "input ThreeComponentEnsemble has absolute time set\n"
                        + "Implementation only supports relative time.\n"
                        + "Convert all times to relative with ator");
        if(!ThreeComponentMode) throw SeisppError(base_error
                + "Trying to plot 3c mode with ThreeComponentMode not set.  Fix pf");
        cerr << "Entering 3c plot method"<<endl;
        int k;
	XtAppLock(AppContext);
        for(k=0;k<3;++k)
        {
            auto_ptr<TimeSeriesEnsemble> c;
            c=ExtractComponent(d,k);
            /* This proved necessary to get around a mysterious behaviour I never figured out
               if I tried to save the raw auto_ptr. I suspect it is related to the single owner
               property of an auto_ptr which does not mesh well with passing data to the widget or
               keeping it stored in this object.  We thus do the very inefficient thing and copy
               the ensemble to a raw pointer.  */
            TimeSeriesEnsemble *tstmp=new TimeSeriesEnsemble(*c);
            switch (k)
            {
                case 1:
                    if(comp1!=NULL) delete comp1;
                    comp1=tstmp;
                    XtVaSetValues(seisw[k],ExmNseiswEnsemble, (XtPointer) (comp1),
                        ExmNseiswMetadata,(XtPointer)(dynamic_cast<Metadata*>(this)), NULL);
                    break;
                case 2:
                    if(comp2!=NULL) delete comp2;
                    comp2=tstmp;
                    XtVaSetValues(seisw[k],ExmNseiswEnsemble, (XtPointer) (comp2),
                        ExmNseiswMetadata,(XtPointer)(dynamic_cast<Metadata*>(this)), NULL);
                    break;
                case 0:
                default:
                    if(comp0!=NULL) delete comp0;
                    comp0=tstmp;
                    XtVaSetValues(seisw[k],ExmNseiswEnsemble, (XtPointer) (comp0),
                        ExmNseiswMetadata,(XtPointer)(dynamic_cast<Metadata*>(this)), NULL);
            }
        }

	XtAppUnlock(AppContext);
        if(block_for_event) 
            block_till_exit_pushed=true;
        else
            block_till_exit_pushed=false;
        if(!EventLoopIsActive) 
            this->launch_Xevent_thread_handler();
    }catch(...){throw;}
}
void EventHandler(SeismicPlot *plot_handle)
{
    XtAppLock(plot_handle->AppContext);
        do {
            XEvent event;
            XtAppNextEvent(plot_handle->AppContext,&event);
            XtDispatchEvent(&event);
        }while (plot_handle->WindowIsActive());
    XtAppUnlock(plot_handle->AppContext);
}
void SeismicPlot::launch_Xevent_thread_handler()
{
    EventLoopIsActive=true;  // This may need a mutex
    boost::thread Xevthrd(boost::bind(&EventHandler,this));
    if(block_till_exit_pushed)
    {
        Xevthrd.join();
        EventLoopIsActive=false;
    }
}
/***** BEGIN TraceEditPlot code ************/

/* This is a pair of labels that are posted on the edit menu 
   displaying edit mode.  Edit is a toggle and this label will change
   back and forth between these two codes */
const string cutlabel("Switch to Single Trace Edit");
const string stlabel("Switch to Cutoff Edit Mode");
/* This callback does nothing.  It is purely a placeholder required
   by the X interface to make btn2 active.  At least I don't see a way
   to avoid it.  This is null because the widget knows about time series 
   objects and the concept of the live boolean.  It has wired in it a
   feature that toggles the live varibles when btn2 is pushed. */
void ecb(Widget w, XtPointer client_data, XtPointer userdata)
{
    cerr << "ecb callback entered"<<endl;
}
void TraceEditPlot::edit_enable()
{
    /*Always initialize the widget in single trace mode which is 
      what editon==1 means.  Set to 2 to enable cutoff edit function */
    int editon(1);
    for(int k=0;k<3;++k)
    {
        if(seisw[k]!=NULL)
        {
            XtVaSetValues(seisw[k],ExmNeditEnable,editon,NULL);
            XtRemoveAllCallbacks(seisw[k],ExmNbtn2Callback);
            // data pointer or this callback is intentionally NULL because
            // callback does nothing
            XtAddCallback(seisw[k],ExmNbtn2Callback,ecb,NULL);
        }
    }
}
void toggle_edit_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    TraceEditPlot *plot_handle=reinterpret_cast<TraceEditPlot *>(client_data);
    plot_handle->toggle_edit_mode();
}
void TraceEditPlot::toggle_edit_mode()
{
    int k;
    XmString str;
    int kmax;
    if(ThreeComponentMode)
        kmax=3;
    else
        kmax=1;
    for(k=0;k<kmax;++k)
    {
        XtRemoveAllCallbacks(this->seisw[k],ExmNbtn2Callback);
        /* In each block below this is a toggle.  i.e. if one mode
           switch to the other */
	if(edit_mode==SINGLE_TRACE_EDIT_MODE)
	{
            edit_mode=CUTOFF_EDIT_MODE;
            str = XmStringCreateLocalized(const_cast<char *>(cutlabel.c_str()));
	}
	else if(edit_mode==CUTOFF_EDIT_MODE)
	{
            edit_mode=SINGLE_TRACE_EDIT_MODE;
            str = XmStringCreateLocalized(const_cast<char *>(stlabel.c_str()));
	}
	else
	{
	    /* Perhaps should throw an exception here, but instead we force
	       default of single trace edit mode if edit_mode is mangled.
	       This makes the result more robust but potentially mysterious
	       if edit_mode were overwritten */
            edit_mode=SINGLE_TRACE_EDIT_MODE;
            str = XmStringCreateLocalized(const_cast<char *>(cutlabel.c_str()));
	}
        XtVaSetValues(this->seisw[k],ExmNeditEnable,edit_mode,NULL);
        /* This is currently a do nothing callback.  In edit mode nothing
           really needs to be reported with seisw as the plot is updated with
           the killed traces zeroed and displayed red. */
        XtAddCallback(this->seisw[k],ExmNbtn2Callback,ecb,NULL);
        /* Set the label for the edit menu whether we toggle or not. */
        XtVaSetValues(this->menu_edit,XmNlabelString,str,NULL);
    }
}

void TraceEditPlot::build_edit_menu()
{
    /* Default forces single trace edit mode so use that label here. */
    MenuItem edit_menu[]={
        {const_cast<char *>(stlabel.c_str()),
        &xmPushButtonGadgetClass,'c',
        (char *)"<Key>C",(char *)"C",
        toggle_edit_callback,
        this,NULL,(MenuItem *)NULL},
        {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}};
    menu_edit=BuildMenu(menu_bar,XmMENU_PULLDOWN,
            (char *)"Edit Mode",'E',
            false,edit_menu);    
    XtManageChild(menu_bar);
}

TraceEditPlot::TraceEditPlot() : SeismicPlot()
{
    /* Always initialize in single trace edit mode */
    edit_mode=SINGLE_TRACE_EDIT_MODE;
    this->edit_enable();
    this->build_edit_menu();
}
TraceEditPlot::TraceEditPlot(Metadata& md) : SeismicPlot(md)
{
    /* Always initialize in single trace edit mode */
    edit_mode=SINGLE_TRACE_EDIT_MODE;
    this->edit_enable();
    this->build_edit_menu();
}
set<int> TraceEditPlot::report_kills()
{
    set<int> result;
    int i;
    vector<TimeSeries>::iterator dptr;
    if(comp0!=NULL)
    {
        for(dptr=comp0->member.begin(),i=0;
          dptr!=comp0->member.end();++dptr,++i)
            if(!dptr->live) result.insert(i);
    }
    if(comp1!=NULL)
    {
        for(dptr=comp1->member.begin(),i=0;
          dptr!=comp1->member.end();++dptr,++i)
            if(!dptr->live) result.insert(i);
    }
    if(comp2!=NULL)
    {
        for(dptr=comp2->member.begin(),i=0;
          dptr!=comp2->member.end();++dptr,++i)
            if(!dptr->live) result.insert(i);
    }
    //DEBUG
    set<int>::iterator ikill;
    cout << "TraceEditPlot returns this kill list:  ";
    for(ikill=result.begin();ikill!=result.end();++ikill)
    {
        cout << *ikill <<", ";
    }
    return(result);
}
