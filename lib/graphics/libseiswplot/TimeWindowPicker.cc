#include "TimeWindowPicker.h"
#include "seiswplot.h"
using namespace SEISPP;
namespace SEISPP {

void pick_tw_callback(Widget w, XtPointer client_data, XtPointer userdata)
{
    cerr << "Entering pick_tw_callback"<<endl;
    TimeWindowPicker *picker_handle=reinterpret_cast<TimeWindowPicker*>
        (client_data);
    TimeWindow twinpicked;
    SeismicPick *spick;
    XtVaGetValues(w,ExmNseiswPick,&spick,NULL);
    if(spick==NULL)
    {
        cerr << "pick_tw_callback return null pointer - coding problem"
            <<endl;
        return;
    }
    try {
        twinpicked=spick->get_window();
    } catch(SeisppError& serr)
    {
        serr.log_error();
        cerr << "Time Window currently set is not valid"<<endl
            << "Code fix required"<<endl;
    }
    // Silently drop a pick if not properly tagged.  
    if(spick->type == WINDOW) 
        picker_handle->setpick(twinpicked);

    cerr << "Leaving pick_tw_callback"<<endl;
}
void arm_tw_pick_callback(Widget w, 
        XtPointer client_data, XtPointer userdata)
{
    cerr << "Entering arm_tw_pick_callback"  <<endl;
    TimeWindowPicker *picker_handle=reinterpret_cast<TimeWindowPicker*>
        (client_data);
    picker_handle->enable_blocking();
    //DEBUG - try preset of markers
    TimeWindow tmp(5.0,30.0);
    picker_handle->setpick(tmp);
    picker_handle->enable_picking();
    cerr << "Exiting arm_tw_pick_callback"  <<endl;
}
void TimeWindowPicker::enable_picking()
{
    cerr << "Entering enable_picking"  <<endl;
    this->block_till_exit_pushed=true;
    int k,kmax;
    if(ThreeComponentMode)
        kmax=3;
    else
        kmax=1;
    for(k=0;k<kmax;++k)
    {
        cerr << "Adding callbacks for seisw widget number "<<k<<endl;
        XtRemoveAllCallbacks(seisw[k],ExmNbtn3Callback);
        XtAddCallback(seisw[k],ExmNbtn3Callback,pick_tw_callback,(XtPointer)this);
    }
    //DEBUG - using routine from internet search
    for(k=0;k<kmax;++k)
    {
        cerr << "Testing seisw widget number "<<k<<endl;
        XtCallbackStatus istat;
        istat=XtHasCallbacks(seisw[k],ExmNbtn3Callback);
        switch(istat)
        {
            case XtCallbackNoList:
                cerr << "Returned XtCallbackNoList"<<endl;
                break;
            case XtCallbackHasNone:
                cerr << "Returned XtCallbackHasNone"<<endl;
                break;
            case XtCallbackHasSome:
                cerr << "Returned XtCallbackHasSome"<<endl;
                cerr << "Calling directly"<<endl;
                XtCallCallbacks(seisw[k],ExmNbtn3Callback,(XtPointer)this);
                cerr << "Call to ExmNbtn3Callback done"<<endl;
                break;
            default:
                cerr << "Returned unrecognized value"<<endl;
        };
    }
    cerr << "Exiting enable_picking"  <<endl;
}
TimeWindowPicker::TimeWindowPicker() : SeismicPlot(), picked_window()
{
    build_pick_menu();
    pick_completed=false;
}
TimeWindowPicker::TimeWindowPicker(Metadata& md) : SeismicPlot(md) ,
    picked_window()
{
    cerr << "Entering TimeWindowPicker(Metadata) constructor"<<endl;
    build_pick_menu();
    pick_completed=false;
    cerr << "Exiting TimeWindowPicker(Metadata) constructor"<<endl;
}
TimeWindow TimeWindowPicker::get()
{
    if(!pick_completed)throw SeisppError(
       string("TimeWindowPicker get method:  ")
      +string("no time window was picked.\n")
      +string("Error handler should allow you to try again"));
    pick_completed=false;
    return picked_window;
}
void TimeWindowPicker::repick()
{
    // This is a protected method is SeismicPlot so this method is 
    // little more than an alias for that method 
    this->launch_Xevent_thread_handler();
    pick_completed=true;
}
void TimeWindowPicker::build_pick_menu()
{
    MenuItem picker_menu_item[]={
        {(char *)"Pick Time Window",
        &xmPushButtonGadgetClass,'p',
        (char *)"<Key>P",(char *)"P",
        arm_tw_pick_callback,this,NULL,(MenuItem *)NULL},
        {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL}};
    pick_menu=BuildMenu(menu_bar,XmMENU_PULLDOWN,
            (char *)"Picker",'P',false,picker_menu_item);
    XtManageChild(menu_bar);
}
void TimeWindowPicker::setpick(TimeWindow twinpicked)
{
    cerr << "Entering setpick"<<endl;
    picked_window=twinpicked;
    pick_completed=true;
    // freeze this for now
    markers.beam_color=string("red");
    markers.beam_tw=twinpicked;
    int k,kmax;
    if(ThreeComponentMode)
        kmax=3;
    else
        kmax=1;
    for(k=0;k<kmax;++k)
        XtVaSetValues(this->seisw[k],ExmNdisplayMarkers,&markers,NULL);
    cerr << "Leaving setpick"<<endl;
}
} // End SEISPP namespace encapsulation
