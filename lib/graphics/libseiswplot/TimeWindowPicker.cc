#include "TimeWindowPicker.h"
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
    twinpicked=spick->get_window();
    // Silently drop a pick if not properly tagged.  
    if(spick->type == WINDOW) 
        picker_handle->setpick(twinpicked);

    cerr << "Leaving pick_tw_callback"<<endl;
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
void TimeWindowPicker::enable_picking()
{
    cerr << "Entering enable_picking"  <<endl;
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
    cerr << "Exiting enable_picking"  <<endl;
}
TimeWindowPicker::TimeWindowPicker() : SeismicPlot(), picked_window()
{
    this->enable_picking();
    pick_completed=false;
}
TimeWindowPicker::TimeWindowPicker(Metadata& md) : SeismicPlot(md) ,
    picked_window()
{
    cerr << "Entering TimeWindowPicker(Metadata) constructor"<<endl;
    this->enable_picking();
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
}
} // End SEISPP namespace encapsulation
