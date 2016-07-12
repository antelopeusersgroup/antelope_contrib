#include "GenericTimePicker.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/* This is the callback procedure that handles a pick made with Btn2 */
void GTPBtn2Callback(Widget w, void *client_data, void *userdata)
{
  GenericTimePicker *gtp=reinterpret_cast<GenericTimePicker *>(client_data);
  SeismicPick *thispick;
  /* The seisw widget has a dumb dependence on this being set to NULL initially so we have go force it
   * here */
  XtVaGetValues(w,ExmNseiswPick,&thispick,NULL);
  //DEBUG
  /*
  cerr << "In GTPBtn2Callback:  pick extracted="
          << thispick->type<<" "
          << thispick->time<<" "
          << thispick->amplitude<<" "
          << thispick->trace_number<<endl;
          */
  gtp->post(*thispick);
}
GenericTimePicker::GenericTimePicker() : SeismicPlot()
{
  const string base_error("GenericTimePicker default constructor:  ");
  if(ThreeComponentMode) throw SeisppError(base_error
        + "ThreeComponentMode is set true.\n"
        + "This picker works only on scalar data.  Edit pf file.");
  allpicks.reserve(1);
  picker_active=false;
  //XtRemoveAllCallbacks(this->seisw[0],ExmNbtn2Callback);
  //XtAddCallback(this->seisw[0],ExmNbtn2Callback,GTPBtn2Callback,this);

}
GenericTimePicker::GenericTimePicker(Metadata& md) : SeismicPlot(md)
{
  const string base_error("GenericTimePicker(Metadata) constructor:  ");
  if(ThreeComponentMode) throw SeisppError(base_error
        + "ThreeComponentMode is set true.\n"
        + "This picker works only on scalar data.  Edit pf file.");
  allpicks.reserve(1);
  //XtRemoveAllCallbacks(this->seisw[0],ExmNbtn2Callback);
  //XtAddCallback(this->seisw[0],ExmNbtn2Callback,GTPBtn2Callback,this);
  picker_active=false;
}
void GenericTimePicker::post(SeismicPick pick_to_add)
{
    allpicks.push_back(pick_to_add);
}
SeismicPick GenericTimePicker::pick_one()
{
  try{
    this->pick_all();
    // Return the last click
    return allpicks[allpicks.size()-1];
  }catch(...){throw;};
}
vector<SeismicPick> GenericTimePicker::pick_all()
{
     try{
    picker_active=true;
    //DEBUG
    //cerr << "Entered GenericTimePicker::pick_all"<<endl;
    allpicks.clear();
    /* As a sanity check we should impose a limit on times through this
    loop to prevent a runaway if btn2 is stuck for some reason.
    */
    int count;
    XtRemoveAllCallbacks(this->seisw[0],ExmNbtn2Callback);
    XtAddCallback(this->seisw[0],ExmNbtn2Callback,GTPBtn2Callback,this);
    /* This will loop until we enter x on the display or hit the exit menu
    item in the SeismicPlot gui.   Simple solution requiring no new widgets */
    cerr << "GerericTimePicker is active"<<endl;
    while(this->EventLoopIsActive)
    {
        count=allpicks.size();
        cerr << "Number of picks made so far="<<count<<endl;
        sleep(1);
    }
    picker_active=false;
    return allpicks;
  }catch(...){throw;};
}
void GenericTimePicker::stop_picker()
{
    this->ExitDisplay();
    this->disable_blocking();
    picker_active=false;
}
} // End SEISPP namespace encapsulation
