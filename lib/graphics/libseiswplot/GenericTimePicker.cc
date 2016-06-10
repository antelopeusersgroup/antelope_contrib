#include "GenericTimePicker.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
GenericTimePicker::GenericTimePicker() : SeismicPlot()
{
  const string base_error("GenericTimePicker default constructor:  ");
  if(ThreeComponentMode) throw SeisppError(base_error
        + "ThreeComponentMode is set true.\n"
        + "This picker works only on scalar data.  Edit pf file.");
  allpicks.reserve(1);

}
GenericTimePicker::GenericTimePicker(Metadata& md) : SeismicPlot(md)
{
  const string base_error("GenericTimePicker(Metadata) constructor:  ");
  if(ThreeComponentMode) throw SeisppError(base_error
        + "ThreeComponentMode is set true.\n"
        + "This picker works only on scalar data.  Edit pf file.");
  allpicks.reserve(1);
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
    allpicks.clear();
    /* As a sanity check we should impose a limit on times through this
    loop to prevent a runaway if btn2 is stuck for some reason.
    */
    int count(0);
    const int MaxPicks(1000);
    /* This will loop until we enter x on the display or hit the exit menu
    item in the SeismicPlot gui.   Simple solution requiring no new widgets */
    while(this->EventLoopIsActive)
    {
      SeismicPick thispick;
      XtVaGetValues(this->seisw[0],ExmNseiswPick,&thispick,NULL);
      allpicks.push_back(thispick);
      ++count;
      if(count>MaxPicks)throw SeisppError(string("GenericTimePicker::pick_all:  ")
          + "Pick loop hit limit of 1000 picks - exiting to stop a runwawy");
    }
  }catch(...){throw;};
}
} // End SEISPP namespace encapsulation
