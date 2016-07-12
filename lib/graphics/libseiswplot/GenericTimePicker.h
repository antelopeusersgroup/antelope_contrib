#ifndef GENERIC_TIME_PICKER_H_
#define GENERIC_TIME_PICKER_H_
#include "SeismicPick.h"
#include "SeismicPlot.h"
#include "display_marker.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! \brief Base class for picking times and amplitudes.

Picking times and amplitudes is a basic data processing function in
seismology.   This object abstracts that concept allowing the time and
amplitude of a seismogram at a point on the display to be returned.
The implementation uses the seisw motif widget, but the API
should be considered as fairly generic.

Also note this object is intended strictly as an intermediary to
use as an internal component of a higher level interface.   The SeismicPick
object is generic, but would be confusing to any end user.  In particular,
this is an intermediary for a multichannel 3C picker.  
*/
class GenericTimePicker : public SeismicPlot
{
public:
  GenericTimePicker();
  GenericTimePicker(Metadata& md);
  /*! Return a point poick for one seismogram.

  The seisw widget defines a generic pick object called SeismicPick that
  can hold multiple things.   For this application the time attribute of
  the object is most likely of interest, but we commonly will also want the
  trace_number and may wantt he amplitude.  This method returns one of
  these for the first point picked with MB2.*/
  SeismicPick pick_one();
  /*! Pick several seismograms at onces.

  When working with an ensemble it is commonly useful to pick several seismograms
  in a single pass.   This returns these as an array of SeismicPick objects.
  Caller will need to work through that array to extract the information in
  the picks */
  vector<SeismicPick> pick_all();
  /*! Post a new pick to list.
   
   This method is used by callbacks to add a new pick to the list.  
   This essentially just does a push_back to the internal array of 
   picks */
  void post(SeismicPick pick);
  void stop_picker();
  bool picker_is_active(){return picker_active;};
private:
  vector<SeismicPick> allpicks;
  bool picker_active;
};
} // End SEISPP namespace encapsulation
#endif
