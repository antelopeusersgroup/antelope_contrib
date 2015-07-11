#ifndef _TIME_WINDOW_PICKER_H_
#define _TIME_WINDOW_PICKER_H__
#include "TimeWindow.h"
#include "SeismicPlot.h"
#include "display_marker.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! Plots seismic data to for picking a time window.

This object builds a seismic display plot identical to it's parent
called SeismicPlot.  The key difference is that it enables the 
the time window picking function feature of the seisw widget.
Normal usage is expected to be to create the window with one of
the contructors, load data with the plot methods of SeisicPlot, 
pick a time window with MB3, exit the graphic loop (x key stroke
in SeismicPlot window), and then retrieve the window with the 
get method.  The get method has a simple sanity check to make 
sure a pick has been made.  See get method description below.
Note repeated picks before exiting the plot event loop will 
overwrite the previous.  Thus be aware the last pick made is 
the one that will be returned by the get method.

Because this plot handle is a child of SeismicPlot the warnings
described for the SeismicPlot object about copies applies equally 
here.  See documentation for SeismicPlot for a longer explanation.

\author Gary L. Pavlis
*/
class TimeWindowPicker : public SeismicPlot
{
public:
    TimeWindowPicker();
    TimeWindowPicker(Metadata& md);
    /*! Fetch the picked time window.

      This method is used to get the result of picking a time window.
      This is a less than trivial thing because we have to handle the
      highly likely scenario that the user failed to make a valid time
      pick.  The get method is effectively a one shot.  You can retrieve
      a time pick only once without an error.  If a repick is required call
      the rearm method.  

      Similarly if this method is called and no time window has been picked 
      this method will throw an exception.  Most programs should handle this
      exception cleanly without an exit and immediately call the repick 
      method.  

      \exception SeisppError is thrown if this method is called with a 
      valid pick having been made from the data. 

      */
    TimeWindow get();
    /*! Rearm the picking function to try again.

    The plot window is armed on construction or after loading a new set
    of data, but has to be disarmed to call the get method.  This method
    is normally called to "try again". 
    */
    void repick();
    void setpick(TimeWindow tw);
private:
    void enable_picking();
    TimeWindow picked_window;
    bool pick_completed;
    DisplayMarkerDataRec markers;
};
} // End SEISPP namespace encapsulation
#endif
