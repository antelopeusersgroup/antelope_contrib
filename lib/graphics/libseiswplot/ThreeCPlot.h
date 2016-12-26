#ifndef _THREECPLOT_H_
#define _THREECPLOT_H_
#include <set>
#include "Metadata.h"
#include "ensemble.h"
#include "TraceEditPlot.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
/*! \brief Object to plot three component data in three windows.

The SeismicPlot object was made to support 3c data, but I built it as
3 paned windows. That turns out to be a terrible way to display anything
but small ensembles.   I then built this variant that draws 3c data in
three windows.
*/

class  ThreeCPlot
{
public:
  ThreeCPlot();
  /*! \brief construct using parameters from Metadata.

  This constructor passes md directly to the SeismicPlot constructors
  for the 3 components so md should contain all the parameters required by
  the SeismicPlot object.  Each window will get a generic title posted to
  it to allow one to tell which window is which. */
  ThreeCPlot(Metadata& md);
  /*! \brief plot a 3c ensemble.
   * */
  void plot(ThreeComponentEnsemble& d,bool block=true);
  void plot(ThreeComponentSeismogram& d1,bool block=true);
  /*! \brief Return which signals have been marked dead.
   *
    This object enables the window in edit mode Which allows
  the user to mark signals to be killed.   This method returns an
  stl set of ensemble member positions of all signals marked bad 
  in any of the 3 windows this object draws.  It uses a method
  with the same name in TraceEditPlot for each component and then
  returns the union of the 3 sets */
  set<int> report_kills();
private:
  TraceEditPlot comp0,comp1,comp2;
};
} // End namespace SEISPP encapsulation
#endif
