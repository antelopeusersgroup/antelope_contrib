#ifndef _THREECPLOT_H_
#define _THREECPLOT_H_
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

  Unlike SeismicPlot this procedure never blocks.  The intent is you only want
  to see data and not interact with it, which is a simplification from the o
  original design. */
  void plot(ThreeComponentEnsemble& d);
  void plot(ThreeComponentSeismogram& d1);
private:
  TraceEditPlot comp0,comp1,comp2;
};
} // End namespace SEISPP encapsulation
#endif
