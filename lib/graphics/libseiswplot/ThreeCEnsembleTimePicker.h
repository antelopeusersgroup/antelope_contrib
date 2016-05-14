#ifndef _THREEC_ENSEMBLE_TIME_PICKER_H_
#define _THREEC_ENSEMBLE_TIME_PICKER_H_

#include <vector>
#include <map>
#include "GenericTimePicker.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
typedef map<string,double> StaTimes;
typedef map<int,double> IndexTimes;
/* This is the metadata key to set the utc (epoch) time of the 0 time
for a seismogram handled by this object.  Caller needs to make sure
this is so or chaos may result.*/
const string t0shift_key("t0shift");
/* \brief Arrival time picker for three component data.

This object is aimed as a simple interface for a picking window
for measuring arrival times.   It is intended to do only that so it is
not a full picking gizmo like dbpick.

A design goal of this gizmo is to provide a mechanism to pick three-component
data cleanly to allow the user to quickly appraise the validity of a set of
picks in the context of all three components.  dbpick and sac, for example, are both
horrible for this purpose.  There are multiple ways one might do this, but
this gizmo was designed with multichannel data in mind where an implicit
assumption is that adjacent seismograms in ensemble order have enough similarity
that alignment by a common signal feature like the first break or a zero
crossing makes sense.

The implementation uses motif widget used in dbxcor but operating in a very
different mode.   The user is presented with 3 windows, one for each of the
three components.   MB2 is used to pick times.   Because of the internal
limits of the seisw widget this has to be done like picking in seismic unix
and is more or less blind.  i.e. all that happens when a pick is made is a
value is stored internally.   If multiple picks are made on the same
station (in any window) the last pick made overrrides a previous pick.
The idea behind this approach is that the user would normally start with
one data component and pick each seismogram presented.   On exit the
caller should arrange to call the align method which will shift all
component windows to have 0 as the time just picked.   Normal procedure
would be to rearm the picker, tweek the picks, and again call align.
Repeat until happy.   The object intentionally does NOT contain any gui
elements to control this because an application can either add that
element to the gui or do this through the command line interface.

\author Gary L. Pavlis, Dept. of Geol. Sci, Indiana University
*/
class ThreeCEnsembleTimePicker
{
public:
  /*! Default constructor - uses default pf stored in antelope contrib data*/
  ThreeCEnsembleTimePicker();
  /*! Construct with parameters defined by a Metadata object */
  ThreeCEnsembleTimePicker(Metadata& md);
  /*! Destructor - nontrivial as it has to destroy windows */
  ~ThreeCEnsembleTimePicker();
  /*! \brief plot and pick an ensemble.

  Use this method to initially load data and enable ploting.  Note
  refine_picks method should be used to modify picks.

  \param din - input ensemble.

  \returns - number of seismograms loaded and plotted
  */
  int plot(ThreeComponentEnsemble& din);
  /*! Return picked times indexed by station name.

  Times returned are relative to 0 reference of each seismogram. */
  StaTimes get_sta_times();
  /*! Return picked times indexed by ensemble member index.

  Times returned are relative to 0 reference of each seismogram.*/
  IndexTimes get_member_times();
  /*! Return absolute (UTC) time indexed by station name.

  This method returns epoch times.   It assuumes the Metadata field
  times has been set to convert pick time to utc.
  */
  StaTimes get_absolute_sta_times();
  /*! Return absolute (UTC) times indexed by ensemble member number.

  This method returns epoch times.   It assuumes the Metadata field
  times has been set to convert pick time to utc.
  */
  IndexTimes get_absolute_member_times();
  /* \brief Align displayed data with pick times.

  When working with multichannel data adjacent seismograms will have similar
  signals.   In that situation similarity can be appraised by aligning
  data to have the picks define the zero mark.   Note that shifts applied
  are stored in the t0shift metatdata attribute and can be accumulated.   i.e
  the same trace can be picked multiple times and the t0shift value will
  accumulate those changes.   For passive source data with an absolute
  time standard t0shift is an epoch time and can be saved as an arrival
  time estimate (e.g. in an css database arrival table).*/
  void align();
  /* Start over - restores original data and removes alignments */
  void reset();

  /*! Rearm the picker for refining picks.  */
  void refine_picks();
  /*! Sets which component should be the active pick window.

  \param ic - component to make active (0, 1, or 2).

  Note ic is not tested for validity as this is assumed only used in the
  gui where there is no way to get it wrong.
  */
  void set_active_component(int ic);
  int get_active_component()
  {
    return active_component;
  }
  /* Set the times returned from a series of picks.

  \param picks - outut of GenericTimePicker of pick objects

  \return - number of seismograms actually set (may not be equal to
          size of picks vector)
          */
  int set_pick_times(vector<SeismicPick> picks);
  /*! Pick times for active component.

  This gizmo is made to do a series of picks on the active component
  window and cache the internally.  They are retrieved with the set of get
  times methods*/
  vector<SeismicPick> pick_times();
private:
  GenericTimePicker components[3];
  /* This is a copy of data currently plotted.   Necessary to guarantee
  order and also allow station indexing */
  ThreeComponentEnsemble d0;
  /* This is the working data ensemble but split into components */
  TimeSeriesEnsemble d[3];
  /* This is used as a ready sanity check */
  bool data_loaded;
  /* Cached for convenience = number of seismograms in working ensemble */
  int nd;
  /* We put a pick menu on each of the components windows.   WE can
  ensable picking from any of them of any component's data. */
  Widget pick_menu[3];
  /* This define which component window is to be used for picking.
  It is set by the pick menu callback as 0 1 or 2. */
  int active_component;
  /* This is used by constructors to build the above 4 widgets. */
  void build_pick_menu();

};
/*! \brief Returns an edited ensemble with pick times set.

This procedure is used to store pick times produced by a ThreeCEnsembleTimePicker
in the Metadata area of each seimogram of the parent.   This version assumes
station name indexing can be used so it uses the StaTime output.
This procedure intentionally edits the ensemble rather than returning a copy
because metadata edits take trivial effort and copying an ensemble can be
a huge overhead.

\param picks - output of pick_times method of ThreeCEnsembleTimePicker.
\param d - parent data to store picks
\param key - metadata key used to store the pick time.
\param kill_unpicked - when true unpicked seismograms will be marked dead (live
attribute will be set false).   Default is false.

\return - number of picks set

*/
int set_arrival_times(StaTimes& picks, ThreeComponentEnsemble& d, string key,
                        bool kill_unpicked=false);


/*! \brief Returns an edited ensemble with pick times set.

This procedure is used to store pick times produced by a ThreeCEnsembleTimePicker
in the Metadata area of each seimogram of the parent.   This version utilizes
simple vector indexing returned through IndexTimes.
This procedure intentionally edits the ensemble rather than returning a copy
because metadata edits take trivial effort and copying an ensemble can be
a huge overhead.

\param picks - output of pick_times method of ThreeCEnsembleTimePicker.
\param d - parent data to store picks
\param key - metadata key used to store the pick time.
\param kill_unpicked - when true unpicked seismograms will be marked dead (live
attribute will be set false).   Default is false.

\return - number of picks set

*/
int set_arrival_times(IndexTimes& picks, ThreeComponentEnsemble& d, string key,
                        bool kill_unpicked=false);
}  // End SEISPP namespace declaration
#endif
