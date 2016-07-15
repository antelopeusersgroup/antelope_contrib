#ifndef _THREEC_ENSEMBLE_TIME_PICKER_H_
#define _THREEC_ENSEMBLE_TIME_PICKER_H_

#include <vector>
#include <map>
#include "GenericTimePicker.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
/* Used only in the private part of this object.   Used by resurrect
 * method to know which seismograms should be brought back to life and
 * which should be left dead.  Note initialized to ALLDEAD for 
 * traces marked dead on entry and MOSTLY_DEAD_ALL_DAY for traces
 * live on entry but not picked.   */
enum PickState {ALLDEAD,MOSTLY_DEAD_ALL_DAY,PICKED};
/* key for relative time picks used within this object. */
const string TCPICKKEY("pickedtime");
/* TCPICKKEY values are initialzied to this value to mark them as not having a
pick */
const double null_pick(-9.999e99);
/* this is the test value - smaller than this defines null */
const double null_pick_test(-1e20);
/* This is the metadata key to set the utc (epoch) time of the 0 time
for a seismogram handled by this object.  Caller needs to make sure
this is so or chaos may result.*/
const string t0shift_key("t0shift");
/*! Used to store times indexed by a station name. */
typedef map<string,double> StaTimes;
/*! Used to store times indexed by the integer index of the member vector 
  of a ThreeComponentSeismogram object being plotted. */
typedef map<int,double> IndexTimes;
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

The implementation uses a motif widget used in dbxcor but operating in a very
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

A VERY IMPORTANT limitation to keep in mind is that we assume that because
picking is blind the user will always call the align method before pulling
any of the data with any of the getters.   get_member_times and 
get_sta_times may not yield the right answer if this is not done.  There
are curently no safety features to protect the innocent from this feature.

\author Gary L. Pavlis, Dept. of Geol. Sci, Indiana University
*/
class ThreeCEnsembleTimePicker
{
public:
  /*! Default constructor - uses default pf stored in antelope contrib data*/
  ThreeCEnsembleTimePicker();
  /*! Construct with parameters defined by a Metadata object */
  ThreeCEnsembleTimePicker(Metadata md);
  /*! Destructor - nontrivial as it has to destroy windows */
  ~ThreeCEnsembleTimePicker();
  /*! \brief plot and pick an ensemble.

  Use this method to initially load data and enable ploting.  Note
  refine_picks method should be used to modify picks.

  \param din - input ensemble.
  \param initialize - effects the behaviour of the time shifting variable
     stored internally in picking. When plotting a new data set for the
     first time set this true.  Default is false which will not alter
     any of the pick variables. 

  \returns - number of seismograms loaded and plotted
  */
  int plot(ThreeComponentEnsemble& din,bool initialize=false);
  /*! Return picked times indexed by station name.

  Times returned are relative to original time reference for each seismogram.
  That means an epoch time for absolute tref or relative to some 0 for 
  something like active source data. */
  StaTimes get_sta_times();
  /*! Return picked times indexed by ensemble member index.

  Times returned are relative to original time reference of each seismogram.
  That means an epoch time for absolute tref or relative to some 0 for 
  something like active source data. */
  IndexTimes get_member_times();
  /*! \brief Align displayed data with pick times.

  When working with multichannel data adjacent seismograms will have similar
  signals.   In that situation similarity can be appraised by aligning
  data to have the picks define the zero mark.   Note that shifts applied
  are stored in the t0shift metatdata attribute and can be accumulated.   i.e
  the same trace can be picked multiple times and the t0shift value will
  accumulate those changes.   For passive source data with an absolute
  time standard t0shift is an epoch time and can be saved as an arrival
  time estimate (e.g. in an css database arrival table).*/
  void align();
  /*! \brief  Align with a set of external lag estimates.
   *
   * This overloaded version of align applies lags in the input 
   * vector and adjust the times to align with those lags.   This
   * is used, for example, in a code I wrote to use cross-correlation
   * to provide a secondary alignment.   
   *
   * Unset lag estimates should be flagged with the null_pick value
   * defined in the header to this file */
  void align(vector<double>& lags);
  /* Start over - restores original data and removes alignments */
  void reset();
  /*! Pick times for active component.

  This gizmo is made to do a series of picks on the active component
  window and cache the internally.  They are retrieved with the set of get
  times methods.   Note the picks are stored in the metadata (headers) of 
  each seismogram loaded for processing.  If align is not called the new
  picks overwrite the previous.   If align is called before this the new picks
  are treated as a correction (update) to the previous.  This allows iterative 
  picking until the results are what is desired. 
  
  \return vector of picks - normally should immediately call the set_pick_times method 
  with the data returned.
  */
  vector<SeismicPick> run_picker();

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
  /*! Run picker and set pick times.

   This would be the normal method called to pick an ensemble.   
   Picks are stored internally.  This method always blocks until 
   the exit command (menu item) is initiated in the gui.   This 
   method can also be viewed (more or less) as running run_picker
   followed be set_pick_times.   

   Note also the data need to retrieved when picking is finished on 
   an ensemble or results will be lose.

   \return Returns number of picks set in this run.
    */
  int pick();
  /*! \brief Mark unpicked data dead

    It is sometimes convenient to mark the seismograms dead that have
    not been picked.  This is, for example, an easy way on the display
    that you want to be sure to drop.   This method should normally be 
    followed by the plot method to verify it did what you wanted.  
    In the seisw widget the unpicked traces will marked dead.

    This can be reversed with the resurrect method. 
    */
  void kill_unpicked();
  /*! \brief Restore data killed by the kill_unpicked method.
   
    This method restores data marked dead with any previous calls to the
    kill_unpicked method.  */
  void resurrect();
  /*! \brief Return data with picks set in headers.
   
    Because this beast works by setting pick relative times in the 
    data ensemble header an alternative to the pick retrieval methods
    is to just return the entire ensemble that contains the pick
    values.  That is what this method does.
    */
  ThreeComponentEnsemble picked_data();
private:
  GenericTimePicker comp0;
  GenericTimePicker comp1;
  GenericTimePicker comp2;
  /* This is a copy of data currently plotted in the original 3C object
   * format.   Metadata and kill states are maintained in this copy.*/
  ThreeComponentEnsemble d0;
  /* This is the working data ensemble but split into components */
  TimeSeriesEnsemble d[3];
  /* This is a raw copy of the data being plotted.  It is cached and
   * used only if the reset method is called. */
  ThreeComponentEnsemble d0_original;
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
  /* This is set from Metadata.  The  t0shift attribute is initialized
   * with the value extracted by this key */
  string t0_align_key_d0;
  /* This vector is used by resurrect to know which data to restore.   
   * This is not just a true false test because there are three possiblities:
   * dead on entry, marked dead by not being picked, and live.  Hence
   * we use this enum. */
  vector<PickState> pick_state;
};
}  // End SEISPP namespace declaration
#endif
