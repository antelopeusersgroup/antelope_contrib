#ifndef _SEISMICPLOT_H_
#define _SEISMICPLOT_H_
#include <pthread.h>
#include "ensemble.h"
#include "Seisw.h"
#include "BasicSeisPlot.h"
#include "SeismicPick.h"
#include "Metadata.h"
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! This defines a standardized interface to a plot for the SEISPP library.

   This class is an attempt to abstract the basic plot functions required
   to plot and work with seismic data.  It inherits an abstract base class
   called BasicSeismicPlot which is a pure virtual base.  It also inherits
   Metadata as a convenient way to change plot parameters.  An anomaly that
   makes this a nonideal interface is that in the current implementation
   this requires the refresh method to force the new parameters to take
   effect.  It would be better if any parameter changes caused an immediate
   redraw, but the current version based on Xt and the Seisw Motif widget
   just can't do that.  

   Because the implementation of this class is focused on the Seisw Motif
   widget there is a very important limitation to note.  One must never
   attempt to copy this object. The reason is that the X implemenation 
   used here is simply not readily capable of handling multiple copies of
   the same object in the same program.  There are several good reasons for
   this but user should just realize the key rule - just don't ever try
   to create copies of this plot handle.  A C++ compiler might just
   allow you to do so because a default copy constructor and operator=
   are often created even if you don't declare them.  

   \author Gary L. Pavlis
   */
class SeismicPlot : public BasicSeisPlot, public Metadata
{
    public:
        /*! Construct a plot handle using default parameters. 

          Defaults for this object are stored in an Antelope a parameter file calld
          SeismicPlot.pf stored in $ANTELOPE/data/pf.  This constructor uses pure
          defaults for parameters. */
        SeismicPlot();
        /* Constructor using Metadata object to define plot parameters.

           The plot geometry is defined by a large group of parameters passed to the
           plotting widget through a Metadata object.  
           \param md is the Metadata object that is assumed to contain the (fairly large) 
                number of parameters needed to define the plot geometry. 
            \exception Will throw a SeisppError object if plot parameters are missing.
        */
        SeismicPlot(Metadata& md);
        /*! Destructor. 

          Destructor for this beast is not trivial as we need to have the window system
          clear the plot window and close it cleanly. */
        ~SeismicPlot();
        /*! Plot an ensemble of data.

          This method plots an ensemble of scalar data in a single window frame. 

          \param d is the ensemble to be plotted. 
          \param blocking boolean to control whether or not the plot enters
            an event loop (default true).  When true the caller will block
            until the user exits through the user interface.  When false 
            the plot is generated and the program continues immediately. 
          */
        void plot(TimeSeriesEnsemble& d,bool blocking=true);
        /*! Plot a three component ensemble of data.

          Three-component data could be plotted a variety of ways, but in this implmentation
          this is plotted in 3 panes of the base window.

          \param d is data to be plotted.
          \param blocking boolean to control whether or not the plot enters
            an event loop (default true).  When true the caller will block
            until the user exits through the user interface.  When false 
            the plot is generated and the program continues immediately. 
          */
        void plot(ThreeComponentEnsemble& d, bool blocking=true);
        /*! Plot a single time series.

          This is essentially a special case of a TimeSeriesEnsemble with one member and it is
          treated that way.  i.e. you get a plot of the data with one trace in the window.
          */
        void plot(TimeSeries& d,bool blocking=true);
        /*! Plot a single three-component seismogram.

          A three component seismogram is plotted in the window 0 (the one labelled component 0) 
          on a common time base.  The alternative would have been to display each component in a 
          different window, which would not make sense for common use for this type of plot.  

          \param d is the seismogram to be plotted.
          \param blocking boolean to control whether or not the plot enters
            an event loop (default true).  When true the caller will block
            until the user exits through the user interface.  When false 
            the plot is generated and the program continues immediately. 
          */
        void plot(ThreeComponentSeismogram& d,bool blocking=true);
        /*! Set blocking on or off.

          Constructors can enable the plot as blocking or nonblocking. 
          If blocking the plot will stay active until the exit menu
          (x key) is clicked.  If set nonblocking a plot will return 
          immediately but still be in a potentially useful state.  
          The assumption is there is no point in turning blocking off,
          but it can be useful to turn it on to retry something in a
          gui.  Mostly useful for children of this like TimeWindowPicker.
          */
        void enable_blocking()
        {
            block_till_exit_pushed=true;
        };
        /*! Make new plot parameters active.

          This plot object inherits a Metadata object.  You can thus use the 
          get and put methods in Metadata to change parameters internally to this object's
          internal data definitions.  The design of the Widget used to implement this 
          class requires an explicit push of the entire Metadata object through an 
          Xwindows call.  Rather than override the get and put methods I judged it 
          preferable to use this model.  That is, if you want to change an plot
          parameters use the Metadata put methods to change what you want.  When you
          have put all the new parameters you want call this (refresh) method to 
          have them new parameters pushed to the Widget.  
          */
        void refresh();
        /*! Used internally to break the event loop.  

          This really shouldn't be int he public interface, but I couldn't figure out
          a way to get rid of it. */
        void ExitDisplay(){
            EventLoopIsActive=false; 
        };
        /*! Used internally by event loop thread to test for termination.

          his really shouldn't be int he public interface, but I couldn't figure out
                    a way to get rid of it. */
        bool WindowIsActive(){
            return EventLoopIsActive;
        };
        /*! X application context.

          This plotting is based on X and I could not figure out a way to not put
          this in the public interface and get threading to work right.  It really
          doesn't belong here and a caller should never ever touch this. */
        XtAppContext AppContext;
        friend class TraceEditPlot;
        friend class TimeWindowPicker;
        friend class GenericTimePicker;
    protected:
        /* When true calls to plot will block until until the exit button is
           pushed.   When false a call to plot immediately returns. */
        bool block_till_exit_pushed;
        bool EventLoopIsActive;  // Set true when an event loop thread is running
        /* These are used to be safe.  They may not be necessary, but 
           because normally these are initialized from main this fakery
           may be needed */
        int argc;  
        char *argv[1];
        Widget toplevel,main_w;
        Widget slrc,paned_win;
        Widget continue_button;
        Widget seisw[3];   // This is the set of seismic display widgets
        //Widget seisw0,seisw1,seisw2;
        //For now just one entry, add pick functions to menu bar later
        Widget menu_bar,menu_file;
        /* Private method that launches event handler thread */
        void launch_Xevent_thread_handler();
        /* We will need this to implement kills on a 3c ensemble.  Intentionally 
           commented out initially as this is a nasty add on that may never happen. */
        //ThreeComponentEnsemble *parent3c;
        /* These are temporaries needed to cache data being plotted.
           Without this we will get a seg fault when the ensemble goes out of scope
           because the widget accepts a raw pointer */
        TimeSeriesEnsemble *comp0,*comp1,*comp2;
        /*! Private method initializes widgets in a regular way.
          
          This is mainly here to provide common code for unparameterized
          constructor and the one using a Metadata.*/
        void initialize_widgets(Metadata& md);
};
}
#endif
