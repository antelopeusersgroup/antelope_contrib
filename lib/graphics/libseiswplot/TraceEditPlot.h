#ifndef _TRACEEDITPLOT_H_
#define _TRACEEDITPLOT_H__
#include "SeismicPlot.h"
#define SINGLE_TRACE_EDIT_MODE 1
#define CUTOFF_EDIT_MODE 2
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! Make a plot with trace edit functions enabled.

This object builds a seismic display plot identical to it's parent
called SeismicPlot.  The key difference is that in enables edit functions
in the seisw Widget.   The edit functions are of two form:  (1) single
trace kill and (2) a cutoff method kill.  The first is implemented by 
pointing at a trace and clicking MB2.  The second is implemented with
shift-MB2.  The cutoff editing is similar to that used in dbxcor where
the set of trace below the water mark picked will be killed.  Cutoff makes
sense only if the data are presorted by some metric that defines a level
of badness.  The seisw editor implements kills by setting the live boolean
of the family of time series objects used in my library. Because the 
SeismicPlot object from which this is derives caches a copy of an original
data set the primary method of this object is the report_edit function.
It returns a list of integer index values to the parent ensemble. The caller
must then use this list to implement the kills.  Since what that means is
application dependent it was a design decision to not provide methods
that use this info.  A template is provided below, however, to implement
this in the way I intend with the live variable of time series type objects.

Because this plot handle is a child of SeismicPlot the warnings
described for the SeismicPlot object about copies applies equally 
here.  See documentation for SeismicPlot for a longer explanation.

\author Gary L. Pavlis
*/
class TraceEditPlot : public SeismicPlot
{
public:
    TraceEditPlot();
    TraceEditPlot(Metadata& md);
    set<int> report_kills();
    /*! Toggle between single trace and cutoff mode editing. 
     
     The plot produced by this object always is initialized
     in single trace edit mode.  Calling this method changes
     the edit mode to cutoff where clicking on mb2 kills or 
     restores all data below a picked trace.*/
    void toggle_edit_mode();
private:
    Widget menu_edit;
    /* These two private methods are used to allow common code for
       constructors. */
    void edit_enable();
    void build_edit_menu();
    /* This is used by Seisw widget.  1 for single trace edit mode
       set 2 for cutoff mode */
    int edit_mode;
};
template <class T> void killmembers(T& ensemble,set<int> kills)
{
    if(kills.empty()) return;
    int nm=ensemble.member.size();
    set<int>::iterator kptr;
    for(kptr=kills.begin();kptr!=kills.end();++kptr)
    {
        int ik=(*kptr);
        if(ik>=nm) 
            throw SeisppError(string("killmembers:  ")
                + "string index is larger than ensemble size\n");
        else
            ensemble.member[ik].live=false;
    }
}
} // End SEISPP namespace encapsulation
#endif
