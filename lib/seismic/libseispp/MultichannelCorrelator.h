#ifndef _MULTICHANNELCORRELATOR_H_
#define _MULTICHANNELCORRELATOR_H_

#include <vector>
#include <math.h>

#include "perf.h"
#include "stock.h"
#include "coords.h"
#include "pf.h"
#include "tt.h"
#include "db.h"
#include "f2c.h"
#include "TimeSeries.h"
#include "ensemble.h"
#include "stack.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP 
{

enum CorrelationMethod {Basic, SimpleStack, RobustStack};
enum PenaltyFunction {none, huber, bisquare, thomson};

TimeSeries correlation(TimeSeries& x, TimeSeries& y,bool normalize=false);

//@{
// Generalized array cross correlation object.
// This object implements multiple methods of multichannel cross-correlation.  
// It uses a model foreign to most of us old procedural guys in that the creation
// of the object involves the actual calculations behind the concept.  That is,
// it isn't arguments in, data out.  It is arguments create the object that is
// the result of the calculations;  in this case multichannel cross correlation.
// Although foreign it makes for a clean design for this concept as it cleanly
// encapsulates the output of the calculations.
//
///@author Gary L. Pavlis
//@}

class TimeSeriesMaximum
{
public:
        double lag;
        double peak;
        TimeSeriesMaximum(){lag=0.0;peak=0.0;};
        TimeSeriesMaximum(TimeSeries& d);
        TimeSeriesMaximum& operator=(const TimeSeriesMaximum& other);
};

class MultichannelCorrelator
{
public:
	vector<double>lag;
	vector<double>peakxcor;
	vector<double>weight;
	vector<double>amplitude_static;
	CorrelationMethod method_used;
	PenaltyFunction pfunction_used;
	TimeSeriesEnsemble xcor;
	
	MultichannelCorrelator();  
	MultichannelCorrelator(TimeSeriesEnsemble d,
		CorrelationMethod meth,
			TimeWindow beam_window,
				TimeWindow robust_window=TimeWindow(),
					StackType stacktye=BasicStack,
						TimeSeries *intial_beam=NULL,
						     int reference_member=0,
							bool normalize=false,
								bool parallel=false);
	MultichannelCorrelator(ThreeComponentEnsemble d,
		CorrelationMethod meth,
			TimeWindow beam_window,
				int component,
					TimeWindow robust_window=TimeWindow(),
						StackType stacktye=BasicStack,
							TimeSeries *intial_beam=NULL,
							    int reference_member=0,
								bool normalize=false,
									bool parallel=false);
	MultichannelCorrelator(const MultichannelCorrelator& co);
	MultichannelCorrelator& operator=(const MultichannelCorrelator& co);

	// Will definitely need these
	TimeSeries ArrayBeam() {return beam;}  // needs to test against method a beam not always computed
	TimeSeries ArrayBeam(Metadata m);  // alternate appends m to beam metadata
private:
	TimeSeries beam;
};
}  // End SEISPP namespace declaration

#endif
