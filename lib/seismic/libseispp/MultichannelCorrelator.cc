// Implements class MultichannelCorrelator.  
// CCDOC COMMENTS NEED TO ULTIMATELY BE PASTED INTO INCLUDE FILE
//
//

/*  This is a simple object intentionally limited to the scope of this file.
It is used to find the peak correlation value.  Simple idea that could be
made generic eventually, but not worth the trouble at this point. 
It is used by the MultichannelCorrelator object which is why it is placed 
in this file and at the top.
*/

#include "MultichannelCorrelator.h"
#include "seispp.h"

//class TimeSeriesMaximum
//{
//public:
//	double lag;
//	double peak;
//	TimeSeriesMaximum(){lag=0.0;peak=0.0;};
//	TimeSeriesMaximum(TimeSeries& d);
//	TimeSeriesMaximum& operator=(const TimeSeriesMaximum& other);
//};
// This algorithm might be replaceable with stl find algorithm.  Check
TimeSeriesMaximum::TimeSeriesMaximum(TimeSeries& d)
{
	int j,jlag;
	peak=d.s[0];
	for(j=1;j<d.s.size();++j)
	{
		if(peak<d.s[j])
		{
			jlag=j;
			peak=d.s[j];
		}
		lag=d.time(jlag)-d.t0;
	}
}
// this is essential what the compiler would generate automatically, but
// better safe than sorry.
TimeSeriesMaximum& TimeSeriesMaximum::operator=(const TimeSeriesMaximum& other)
{
	if(this!=&other)
	{
		peak=other.peak;
		lag=other.lag;
	}
	return(*this);
}
// The beam needs to have this normalization constant set.  All statics are computed
// relative to this amplitude using the dot product of the beam with data
// This files scope global sets the name used for this
//
string mdscalename("beam_scale_factor");
double ComputeAmplitudeStatic(TimeSeries& beam, TimeSeries& data, int lag)
{
	double beam_scale=beam.get_double(mdscalename);
	// Because used internally won't best bounds, but don't use this in
	// another program without making it safe in that sense.
	double datamp=ddot(beam.s.size(),&(beam.s[0]),1,&(data.s[lag]),1);
	return(datamp/beam_scale);
}
double linfnorm(vector<double> x)
{
        int i;
	double xmax;

	for(i=1,xmax=fabs(x[0]);i<x.size();++i)
		xmax=max(fabs(x[i]),xmax);
	return(xmax);
}
	
//@{
// Default constructor.  Probably could leave defaulted but better safe than sorry.
//
//@}
MultichannelCorrelator:: MultichannelCorrelator()
{
	lag.resize(0);
	peakxcor.resize(0);
	weight.resize(0);
	amplitude_static.resize(0);
	pfunction_used=none;
	xcor.member.resize(0);
}
//@{
// Constructor that implements MultichannelCorrelation with different choices
// of algorithm.
// This constructor takes the contents of in input ensemble and performs
// a multichannel correlation with the results depending on the choice of 
// algorithm and, in anything but the "Basic" method, the choice of the
// initial estimate of the array beam.  
//
// The "Basic" method does a simple cross correlation of each trace with a
// reference trace passed either through the initial_beam parameter or the 
// reference_member parameter.  Other methods involve an iterative procedure
// using a beam recomputed iteratively until convergence.  The basic algorithm
// is correlate with beam, align stack using current lags, sum, repeat until
// time shifts do not change.  
//
// The robust methods differ from the simpler methods in the use of a penalty 
// function in forming the stack.  Currently two robust stacks are supported:
// (a) a median stack and (b) a SNR weighting method.  The later is of greatest
// value for highly variable signal to noise conditions.  The most significant 
// example is source correlations where different magnitude events can enter in
// the same stack.  Note the SNR scheme is slightly more complicated than a signal
// to noise estimate.  It is more coherence-like using a ratio of L2 norm of the
// original data to the L2 norm of the residuals (amplitude adjusted stack) to 
// define the SNR.  
//
//@}

MultichannelCorrelator:: MultichannelCorrelator(TimeSeriesEnsemble data,
	CorrelationMethod method, 
		TimeWindow beam_window,
			TimeWindow robust_window,
				StackType stacktype,
					TimeSeries *initial_beam,
						int reference_member,
							bool normalize,
								bool parallel)
{
	int i, count;
	double tshiftnorm;
	const double TSCONVERGE=beam.dt;
	const int MAXIT=30;
	const string base_message("MultichannelCorrelator constructor:  ");

	if(data.member.empty())
		throw SeisppError(base_message+string("Input data ensemble is empty\n"));
	// All methods use reference_member when that argument is used.  Otherwise
	// the contents of the Beam trace are used.
	try{
		if((initial_beam==NULL) && (reference_member<0) )
			throw SeisppError(base_message+string("Illegal input.\n")
				+ string("Require either a TimeSeries initial trace or index to ensemble member.\n"));
	
		if(initial_beam!=NULL)
		{
			beam=WindowData(*initial_beam,beam_window);
		}
		else
		{
			//used to be data.s
			beam=WindowData(data.member[reference_member],beam_window);
			// The beam needs to be normalized for efficiency
			double rms=dnrm2(beam.s.size(),&(beam.s[0]),1);
			dscal(beam.s.size(),rms,&(beam.s[0]),1);
			beam.put(mdscalename,rms);
		}
		//
		// All methods do an initial correlation against the beam.
		// Even the basic method does this because the beam is a selected
		// station or some input reference trace passed as initial_beam.  
		//
		// correlation function should have a normalized boolean to allows
		// normalization to be turned on or off.
		//`
		for(i=0;i<data.member.size();++i)
		{
			xcor.member.push_back(correlation(beam,data.member[i]));
			TimeSeriesMaximum tsm(xcor.member[i]);
			lag.push_back(tsm.lag);
			peakxcor.push_back(tsm.peak);
			weight.push_back(1.0);
			amplitude_static.push_back(ComputeAmplitudeStatic(beam,data.member[i],tsm.lag));
			// This is needed for the stacker to work correctly below
			data.member[i].put(moveout_keyword,tsm.lag);
		}
			
                //Peng Wang
		//method_used=CorrelationMethod;
		method_used=method;
		//switch(CorrelationMethod)
		switch(method)
		{
		case Basic:
			// Do almost nothing more with the simple method
			pfunction_used=none;
			break;
		case SimpleStack:
		case RobustStack:
			//Peng Wang
			count=0;
			do {
				Stack newstack(data,beam_window,robust_window,stacktype);	
				beam=newstack.stack;
				for(i=0;i<data.member.size();++i)
				{
					xcor.member[i]=correlation(beam,data.member[i]);
					TimeSeriesMaximum tsm(xcor.member[i]);
					lag[i]=tsm.lag;
					peakxcor[i]=tsm.peak;
					weight[i]=newstack.weights[i];
					amplitude_static[i] = ComputeAmplitudeStatic(beam,
									data.member[i],tsm.lag);
					data.member[i].put(moveout_keyword,tsm.lag);
				} 
				tshiftnorm=linfnorm(lag);
				//Peng Wang, added count++
				count++;
			} while( tshiftnorm > TSCONVERGE && count < MAXIT );
			break;
		default:
			string message("Unknow method requested\n");
			throw SeisppError(base_message+message);
		}
	} catch (...) {
	    //supposed to throw something
	};
}
//MultichannelCorrelator:: MultichannelCorrelator(ThreeComponentEnsemble data,
//	CorrelationMethod method, int component, bool parallel=false);
MultichannelCorrelator:: MultichannelCorrelator(ThreeComponentEnsemble data,
	CorrelationMethod method, 
		TimeWindow beam_window,
			int component,
				TimeWindow robust_window,
					StackType stacktype,
						TimeSeries *initial_beam,
							int reference_member,
								bool normalize,
									bool parallel)
{
//Peng Wang, commented out for compilation and testing, this should not be
//directly data or ThreeComponentEnsemble, instead, should converted to ThreeComponentSeismogram
//	try{
//		auto_ptr<TimeSeriesEnsemble>comp(ExtractComponent(data,component));
//		*this = MultichannelCorrelator(*comp,method,beam_window,robust_window,
//				stacktype,initial_beam,reference_member,normalize,parallel);
//	}catch(...) {
	    //supposed to throw something
//	}
}
MultichannelCorrelator:: MultichannelCorrelator(const MultichannelCorrelator& co)
{
	lag=co.lag;
	peakxcor=co.peakxcor;
	weight=co.weight;
	amplitude_static=co.amplitude_static;
	method_used=co.method_used;
	pfunction_used=co.pfunction_used;
	xcor=co.xcor;
	beam=co.beam;
}
MultichannelCorrelator& MultichannelCorrelator::operator=(const MultichannelCorrelator& co)
{
	if(this!=&co)
	{
		lag=co.lag;
		peakxcor=co.peakxcor;
		weight=co.weight;
		amplitude_static=co.amplitude_static;
		method_used=co.method_used;
		pfunction_used=co.pfunction_used;
		xcor=co.xcor;
		beam=co.beam;
	}
	return(*this);
}
