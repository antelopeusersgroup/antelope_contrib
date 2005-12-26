#include <vector>
#include <algorithm>
#include "perf.h"
#include "dmatrix.h"
#include "seispp.h"
#include "stack.h"
using namespace SEISPP;
namespace SEISPP {

Stack::Stack(TimeSeriesEnsemble& d, TimeWindow twin)
{
    try {
	TimeWindow twd;  // derived from twin using moveout
	double moveout;
	int i;
	//This is a plain simple stacker using the += algorithm.  It arbitrarily clones
	// the metadata of the first member of the ensemble.
	if(d.member.size()<=0) 
	moveout=d.member[0].get_double(moveout_keyword);
	twd=twin.shift(moveout);
	stack=WindowData(d.member[0],twd); 
	weights.resize(d.member.size());
	for(i=1,fold=1;i<d.member.size();++i)
	{
		if(d.member[i].live)
		{
			moveout=d.member[i].get_double(moveout_keyword);
			twd=twin.shift(moveout);
			stack += WindowData(d.member[i],twd);
			weights.push_back(1.0);
			sumwt+=1.0;
			++fold;
		}
		else
		{
			weights.push_back(0.0);
		}
	}
	stacktype=BasicStack;
    } catch(...) {throw;}
}
//@{ Constructor for more complicated stacking methods selected by method variable.
// Selecting method=BasicStack and this function will cause an exception to be thrown.
// Somewhat brutal, but better than mucking around with allowing this function to 
// accept that form.  All other methods require building a matrix containing all
// valid data, which is drastically different than accumulating a sum.
//  
// The median method computes a median stack sample by sample
// The RobustSNR method uses a form of robust estimator using the median stack as a starting
// point and computing the stack using an interative method that applies a penalty function
// based on a measure off the signal-to-noise ratio.  The measure used is derived from
// residuals with respect to the current stack.  
//
//@throws SeisppError object for one of several reasons when a stack is impossible to compute.
//
// @param d Input data ensemble
// @stack_twin The moveout corrected data will be stacked over this time gate.
// @robust_twin The most coherent portion of most waveforms is the first few cycles.  The stack
//    is commonly desired over a much longer time window.  This allows a shorter window to be
//    used in applying the penalty function.  Note this time window must be completely enclosed
//    within the stack_win time gate or this constructor will throw an exception. 
// @param method choice of method to use.  Currently only valid choices for this constructor
//    are median and RobustSNR.  Note that StackType is an enum.
// 
//@}
Stack::Stack(TimeSeriesEnsemble& d, TimeWindow stack_twin, TimeWindow robust_twin, StackType method) 
{
	int i,j;
	double moveout;
	TimeWindow twd;
	const string basemessage("Stack object constructor:  ");
	if( (robust_twin.start<stack_twin.start) || (robust_twin.end>stack_twin.end) )
		throw SeisppError(basemessage
		  +string("Illegal time window specified\nStack time window must enclose window used for applying penalty function\n")); 
	stacktype=method; // Might as well set this at the top.
	try {
		int ensemblesize=d.member.size();
		double ampscale;
		vector<double> work;
		const double CONVERGE=0.0001;
		double deltad;
		const int MAXIT=50;
		int iteration_count;
		int medposition;
		int nsamp,i0;
		dmatrix r,raw_data;

		// basic sanity check.
		if(ensemblesize<=0)
			throw SeisppError(basemessage+string("input ensemble contains no data"));
		TimeWindow twd;  
		double moveout;
		vector<TimeSeries> stackdata;
		vector<int> dindex;
		vector<double>rweight;
		stackdata.resize(ensemblesize);
		dindex.resize(ensemblesize);
		weights.resize(ensemblesize);
		for(i=0;i<ensemblesize;++i)
		{
			if(d.member[i].live)
			{
				moveout=d.member[i].get_double(moveout_keyword);
				twd=stack_twin.shift(moveout);
				// Note reverse logic not the clearest, but most efficient for coding
				if(!d.member[i].is_gap(twd))
				{
					stackdata.push_back(WindowData(d.member[i],twd));
					dindex.push_back(i);
				}
			}
		}
		fold=dindex.size();
		if(fold<=0)
			throw SeisppError(basemessage 
				+ string("input ensemble has no data free of gaps in time window"));
		stack = stackdata[0];
		for(i=0;i<stack.ns;++i) stack.s[i]=0.0;
		switch(method)
		{
		case median:
		case RobustSNR:
			work.resize(fold);
			// I've had problems with code like this before.
			// resize doesn't seem to initialize to allow indexing to work 
			// without using something like push_back first.  For now
			// I'm going to just do it that way anwyay.
			medposition=fold/2;
			for(i=0;i<stack.ns;++i)
			{
				for(j=0;j<fold;++j)
					work[j]=stackdata[j].s[i];
				sort(work.begin(),work.end());
				if(fold%2)
					stack.s[i]=work[medposition];	
				else
					stack.s[i]=(work[medposition]+work[medposition-1])/2.0;
			}
			// The median is used as an initial estimate for the robust algorithm
			// Here we break out if we want just the median
			if(method==median) break;
			//
			// The penalty function we use here amounts to weighting by the
			// reciprocal of the signal-to-noise ratio.  It essentially assumes
			// residuals measure noise and signal is measured by the correlation of
			// the beam with a signal at peak correlation.  Note the data are 
			// normalized to unit power with an amplitude factor to make the 
			// penalty function nondimensional.  
			// Here we know the size of the data we will work with in this 
			// loop so we can used a matrix to represent it more easily than
			// the STL vectors used above.
			//
			nsamp=stack.sample_number(robust_twin.end)
				-stack.sample_number(robust_twin.start)+1;
			i0=stack.sample_number(robust_twin.start);
			raw_data=dmatrix(nsamp,fold);
			r=dmatrix(nsamp,fold);  // residual matrix
			for(i=0;i<fold;++i)
			{
				dcopy(nsamp,&(stackdata[i].s[i0]),1,raw_data.get_address(0,i),1);
				weights[i]=1.0;
			}
			// reuse work to hold the current beam in this time gate
			work.resize(nsamp);
			for(i=0;i<nsamp;++i) work[i]=stack.s[i0+i];
			
			do {
				// Stack must be normalized
				ampscale=dnrm2(nsamp,&(work[0]),1);
				dscal(nsamp,1.0/ampscale,&(work[0]),1);
				sumwt = 0.0;
				for(j=0;j<fold;++j)
				{
					ampscale=ddot(nsamp,&(work[0]),1,raw_data.get_address(0,j),1);
					for(i=0;i<nsamp;++i)
					{
						r(i,j)=raw_data(i,j)-ampscale*work[i];
					}
					weights[j]=ampscale/dnrm2(nsamp,r.get_address(0,j),1);
					sumwt+=weights[j];
				}
				// Since this problem is linear we don't need to sum residuals
				// but can form weighted sum of data directly each iteration.
				for(i=0;i<nsamp;++i)work[i]=0.0;
				for(j=0;j<fold;++j)
				{
					daxpy(nsamp,weights[j],raw_data.get_address(0,j),1,
							&(work[0]),1);
				}
				dscal(nsamp,1.0/sumwt,&(work[0]),1);
				// using a loop here to avoid unnecessary creation of another
				// temporary vector.  This is a L1 norm of delta data computation.
				//
				for(i=0,deltad=0.0;i<nsamp;++i) deltad+=fabs(stack.s[i]-work[i]);
				//Normalize deltad by nsamp to make it less dependent on stack size
				deltad /= static_cast<double>(fold);
			} while( (deltad>CONVERGE) && (iteration_count<MAXIT) );
			// A simple algorithm for setting weights array.  Inefficient for sparse hits, but
			// but that would be rare and unimportant for efficiency here anyway
			for(i=0;i<ensemblesize;++i) weights[i]=0.0;
			for(i=0;i<dindex.size();++i) weights[dindex[i]]=rweight[i];
			break;
		case BasicStack:
			throw SeisppError(basemessage
				+ string("coding error.  Use Stack(x,x) to invoke Basic method."));
			break;
		default:
			throw SeisppError(basemessage
				+ string("coding error.  Passed unknown StackType.") );
		}

	} catch (...) {throw;}
}
Stack::Stack(const Stack& old)
{
	fold=old.fold;
	weights=old.weights;
	sumwt=old.sumwt;
	stack=old.stack;
	stacktype=old.stacktype;
}
Stack& Stack::operator=(const Stack& old)
{
	if(this!=&old)
	{
		fold=old.fold;
		weights=old.weights;
		sumwt=old.sumwt;
		stack=old.stack;
		stacktype=old.stacktype;
	}
	return(*this);
}

} // Close of SEISPP namespace
