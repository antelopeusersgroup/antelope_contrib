#include <vector>
#include <algorithm>
#include <fstream>
#include <float.h>
#include "perf.h"
#include "dmatrix.h"
#include "seispp.h"
#include "stack.h"
using namespace SEISPP;
namespace SEISPP
{

    Stack::Stack()
    {
        // Assume the TimeSeries stack and vector objects
        // are properly initialized with default constructors
        fold=0;
        sumwt=0.0;
    }

    Stack::Stack(TimeSeriesEnsemble& d, TimeWindow twin)
    {
        const string base_error("Stack (BasicStack) constructor: ");
        int i;
        try
        {
            TimeWindow twd;                       // derived from twin using moveout
            double moveout;
            vector<TimeSeries> stackdata;
            int nmember=d.member.size();
            if(nmember<=0)
                throw SeisppError(base_error
                    + "Input ensemble is empty. ");
            stackdata.reserve(nmember);
            weights.reserve(nmember);
            for(i=0;i<nmember;++i)
            {
                if(d.member[i].live &&(!d.member[i].is_gap(twd)))
                {
                    moveout=d.member[i].get_double(moveout_keyword);
                    twd=twin.shift(moveout);
                    if( (twd.end<d.member[i].t0)
                        || (twd.start>d.member[i].endtime()) ) continue;
                    if(d.member[i].is_gap(twd))
                    {
                        weights.push_back(0.0);
                    }
                    else
                    {
                        stackdata.push_back(WindowData(d.member[i],twd));
                        weights.push_back(1.0);
                    }
                }
            }
            fold=stackdata.size();
            if(fold<=0) throw SeisppError(base_error
                    + "Ensemble has no live, gap-free data in stack window");
            stack=stackdata[0];
            for(i=1;i<fold;++i)
            {
                stack+=stackdata[i];
            }
            sumwt=static_cast<double>(fold);
            for(i=0;i<stack.ns;++i) stack.s[i]/=sumwt;
            // Compute the rms for applications needing
            // an absolute amplitude measure.  This form is
            // consistent with other methods below.
            double ampscale=dnrm2(stack.ns,&(stack.s[0]),1);
            stack.put(beam_rms_key,ampscale/sqrt(static_cast<double>(stack.ns)));
            /* We need to set these to some default values to allow this
             * option to be used in MultichannelCorrelator.  Without it 
             * that object will throw a MetadataGetError when trying to 
             * fetch these. */
            for(i=0;i<nmember;++i)
            {
                d.member[i].put(coherence_keyword,0.0);
                d.member[i].put(amplitude_static_keyword,1.0);
                d.member[i].put(stack_weight_keyword,1.0);
            }
            stacktype=BasicStack;
        }
        catch(...)
        {
            throw;
        }
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
    //    are MedianStack and RobustSNR.  Note that StackType is an enum.
    //
    //@}
    Stack::Stack(TimeSeriesEnsemble& d, TimeWindow stack_twin, TimeWindow robust_twin, StackType method,double power)
    {
        int i,j;
        double moveout;
        TimeWindow twd;
        const string basemessage("Stack object constructor:  ");
        // Used to test for a zero vector.  10.0 is a conservative fudge factor
        const double zero_test_scale(DBL_EPSILON*10.0);
        // a floor in the residual weight is useful to avoid Inf and NaN results
        const double rweight_floor(0.00001);
        double vzerotest;
        if( (robust_twin.start<stack_twin.start) || (robust_twin.end>stack_twin.end) )
            throw SeisppError(basemessage
                +string("Illegal time window specified\nStack time window must enclose window used for applying penalty function\n"));
        stacktype=method;                         // Might as well set this at the top.
        try
        {
            // Call alternate constructor an return immediately for
            // simple stack method.
            if(stacktype==BasicStack)
            {
                *this=Stack(d,stack_twin);
                return;
            }
            int ensemblesize=d.member.size();
            double ampscale;
            double nrmd,nrmr;
            vector<double> work;
            vector<double>lastbeam;
            const double CONVERGE=0.0001;
            double deltad;
            const int MAXIT=30;
            int iteration_count;
            int medposition;
            int nsamp,i0;
            dmatrix r,raw_data;

            // basic sanity check.
            if(ensemblesize<=0)
                throw SeisppError(basemessage+string("input ensemble contains no data"));
            double moveout;
            vector<TimeSeries> stackdata;
            vector<int> dindex;
            vector<double>rweight;
            vector<double>coh;
            vector<double>amplitude_statics;
            stackdata.reserve(ensemblesize);
            dindex.reserve(ensemblesize);
            weights.reserve(ensemblesize);
            coh.reserve(ensemblesize);
            amplitude_statics.reserve(ensemblesize);
            for(i=0;i<ensemblesize;++i)
            {
                if(d.member[i].live)
                {
                    moveout=d.member[i].get_double(moveout_keyword);
                    twd=stack_twin.shift(moveout);
                    // Large moveouts are not allowed and
                    // provide a way for caller to force discarding
                    // an input trace.  The gap processing could
                    // be set to handle this, but it is safer to
                    // run this test explicitly.
                    if( (twd.end<d.member[i].t0)
                        || (twd.start>d.member[i].endtime()) )
                        continue;
                    // Note reverse logic not the clearest, but most efficient for coding
                    if(!d.member[i].is_gap(twd))
                    {
                        try
                        {
                            stackdata.push_back(WindowData(d.member[i],twd));
                            dindex.push_back(i);
                        } catch (SeisppError& serr)
                        {
                            cerr << "Stacker:  problem processing ensemble member = "<<i<<endl;
                            cerr << "Data skipped"<<endl;
                            serr.log_error();
                        }

                    }
                }
            }
            fold=dindex.size();
            if(fold<=0)
                throw SeisppError(basemessage
                    + string("input ensemble has no data free of gaps in time window"));
            stack = stackdata[0];
            stack.t0=stack_twin.start;
            // Handle this special case or we get NaNs
            if(fold==1)
            {
                weights.push_back(1.0);
                sumwt=1.0;
                return;
            }
            for(i=0;i<stack.ns;++i) stack.s[i]=0.0;
            switch(method)
            {
                case MedianStack:
                case RobustSNR:
                    // I've had problems with code like this before.
                    // reserve doesn't seem to initialize to allow indexing to work
                    // without using something like push_back first.  For now
                    // I'm going to just do it that way anwyay.
                    work.resize(fold);
                    /* median stack*/
                    for(i=0;i<stack.ns;++i)
                    {
                        for(j=0;j<fold;++j)
                            work[j]=stackdata[j].s[i];
                        stack.s[i]=median<double>(work);
                    }
                    // Need to save this scale factor for beam
                    // to retain amplitude.  REtain this estimate
                    // using median as amplitude factor.  Intentionally
                    // ignore amplitudes in iterative loop below.
                    stack.put(beam_rms_key,
                        dnrm2(stack.s.size(),&(stack.s[0]),1)/sqrt(static_cast<double>(stack.ns)));
                    // The median is used as an initial estimate for the robust algorithm
                    // Here we break out if we want just the median
                    if(method==MedianStack) break;
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
                    if(i0<0)
                        throw SeisppError(basemessage
                            + string("robust window inconsistent with")
                            + string(" data window") );
                    raw_data=dmatrix(nsamp,fold);
                    r=dmatrix(nsamp,fold);        // residual matrix
                    raw_data.zero();
                    r.zero();
                    for(i=0;i<fold;++i)
                    {
                        dcopy(nsamp,&(stackdata[i].s[i0]),1,raw_data.get_address(0,i),1);
                        weights[i]=1.0;
                    }
                    // reuse work to hold the current beam in this time gate
                    work.resize(nsamp);
                    rweight.resize(fold);
                    coh.resize(fold);
                    amplitude_statics.resize(fold);
                    for(i=0;i<nsamp;++i) work[i]=stack.s[i0+i];
                    // Stack must be normalized
                    ampscale=dnrm2(nsamp,&(work[0]),1);
                    // careful of zero vector stacks
                    // would make 1/ampscale inf
                    vzerotest=zero_test_scale
                        *static_cast<double>(nsamp);
                    if(ampscale>=vzerotest)
                        dscal(nsamp,1.0/ampscale,&(work[0]),1);
                    lastbeam=work;
                    iteration_count=0;

                    do
                    {

                        sumwt = 0.0;
                        for(j=0;j<fold;++j)
                        {
                            ampscale=ddot(nsamp,&(work[0]),1,raw_data.get_address(0,j),1);
                            ampscale=abs(ampscale);
                            for(i=0;i<nsamp;++i)
                            {
                                r(i,j)=raw_data(i,j)-ampscale*work[i];
                            }
                            // This was in error in previous version.  Missed a
                            // scaling constant.
                            //weights[j]=ampscale/dnrm2(nsamp,r.get_address(0,j),1);
                            nrmd=dnrm2(nsamp,raw_data.get_address(0,j),1);
                            nrmr=dnrm2(nsamp,r.get_address(0,j),1);
                            /* set weight to the floor if any of these
                               quantities are zero */
                            if(ampscale<=vzerotest || nrmd<=vzerotest
                                || nrmr<=vzerotest)
                                rweight[j]=rweight_floor;
                            else
                                rweight[j]=ampscale/(nrmd*nrmr);

                            if(power!=1.0)
                                rweight[j]=pow(rweight[j],power);
                            if(nrmr==nrmd)
                                coh[j]=0.0;
                            else
                                coh[j]=1.0-((nrmr*nrmr)/(nrmd*nrmd));
                            if(coh[j]<0.0) coh[j]=0.0;
                            amplitude_statics[j]=ampscale;
                            sumwt+=rweight[j];
                        }
                        // Since this problem is linear we don't need to sum residuals
                        // but can form weighted sum of data directly each iteration.
                        for(i=0;i<nsamp;++i)work[i]=0.0;
                        for(j=0;j<fold;++j)
                        {
                            daxpy(nsamp,rweight[j],raw_data.get_address(0,j),1,
                                &(work[0]),1);
                        }
                        /* Stack must be normalized
                                               Do so carefully to avoid Inf or NaN with zero vectors.*/
                        ampscale=dnrm2(nsamp,&(work[0]),1);
                        if(ampscale>=vzerotest)
                            dscal(nsamp,1.0/ampscale,&(work[0]),1);
                        else
                            break;                // no use continuing if stack is zeros
                        // using a loop here to avoid unnecessary creation of another
                        // temporary vector.  This is a L1 norm of delta data computation.
                        // lastbeam and beam are normalized so this
                        // convergence criteria is relative to 1
                        //
                        for(i=0,deltad=0.0;i<nsamp;++i) deltad+=fabs(lastbeam[i]-work[i]);
                        //Normalize deltad by nsamp to make it less dependent on stack size
                        deltad /= static_cast<double>(nsamp);
                        lastbeam=work;
                        ++iteration_count;
                        //
                        // Require at least two passes because
                        // the first pass compares to median stack.
                        //
                    } while( (iteration_count<=2)
                        || ((deltad>CONVERGE)
                        && (iteration_count<MAXIT) ));
                    //
                    // Form the final stack as weighted sum
                    //
                    for(i=0;i<stack.ns;++i) stack.s[i]=0.0;
                    for(i=0;i<fold;++i)
                    {
                        daxpy(stack.ns,rweight[i],&(stackdata[i].s[0]),
                            1,&(stack.s[0]),1);
                    }
                    if(sumwt>=vzerotest)
                        dscal(stack.ns,1/sumwt,&(stack.s[0]),1);
                    //
                    // A simple algorithm for initializing
                    // outputs.  This is quite inefficient, but we'll
                    // see if it matters down the road.
                    //
                    weights.clear();
                    for(i=0;i<ensemblesize;++i)
                    {
                        weights.push_back(0.0);
                        d.member[i].put(coherence_keyword,0.0);
                        d.member[i].put(amplitude_static_keyword,0.0);
                        d.member[i].put(stack_weight_keyword,0.0);
                    }

                    //
                    // Now we set the valid data int eh parent
                    // ensemble metadata and return a parallel array
                    // for weights.
                    //
                    for(i=0;i<dindex.size();++i)
                    {
                        int id;
                        id=dindex[i];
                        weights[id]=rweight[i];
                        d.member[id].put(coherence_keyword,coh[i]);
                        d.member[id].put(amplitude_static_keyword,
                            amplitude_statics[i]);
                        d.member[id].put(stack_weight_keyword,rweight[i]);
                    }

                    break;
                case BasicStack:
                    throw SeisppError(basemessage
                        + "Logic error: Unexpected entry into BasicStack switch block.\n"
                        + string("Report this error to author."));
                    break;
                default:
                    throw SeisppError(basemessage
                        + string("coding error.  Passed unknown StackType.") );
            }

        }
        catch (...)
        {
            throw;
        }
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

}                                                 // Close of SEISPP namespace
