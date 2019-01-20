#include <sstream>
#include "seispp.h"
namespace SEISPP {
/*! \brief Extracts a requested time window of data from a parent TimeSeries object.
//
It is common to need to extract a smaller segment of data from a larger 
time window of data.  This function accomplishes this in a nifty method that
takes advantage of the methods contained in the BasicTimeSeries object for
handling time and data gaps.  
//
\return new TimeSeries object derived from  parent but windowed by input
//      time window range.
//
\exception SeisppError object if the requested time window does not overlap data
//
\param parent is the larger TimeSeries object to be windowed
\param tw defines the data range to be extracted from parent.
*/
TimeSeries WindowData(const TimeSeries& parent, const TimeWindow& tw)
{
	// Always silently do nothing if marked dead
	if(!(parent.live)) 
	{
		TimeSeries tmp;
		return(tmp);
	}

	// Start with some basic sanity checks and throw an exception if 
	// the request is stupid
	if( (tw.end<parent.t0) || (tw.start>parent.endtime()) )
	{
		char buf[256];
		ostringstream message(buf);
		message << "WindowData:  Window data mismatch" <<endl
			<< "Requested time window = " << tw.start <<" to "<<tw.end<<endl
			<< "Data time range = "<<parent.t0<<" to "<<parent.endtime()<<endl;
		throw SeisppError(message.str());
	}
	TimeSeries result(parent);
	result.ns=nint( (tw.end - tw.start)/parent.dt) + 1;
	result.s.resize(result.ns);
	result.t0=tw.start;
	// add gaps if there is a mismatch on the left or right
	if(tw.start<parent.t0) result.add_gap(TimeWindow(tw.start,parent.t0));
	if(tw.end>parent.endtime()) result.add_gap(TimeWindow(parent.endtime(),result.endtime()));
	//
	// Now copy the data to result
	// Setting the above gaps simplifies this process a lot.
	//	
	int sampnumber;
	double t;
	for(int i=0;i<result.ns;++i)
	{
		if(result.is_gap(i)) 
			result.s[i]=0.0;
		else
		{
			t=result.time(i);
			sampnumber=parent.sample_number(t);
			if(sampnumber>=parent.ns) 	
				// This should always be and essentially
				// redundant, but safer
				result.s[i]=0.0;
			else
				result.s[i]=parent.s[sampnumber];
		}
	}
	return(result);
}
/*  The following is a near copy for 3C data objects.  Might be a way to do this
with templates, but I'm not going to mess with that complication.  Forgiveness
requested of purists.*/

//@{
// Extracts a requested time window of data from a parent ThreeComponentSeismogram object.
//
// It is common to need to extract a smaller segment of data from a larger 
// time window of data.  This function accomplishes this in a nifty method that
// takes advantage of the methods contained in the BasicTimeSeries object for
// handling time and data gaps.  
//
//@returns new ThreeComponentSeismogram object derived from  parent but windowed by input
//      time window range.
//
//@throws SeisppError object if the requested time window does not overlap data
//
//@param parent is the larger ThreeComponentSeismogram object to be windowed
//@param tw defines the data range to be extracted from parent.
//@author Gary L. Pavlis
//@}
ThreeComponentSeismogram WindowData(const ThreeComponentSeismogram& parent, const TimeWindow& tw)
{
	// Always silently do nothing if marked dead
	if(!parent.live) 
	{
		// return(ThreeComponentSeismogram()) doesn't work
		// with g++.  Have to us this long form
		ThreeComponentSeismogram tmp;
		return(tmp);
	}

	// Start with some basic sanity checks and throw an exception if 
	// the request is stupid
	if( (tw.end<parent.t0) || (tw.start>parent.endtime()) )
	{
		char buf[256];
		ostringstream message(buf);
		message << "WindowData:  Window data mismatch" <<endl
			<< "Requested time window = " << tw.start <<" to "<<tw.end<<endl
			<< "Data time range = "<<parent.t0<<" to "<<parent.endtime()<<endl;
		throw SeisppError(message.str());
	}
	ThreeComponentSeismogram result(parent);
	result.ns=nint( (tw.end - tw.start)/parent.dt) + 1;
	result.u=dmatrix(3,result.ns);
	result.t0=tw.start;
	// add gaps if there is a mismatch on the left or right
	if(tw.start<parent.t0) result.add_gap(TimeWindow(tw.start,parent.t0));
	if(tw.end>parent.endtime()) result.add_gap(TimeWindow(parent.endtime(),result.endtime()));
	//
	// Now copy the data to result
	// Setting the above gaps simplifies this process a lot.
	//	
	int i,j;
        /* This oddity is necessary in current implementation to deal
           with a const issue I'm unable to figure out.  */
        dmatrix *uptr;
        uptr=const_cast<dmatrix *>(&(parent.u));
        double *ptr;
	for(i=0;i<result.ns;++i)
	{
		double t;
                int is;
		if(result.is_gap(i)) 
                        for(j=0;j<3;++j) result.u(j,i)=0.0;
		else
		{
			t=result.time(i);
                        is=parent.sample_number(t);  
                        if( (is<0) || (is>=result.ns) )
                            for(j=0;j<3;++j) result.u(j,i)=0.0;
                        else
			    for(j=0;j<3;++j)
                            {
                              ptr=uptr->get_address(j,parent.sample_number(t));
                              result.u(j,i)=(*ptr);
                            }
		}
	}
	return(result);
}

/*******************************************
//  Commented out for now.  export has not been adopted
//  by any compilers even though the standard specifies it
//  a book I uses says it should be used in this context.
//  For now WindowData is not a template anyway.  There
//  are specialized implementations for TimeSeries and 3c
//  ensembles.

export template <class T>
shared_ptr<T>WindowData(T& parent, TimeWindow& tw)
{
	shared_ptr<T> result=new T(parent);
	try{
		for(int i=0;i<parent.member.size();++i)
                	result->member[i]=WindowData(parent.member[i],tw);
        } catch (...) {throw;}
        return(result);
}
************************************************/
	
}
