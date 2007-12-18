#ifndef _SIGNALTONOISE_H_
#define _SIGNALTONOISE_H_
#include <vector>
#include "TimeWindow.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"

namespace SEISPP 
{
using namespace std;
using namespace SEISPP;
/*! \brief Generic algorithm to compute signal-to-noise ratio by an RMS method for an ensemble.

Signal to noise ratio (SNR) is a standard concept in time series data analyis and often an
essential tool for dealing with real data.  There are multiple definitions of signal to 
noise ratio.  This generic algorithm is used to compute SNR using an rms measure over
a specified time gate defined by the signal parameter.  The algorithm assumes there exists
an procedure called SNR_rms(class T&, TimeWindow signal)  that can be called to compute
SNR or each member of the ensemble.  

Live members of the input ensmemble will have a metadata attribute defined by the argument
key set to the computed SNR value.  If an input member is marked dead the SNR key 
value will be set to a negative value, which is an invalid SNR estimate since 
SNR is always a positive number.  This means the SNR_rms method must not try to 
convert SNR estimates to db, which could be negative.

This algorithm differs slightly from a similar generic algorithm that contains
another argument to define an explicit noise window.  This method is more 
simplistic assuming that all data in each member before the start time of the
signal window is noise.  

\param Tens is the class name of the ensemble to be processed.
\param Tmember is the type of the member vector elements of the input ensemble.
\param d data ensemble to be processed
\param signal is the signal time window.
\param key is the keyword used to put the computed SNR to each data member.  i.e. the
	estimate SNR computed here can be retrieved with a call to get_double with
	this key applied to each data member. 
*/
template <class Tens, class Tmember>
	void ensemble_SNR_rms(Tens& d, TimeWindow signal, const string key)
{
	double snr;
	/*
	vector<Tmember>::iterator memptr;
	for(memptr=d.member.begin();memptr!=d.member.end();++memptr)
	*/
	Tmember *memptr;
	for(int i=0;i<d.member.size();++i)
	{
		memptr=&(d.member[i]);
		if(memptr->live)
		{
			snr=SNR_rms(*memptr,signal);
//cout << "SNR="<<snr<<endl;
			memptr->put(key,snr);
		}
		else
		{
			memptr->put(key,-1.0);
//cout << "Dead Trace, SNR set to -1.0"<<endl;
		}
	}
}
/*! \brief Generic algorithm to compute signal-to-noise ratio by an RMS method for an ensemble.

Signal to noise ratio (SNR) is a standard concept in time series data analyis and often an
essential tool for dealing with real data.  There are multiple definitions of signal to 
noise ratio.  This generic algorithm is used to compute SNR using an rms measure over
a specified time gate defined by the signal parameter and a noise window
defined by the noise parameter.  The algorithm assumes there exists
an procedure called SNR_rms(class T&, TimeWindow signal, TimeWindow noise)  
that can be called to compute SNR for each member of the ensemble.

Live members of the input ensmemble will have a metadata attribute defined by argument
key set to the computed SNR value.  If an input member is marked dead the SNR key 
value will be set to a negative value, which is an invalid SNR estimate since 
SNR is always a positive number.  This means the SNR_rms method must not try to 
convert SNR estimates to db, which could be negative.

This algorithm differs slightly from a similar generic algorithm that contains
only one TimeWindow for the signal range.  This algorithm gives more flexibility
in defining then noise time window. 

\param Tens is the class name of the ensemble to be processed.
\param Tmember is the type of the member vector elements of the input ensemble.
\param d data ensemble to be processed
\param signal is the signal time window.
\param noise is the noise time window.
\param key is the keyword used to put the computed SNR to each data member.  i.e. the
	estimate SNR computed here can be retrieved with a call to get_double with
	this key applied to each data member. 
*/
template <class Tens, class Tmember>
	void ensemble_SNR_rms(Tens& d, TimeWindow signal, 
		TimeWindow noise, const string key)
{
	double snr;
	/*
	vector<Tmember>::iterator memptr;
	for(memptr=d.member.begin();memptr!=d.member.end();++memptr)
	*/
	Tmember *memptr;
	for(int i=0;i<d.member.size();++i)
	{
		memptr=&(d.member[i]);
		if(memptr->live)
		{
			snr=SNR_rms(*memptr,signal,noise);
			memptr->put(key,snr);
		}
		else
		{
			memptr->put(key,-1.0);
		}
	}
}
/* Compute rms over a range of samples testin each for gaps.
Return negative number if interval is invalid or all gaps. 
Otherwise return rms. */
double compute_rms_with_gaps(TimeSeries& d, SampleRange& srange);
double compute_rms(TimeSeries&d, TimeWindow win);
double SNR_rms(TimeSeries& d, TimeWindow signal, TimeWindow noise);
double SNR_rms(TimeSeries& d, TimeWindow signal);
double SNR_rms(ThreeComponentSeismogram& d, TimeWindow signal, TimeWindow noise);
double SNR_rms(ThreeComponentSeismogram& d, TimeWindow signal);

} // end SEISPP namespace encapsulation
#endif
