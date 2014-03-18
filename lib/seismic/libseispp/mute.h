#ifndef _MUTE_H_
#define _MUTE_H_
#include <algorithm>
#include <string>
#include "stock.h"
#ifndef NO_ANTELOPE
#include "pf.h"
#endif
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"

namespace SEISPP
{
//
//  Mute definitions
//
/*! \brief Defines a top mute zone. 

// Top Mutes are a common concept in reflection processing.  They are used to 
// zero out sections of data with an optional taper of a specified width.  
// This object encapsulates the idea of a top mute in a simplified package.
//\author Gary L. Pavlis
**/
class TopMute
{
public:
/*! Turns the mute on or off.

if true mute will be applied if used.  If false ApplyTopMute
functions will be return immediately doing nothing.*/
	bool enabled;
/*!
// Time of end of zero mute region.  The start of the zero region of a top
// mute is always assumed to be the start of data.  Data will be zeroed from 
// start to this time.  
**/
	double t0e;
/*!
// End of taper region.  This top mute object defines a linear taper from
// 0 to 1 between t0e and t1.  
**/
        double 	t1;  
/*!
// Time reference type.  Defined by an enum in seispp as absolute or relative.  
// If relative time is used the t0e and t1 times are assumed to be computed relative
// to the first sample of data.  If absolute the actual value of t0 for the data file
// is referenced and times are presumed to be relative to that standard.
**/
	TimeReferenceType reftype;   
	//* Default constructor */
	TopMute(){t0e=1.0; t1=2.0; reftype=relative; enabled=false;};
#ifndef NO_ANTELOPE
/*!
// Parameter file driven constructor.  
// Looks for three keyword strings to set the three data parameters
// that define the object.  They are:  Zero_End_Time, End_Time, and TimeReferenceType 
// which reference t0, t1e, and reftype respectively.  
//
//\param pf Antelope parameter file pf object.
//\param tag is a string that defines an &Arr{} enscapsulation of the parameters
//   parsed for the mute definition. The nesting of an &Arr{ } with the parameters
//   between the curly brackets allows the same keywords to be used in multiple
//   constructors for different mute definitions.
**/
	TopMute(Pf *pf,string tag);
#endif
/*!  \brief Parameter file style contructor.

  This constructor is similar to the Pf version and is effectively a replacement 
  for the NO_ANTELOPE option.   The usage is essentially identical except the
  required data comes from a PfStyleMetadata object instead of the plain 
  C Pf* that has long been a part of the Antelope software.

  Looks for three keyword strings to set the three data parameters
  that define the object.  They are:  Zero_End_Time, End_Time, and TimeReferenceType
  which reference t0, t1e, and reftype respectively.

  \param  md is the object constructed from a Pf file that encapsulates the pf interface.
  \param tag is a string that defines an &Arr{} enscapsulation of the parameters
   parsed for the mute definition. The nesting of an &Arr{ } with the parameters
   between the curly brackets allows the same keywords to be used in multiple
   constructors for different mute definitions.
   */
        TopMute(PfStyleMetadata& md,string tag);
};
/*!
// Applies a top mute to a TimeSeries object.
**/
void ApplyTopMute(TimeSeries &ts,TopMute& mute);
/*!
// Applies a top mute to a ThreeComponentSeismogram object.
**/
void ApplyTopMute(ThreeComponentSeismogram& ts,TopMute& mute);
/*!
// Applies a single top mute definition to all members of a TimeSeriesEnsemble.
**/
void ApplyTopMute(TimeSeriesEnsemble& t, TopMute& mute);
/*!
// Applies a single top mute definition to all members of a ThreeComponentEnsemble.
**/
void ApplyTopMute(ThreeComponentEnsemble &t3c, TopMute& mute);
} // End SEISPP namespace declaration
#endif
