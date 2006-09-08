#ifndef _THREECOMPONENTCHANNELMAP_H_
#define _THREECOMPONENTCHANNELMAP_H_
#include <string>
#include <map>
#include "stock.h"
#include "pf.h"
namespace SEISPP {
using namespace std;
/*! \brief Three component channel map definition.

 Object to handle mapping of channel codes used in external representations 
 of seismic data into three component indexing scheme (includes an optiona heirarchy 
 of names).  

 In seismic data processing with three component seismograms data are frequently 
 represented externally by channel codes (e.g. BHZ, HHZ, etc.)  Three component
 seismogram objects internally consider one channel to be one component of the
 a three element vector stored as a 3xn matrix.  A further complication is that
 there are often multiple channels stored for the same data stream.  For example,
 we commonly have LHZ, BHZ, and GHZ channels at GSN stations.  A different example
 is older high/low gain data that contain the same data sample rate but with 
 different values of calib.  This object was designed to encapsulate this concept
 and provide a general mechanism for defining how external names map to internal
 component names.  
 \author Gary L. Pavlis
**/
class ThreeComponentChannelMap
{
public:
/*!
 Construct this object from an Antelope Pf.
 
 Looks for the pf Tbl element associated with the key tag.
 The associated &Tbl list is assumed to be followed by lines defining
 the object components in the following order:  
 chan, component, level.  chan=channel name; component is an integer component
 number of 0, 1, or 2;  and level is the precedence level.
 
\param pf Antelope parameter file handle
\param tag is the key used to find Tbl to use to build this object
\author Gary L. Pavlis
**/
	ThreeComponentChannelMap(Pf *pf,string tag="ThreeComponentChannelMap");
/*!
 Construct this object from a string.  String is assumed to be 
 same format as the &Tbl entry for the pf constructor.
 Format assumes newline characters between lines as in a Tbl.
 The pf version, in fact, actually uses this constructor
**/
	ThreeComponentChannelMap(string buffer);
/*!
 Standard copy constructor.
**/
	ThreeComponentChannelMap(const ThreeComponentChannelMap& parent);
/*! 
Standard assignment operator.
*/
	ThreeComponentChannelMap& operator=
			(const ThreeComponentChannelMap& parent);
/*!
 Returns componnt number (0, 1, or 2) of input channel code.

 \param chan channel code for which index is requested.
 \exception SeisppError object if chan is not defined.
 \author Gary L. Pavlis
**/
	int component(string chan);
/*!
 Returns precedence level for a given channel code.

 Channels are assumed to define a hierarchy with the precedence code
 defining the order of importance.  For example, if BHZ and HHZ are both
 defined for a station but BHZ has level 0 while HHZ is level 1, BHZ will
 always be used instead of HHZ.  
**/
	int precedence(string chan);
private:
/*! map associating channel codes to row position in 3c seismogram object.
**/
	map<string,int> channels;
/*! Stores hierarchy of each channel. */ 
	map<string,int> level;
};
} // End SEISPP Namespace declaration
#endif
