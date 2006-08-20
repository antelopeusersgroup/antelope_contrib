#ifndef _STATIONCHANNELMAP_H_
#define _STATIONCHANNELMAP_H_
#include <string>
#include <map>
#include "stock.h"
#include "pf.h"
#include "ThreeComponentChannelMap.h"
namespace SEISPP {
using namespace std;
/*!
\brief Interface to station indexed list of channel mapping codes.

 Many seismic stations have multiple data streams that sample
 the same sensor output.  We have, for example, long period channels (LH.),
 broadband channels (BH.), high-low gain channels (HH. versus HL.), and
 probably others I can't think of at the moment.  An application commonly
 has to sort out the precedence of which channel is desired for that 
 application and/or what channels are potentially present for processing.
 This object encapsulates that concept through a simple interface that
 returns an object defining the mapping relationship for a given station
 and a method that returns the known channels for a given station.  
 Note that both of these methods are designed to always return something.
 The reason is that the constructor is designed with a "default" station
 that holds default conditions.  That is, if a station is "normal" it
 can simply not be defined here and the correct associations will be 
 returned.

\author Gary L. Pavlis
**/

class StationChannelMap
{
public:
/*!
  Constructs this object using a (complicated) Antelope parameter file 
  description of the operator.  

\exception SeisppError object if there are problems in parsing the parameter file
 contents.
**/
	StationChannelMap(Pf *pf);
/*!
 Constructs the StationChannelMap object from a formatted file description.

\exception SeisppError object if there are format problems.

\param fname file name to read data from. File is assumed be an asci file
 with lines in this format:  station, channel, channel_index, precedence.
 The channel_index and precedence are integers used to construct the 
 lower level ThreeComponentChannelMap objects used internally by this 
 object.
**/
	StationChannelMap(string fname);
/*!
 Standard copy constructor.
**/
	StationChannelMap(const StationChannelMap& parent);
/*!
 Standard assignment operator.
**/
	StationChannelMap& operator=(const StationChannelMap& parent);
/*!
 Returns an object defining the mapping operator for a station of interest, sta.

 Note if there is no information on the station defined by the key sta the 
 default station is used.  The "default" key is literal.  That is, one cannot
 have a station called default as it would clash with this feature.
**/
	ThreeComponentChannelMap channels(string sta);
private:
	map<string,ThreeComponentChannelMap> chanmap;
};

}  // End SEISPP namespace declaration
#endif
