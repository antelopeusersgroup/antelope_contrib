#include <vector>
#include <map>
#include <string>
#include "stock.h"
#include "TimeWindow.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
/*! \brief Object to define areas of bad data.

Areas of bad data area  common issue in passive array data processing.
This object captures that idea without any notion of what bad means.
It simply allows one to ask a simple question about whether a time for
station is good or bad.

This version is simple but it may evolve if I make this a library to
have a larger set of methods.  It also is a bit misnamed as the input
file is assumed to define times when data is good not bad.    */
class GapDefinition
{
    public:
        GapDefinition(string fname);
        GapDefinition(){};
        vector<TimeWindow> get(string staname);
    private:
        map<string,vector<TimeWindow> > gapmap;
};
