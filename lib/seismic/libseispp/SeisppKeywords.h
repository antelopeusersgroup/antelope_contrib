#ifndef _SEISPP_KEYWORDS_H_
#define _SEISPP_KEYWORDS_H_
#include <string>
namespace SEISPP {
using namespace std;
/*! Defines a keyword to reference moveout used by the generalized stack operator.*/
const string moveout_keyword("moveout"); 
/*! Defines keyword for coherence estimate for generalized stack operator. */
const string coherence_keyword("coherence"); 
/*! Defines keyword for amplitude stack estimate for MultichannelCorrelator operator.*/
const string amplitude_static_keyword("amplitude_static");
/*! Defines keyword for weight used for robust stack estimate in generalized stack operator. */
const string stack_weight_keyword("stack_weight");
/*! Defines keyword for a predicted arrival time. */
const string predicted_time_key("predarr.time");
/*! Defines keyword for a generic measured arrival time. Phase type is normally a different attribute.*/
const string arrival_time_key("arrival.time");
/*! Used to flag a moveout estimate as bad.  Set to this large value. */
const double MoveoutBad=1.0e10;
/*! A test for a bad moveout value uses this number.  It is intentionally much
smaller than MoveoutBad for safety sake. */
const double MoveoutBadTest=1.0e8; 
};
#endif
