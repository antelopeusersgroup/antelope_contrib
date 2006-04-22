#ifndef _SEISPP_KEYWORDS_H_
#define _SEISPP_KEYWORDS_H_
#include <string>
namespace SEISPP {
using namespace std;
const string moveout_keyword("moveout"); //Extract moveout from metadata using this keyword
const string coherence_keyword("coherence"); // key for storing coherence
const string amplitude_static_keyword("amplitude_static");
const string stack_weight_keyword("stack_weight");
const double MoveoutBad=1.0e10;
const double MoveoutBadTest=1.0e8; // Intentionally much smaller as a safe float test
};
#endif
