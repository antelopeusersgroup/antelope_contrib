#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
using namespace std;

const string moveout_keyword("moveout"); //Extract moveout from metadata using this keyword
enum StackType {Basic, median, RobustSNR};

//@{
// Generalized stacking object.
// Stacking is a common theme in seismic data processing.  This generalizes the 
// concept in a single object.  The approach uses here is decidely not procedural.
// The philosphy of this object is that the object is created through the computation
// of the stack.  
//@}

class Stack
{
public:
	int fold;
	vector<double>weights;
	double sumwt;
	TimeSeries stack; 
	Stack(TimeSeriesEnsemble& d,TimeWindow twin);
	Stack(TimeSeriesEnsemble& d,TimeWindow stack_twin, TimeWindow robust_twin, StackType method);
	Stack(const Stack& old);
	Stack& operator=(const Stack& other);
private:
	StackType stacktype;
};
}  // End namespace declaration
