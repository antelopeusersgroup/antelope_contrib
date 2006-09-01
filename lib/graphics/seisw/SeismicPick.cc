#include "SeismicPick.h"
#include "seispp.h"

using namespace std;
using namespace SEISPP;

SeismicPick::SeismicPick() 
{
	type = UNDEFINED;
	x1=0.0;
	x2=0.0;
	time=0.0; 
	amplitude=0.0;
	trace_number=-1;
	twin=TimeWindow(0.0,0.0);
	window_set=false;
	point_set=false;
}

SeismicPick::SeismicPick(float x1in, float x2in)
{
	type = POINT;
	x1=x1in;
	x2=x2in;
	time=static_cast<double>(x1);
	amplitude=0.0;
	trace_number=-1;
	twin=TimeWindow(0.0,0.0);
	window_set=false;
	point_set=false;
}
SeismicPick::SeismicPick(TimeWindow tw)
{
	type = WINDOW;
	x1=0.0;
	x2=0.0;
	time=0.0; 
	amplitude=0.0;
	trace_number=-1;
	twin=tw;
	window_set=true;
	point_set=false;
}
	

SeismicPick::SeismicPick(const SeismicPick& p) 
{
	type=p.type;
	x1=p.x1;
	x2=p.x2;
	time=p.time;
	amplitude=p.amplitude;
	trace_number=p.trace_number;
	twin=p.twin;
	window_set=p.window_set;
	point_set=p.window_set;
}
SeismicPick& SeismicPick::operator=(const SeismicPick& p)
{
	if(this!=&p)
	{
		type=p.type;
		x1=p.x1;
		x2=p.x2;
		time=p.time;
		amplitude=p.amplitude;
		trace_number=p.trace_number;
		twin=p.twin;
		window_set=p.window_set;
		point_set=p.window_set;
	}
	return(*this);
}
void SeismicPick::set_point(double t, double a)
{
	time=t;
	amplitude=a;
	point_set=true;
}
TimeWindow SeismicPick::get_window()
{
	if(type==WINDOW)
		return(twin);
	else
		throw SeisppError("SeismicPick::get_window:  defined pick is not a time window pick");
}
PointPick SeismicPick::get_point()
{
	if(type==POINT)
	{
		PointPick pick;
		pick.time=time;
		pick.amplitude=amplitude;
		return(pick);
	}
	else
		throw SeisppError("SeismicPick::get_point:  defined pick is not a point pick");
}
int SeismicPick::get_trace_number()
{
	if(type==POINT)
		return(trace_number);
	else
		throw SeisppError("SeismicPick::get_trace_number:  defined pick is not a point pick");
		
}

