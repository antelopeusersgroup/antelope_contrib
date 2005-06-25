#include "seispp.h"
namespace SEISPP {
TimeWindow::TimeWindow(const TimeWindow& old)
{
	start=old.start;
	end=old.end;
}
TimeWindow TimeWindow::shift(double tshift)
{
	TimeWindow newwindow(*this);
	newwindow.start+=tshift;
	newwindow.end += tshift;
	return(newwindow);
}
TimeWindow& TimeWindow::operator=(const TimeWindow& old)
{
	if(this!=&old)
	{
		start=old.start;
		end=old.end;
	}
	return(*this);
}
}
