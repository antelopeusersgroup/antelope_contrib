#include "parameters.h"

using namespace std;
using namespace SEISPP;
/* Default constructor initializes all members of this structure.
The SetParameters method below will normally override these defaults, 
but this is a safer approach to initialization than older code written
by Peng Wang.  

Codin in fixed constants for this initialization is a bit ugly.  It could
be avoided by making all these parameter X Resources, but I felt that
created a whole new can of worms best left closed.  
*/
SeiswPar::SeiswPar()
{
	wt=1;
	va=1;
	clip_data=true;
	perc=99.0;
	wigclip=true;
	xcur=1.0;
	xbox=50;
	ybox=50;
	wbox=1000;
	hbox=1000;
	x1beg=0.0;
	x1end=120.0;
	x2beg=0.0;
	x2end=24.0;
	d1num=0.0;
	d2num=0.0;
	f1num=0.0;
	f2num=0.0;
	grid1=1;
	grid2=1;
	n1tic=5;
	n2tic=1;
	label1=strdup("time");
	label2=strdup("index number");
	title=strdup("Data");
	windowtitle=strdup("SeismicPlot");
	labelcolor=strdup("blue");
	titlecolor=strdup("red");
	gridcolor=strdup("blue");
	
	labelfont=strdup("Rom14");
	titlefont=strdup("Rom22");
	labelsize=18.0;
	titlesize=24.0;
	trace_spacing=1.0;
	trace_axis_scaling=string("auto");
	trace_axis_attribute=string("assoc.delta");
	x1begb=x1beg;
	x2begb=x2beg;
	x1endb=x1end;
	x1endb=x1end;
	interp=1;
	default_curve_color=string("black");
	use_variable_trace_spacing=false;
}

void SeiswPar::SetParameters(Metadata * md)
{
    string s;
    bool bval;

    bval=md->get_bool("WiggleTrace");
    if(bval)
    {
        wt=md->get_int("SUVariableArea_grey_value");
        if(wt<0) wt=0;
        if(wt>5) wt=5;
    }
    else
        wt=0;
    bval=md->get_bool("VariableArea");
    if(bval)
    {
        va=md->get_int("SUVariableArea_grey_value");
        if(va<0) va=0;
        if(va>5) va=5;
    }
    else
        va=0;

    clip_data=md->get_bool("clip_data");
    if(clip_data)
        perc=static_cast<float>(md->get_double("clip_percent"));
    bval=md->get_bool("clip_wiggle_traces");
    if(bval)
        wigclip=1;
    else
        wigclip=0;
    // wiggle excursion in traces for clip
    // cryptic name, but no obvious longer name that is self-documenting
    xcur=static_cast<float>(md->get_double("xcur"));

    xbox=md->get_int("xbox");
    ybox=md->get_int("ybox");
    wbox=md->get_int("wbox");
    hbox=md->get_int("hbox");
    s=md->get_string("windowtitle");
    if(windowtitle!=NULL) free(windowtitle);
    windowtitle=strdup(s.c_str());
    s=md->get_string("style");
    d1num=static_cast<float>(md->get_double("d1num"));
    d2num=static_cast<float>(md->get_double("d2num"));
    f1num=static_cast<float>(md->get_double("f1num"));
    f2num=static_cast<float>(md->get_double("f2num"));
    n1tic=md->get_int("n1tic");
    n2tic=md->get_int("n2tic");
    s=md->get_string("label1");
    if(label1!=NULL) free(label1);
    label1=strdup(s.c_str());
    if(label2!=NULL) free(label2);
    s=md->get_string("blabel2");
    label2=strdup(s.c_str());
    if(title!=NULL) free(title);
    s=md->get_string("title");
    title=strdup(s.c_str());

    s=md->get_string("windowtitle");
    if(windowtitle!=NULL) free(windowtitle);
    windowtitle=strdup(s.c_str());
    s=md->get_string("labelfont");
    if(labelfont!=NULL) free(labelfont);
    labelfont=strdup(s.c_str());
    s=md->get_string("titlefont");
    if(titlefont!=NULL) free(titlefont);
    titlefont=strdup(s.c_str());
    s=md->get_string("style");
    if(s=="seismic" || s=="SEISMIC")
        style=SEISMIC;
    else
        style=NORMAL;
    s=md->get_string("time_axis_grid_type");
    grid1=parse_grid_style(s);
    s=md->get_string("trace_axis_grid_type");
    grid2=parse_grid_style(s);

    s=md->get_string("labelcolor");
    if(labelcolor!=NULL) free(labelcolor);
    labelcolor=strdup(s.c_str());
    s=md->get_string("titlecolor");
    if(titlecolor!=NULL) free(titlecolor);
    titlecolor=strdup(s.c_str());
    s=md->get_string("gridcolor");
    if(gridcolor!=NULL) free(gridcolor);
    gridcolor=strdup(s.c_str());
    grid1=md->get_int("grid1");
    grid2=md->get_int("grid2");
    labelsize=static_cast<float>(md->get_double("labelsize"));
    titlesize=static_cast<float>(md->get_double("titlesize"));
    trace_spacing=md->get_double("trace_spacing");
    first_trace_offset=md->get_double("first_trace_offset");
    time_scaling=md->get_string("time_scaling");
    x1beg=static_cast<float>(md->get_double("x1beg"));
    x1end=static_cast<float>(md->get_double("x1end"));
    trace_axis_scaling=md->get_string("trace_axis_scaling");
    x2beg=static_cast<float>(md->get_double("x2beg"));
    x2end=static_cast<float>(md->get_double("x2end"));
    bval=md->get_bool("interpolate");
    if(bval)
        interp=1;
    else
        interp=0;

    default_curve_color=md->get_string("default_curve_color");
    // variable trace spacing option
    use_variable_trace_spacing=md->get_bool("use_variable_trace_spacing");

}

int SeiswPar::parse_grid_style(string keyword)
{
        if(keyword=="none")
                return NONE;
        else if(keyword=="dash")
                return DASH;
        else if(keyword=="solid")
                return SOLID;
        else
                return NONE;
}

