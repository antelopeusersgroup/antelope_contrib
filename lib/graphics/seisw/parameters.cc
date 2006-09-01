#include "parameters.h"

using namespace std;
using namespace SEISPP;

void SeiswPar::SetParameters(Metadata * md)
{
    string s;
    bool bval;

    verbose=md->get_bool("verbose");
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
    windowtitle=strdup(s.c_str());
    s=md->get_string("style");
    d1num=static_cast<float>(md->get_double("d1num"));
    d2num=static_cast<float>(md->get_double("d2num"));
    f1num=static_cast<float>(md->get_double("f1num"));
    f2num=static_cast<float>(md->get_double("f2num"));
    n1tic=md->get_int("n1tic");
    n2tic=md->get_int("n2tic");
    s=md->get_string("label1");
    label1=strdup(s.c_str());
    s=md->get_string("blabel2");
    label2=strdup(s.c_str());
    s=md->get_string("title");
    title=strdup(s.c_str());

    s=md->get_string("windowtitle");
    windowtitle=strdup(s.c_str());
    s=md->get_string("labelfont");
    labelfont=strdup(s.c_str());
    s=md->get_string("titlefont");
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
    labelcolor=strdup(s.c_str());
    s=md->get_string("titlecolor");
    titlecolor=strdup(s.c_str());
    s=md->get_string("gridcolor");
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
    plotfile=md->get_string("plot_file_name");
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

