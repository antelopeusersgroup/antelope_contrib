
// This include defines the parameters that control the display of seismogram

#ifndef __PARAMETERS_H
#define __PARAMETERS_H

#include <stdlib.h>
#include <string.h>
#include "Metadata.h"
//#include "seispp.h"
#include "Seisw.h"
#include "xplot.h"

using namespace std;
using namespace SEISPP;

#define MAXPLOTSAMPLES 100000

class SeiswPar 
{
public:
    int wt;
    int va;
    bool clip_data;
    float perc;
    int wigclip;
    float xcur;
    int xbox, ybox, hbox, wbox;
    float x1beg,x1end,x2beg,x2end;
    float d1num, d2num, f1num, f2num;
    int n1tic, n2tic;
    int grid1, grid2;
    char *label1,*label2,*title,*windowtitle;
    char *labelfont,*titlefont;
    int style;     
    char *labelcolor,*titlecolor,*gridcolor;
    float labelsize,titlesize;
    double trace_spacing;
    double first_trace_offset;

    string time_scaling;
    string trace_axis_scaling;
    string trace_axis_attribute;
    float x1begb,x1endb,x2begb,x2endb;
    int interp;
    string default_curve_color;
    bool use_variable_trace_spacing;

    SeiswPar();

    ~SeiswPar() {
	if (label1!=NULL) free(label1);
        if (label2!=NULL) free(label2);
        if (title!=NULL) free(title);
	if (windowtitle!=NULL) free(windowtitle);
	if (labelfont!=NULL) free(labelfont);
	if (titlefont!=NULL) free(titlefont);
	if (labelcolor!=NULL) free(labelcolor);
	if (titlecolor!=NULL) free(titlecolor);
	if (gridcolor!=NULL) free(gridcolor);
    }

    void SetParameters(Metadata *);

private:
    int parse_grid_style(string);

};

#endif
