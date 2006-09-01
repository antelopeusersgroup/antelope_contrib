
// This include defines the parameters that control the display of seismogram

#ifndef __PARAMETERS_H
#define __PARAMETERS_H

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
    bool verbose;
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
    string plotfile;
    bool use_variable_trace_spacing;

    SeiswPar() {label1=NULL;label2=NULL;title=NULL;windowtitle=NULL;labelfont=NULL;titlefont=NULL;
		labelcolor=NULL;titlecolor=NULL;gridcolor=NULL;}

    ~SeiswPar() {
	if (label1!=NULL) delete label1;
        if (label2!=NULL) delete label2;
        if (title!=NULL) delete title;
	if (windowtitle!=NULL) delete windowtitle;
	if (labelfont!=NULL) delete labelfont;
	if (titlefont!=NULL) delete titlefont;
	if (labelcolor!=NULL) delete labelcolor;
	if (titlecolor!=NULL) delete titlecolor;
	if (gridcolor!=NULL) delete gridcolor;
    }

    void SetParameters(Metadata *);

private:
    int parse_grid_style(string);

};

#endif
