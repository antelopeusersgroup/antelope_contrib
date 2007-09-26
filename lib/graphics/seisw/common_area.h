//This include defines the data structures that are used during the display.
//They are different from parameters.h in that they are not in the parameter 
//file that controls the display, just some intermediate data structures that
//need to be put in a persistent storage during execution

#ifndef __COMMON_AREA_H
#define __COMMON_AREA_H

#include <vector>
#include <string.h>

using namespace std;
using namespace SEISPP;

class SeiswCA 
{
public:
    int nmember;
    vector<string> curvecolor;
    float * x2;
    float * z;
    int endian;
    Window win;
    int x,y,width,height;
    int imageOutOfDate;

    float d1,d2,f1,f2;
    int n1, n2;
    float clip;
    float p2beg, p2end;
    XImage *image;

    //for restore to the initial view after the user select a really small area (smaller than
    //4 pixels or double clik
    float x1begb_init, x1endb_init, x2begb_init, x2endb_init;

    //for use of displaying rubber box
    GC rubberbox_gc;
    float x1beg_rb,x1end_rb,x2beg_rb,x2end_rb;
    int old_xb, old_yb;
    unsigned char going_out;

    SeiswCA() {x2=NULL; z=NULL; image=NULL;}
    ~SeiswCA()
    {
	if (x2!=NULL) delete x2;
	if (z!=NULL) delete z;
        if (image!=NULL) delete image;
    }
};

#endif
