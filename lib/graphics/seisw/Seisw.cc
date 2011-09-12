/* $XConsortium: Seisw.c /main/5 1995/07/15 20:41:24 drk $ */
/*
 *  @OPENGROUP_COPYRIGHT@
 *  COPYRIGHT NOTICE
 *  Copyright (c) 1990, 1991, 1992, 1993 Open Software Foundation, Inc.
 *  Copyright (c) 1996, 1997, 1998, 1999, 2000 The Open Group
 *  ALL RIGHTS RESERVED (MOTIF). See the file named COPYRIGHT.MOTIF for
 *  the full copyright text.
 *  
 *  This software is subject to an open license. It may only be
 *  used on, with or for operating systems which are themselves open
 *  source systems. You must contact The Open Group for a license
 *  allowing distribution and sublicensing of this software on, with,
 *  or for operating systems which are not Open Source programs.
 *  
 *  See http://www.opengroup.org/openmotif/license for full
 *  details of the license agreement. Any use, reproduction, or
 *  distribution of the program constitutes recipient's acceptance of
 *  this agreement.
 *  
 *  EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS
 *  PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT LIMITATION, ANY
 *  WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT, MERCHANTABILITY
 *  OR FITNESS FOR A PARTICULAR PURPOSE
 *  
 *  EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, NEITHER RECIPIENT
 *  NOR ANY CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING WITHOUT LIMITATION LOST PROFITS), HOWEVER CAUSED
 *  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OR DISTRIBUTION OF THE PROGRAM OR THE
 *  EXERCISE OF ANY RIGHTS GRANTED HEREUNDER, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGES.
 */
/*
 * HISTORY
 */
/*********************************** WARNING **********************************
 *
 * ExmSeisw is a demonstration widget.  OSF provides this widget
 * solely to teach programmers how to write their own Motif widgets.
 * OSF does not support this widget in any way
 *********************************** WARNING *********************************/


/******************************************************************************
 *
 * Seisw.c - ExmSeisw widget.  This widget renders a rectangle or an oval. 
 *            The ExmSeisw widget demonstrates how to create a
 *            subclass of XmPrimitive that can, itselve, serve as a superclass
 *            for other widgets.  ExmSeisw also demonstrates how to  
 *               * create a new representation type (ExmRseiswShape).
 *               * install the XmQTcontainerItem trait and its trait methods.
 *               * install the XmQTcareParentVisual trait and methods.
 *               * access the trait methods of the XmQTcontainer trait. 
 *            See the "OSF/Motif Widget Writer's Guide" for more details.
 *
******************************************************************************/
/* Some standard X file seems to have a guard problem.  This won't
compile on either solaris or g++ if these appear after the Xm includes.
--GLP August 1, 2006*/
#include <string.h>
#include <vector>
#include "seispp.h"
#include "TimeSeries.h"
#include "ensemble.h"

/* Include appropriate header files. */ 
#include "SeiswP.h"    /* private header file for the ExmSeisw widget */
#include <Xm/DrawP.h>       /* header file for Xme drawing functions */
#include <Xm/RepType.h>     /* header file for representation type facility */
#include <Xm/Screen.h>      /* header file for screen information */
#include <Xm/TraitP.h>      /* header file for installing traits */
#include <Xm/CareVisualT.h> /* header file for XmQTcareParentVisual trait */
#include <Xm/ContItemT.h>   /* header file for XmQTcontainerItem trait */
#include <Xm/ContainerT.h>  /* header file for XmQTcontainer trait */
/* for scrolling */
#include <Xm/ClipWindowP.h>
#include <Xm/NavigatorT.h>
#include <Xm/ScrollBarP.h>      /* might be worth getting rid of this one */
#include <Xm/TransltnsP.h>

#include "parameters.h"
#include "common_area.h"
#include "SeismicPick.h"
#include "display_marker.h"
#include "xplot.h"

//using namespace std;
//using namespace SEISPP;

#define TOPLEAVE        1
#define BOTTOMLEAVE     2
#define LEFTLEAVE       4
#define RIGHTLEAVE      8

#define ASSIGN_MAX(x,y)           if ((y) > (x)) x = (y)

/* The following section defines constants for the widget.  
*/
static XmConst int FIND_NATURAL_SIZE = 0;


/* Declare all static functions. */ 
static void ClassInitialize(void);
static void ClassPartInitialize (
                        WidgetClass widgetClass );
static void SetBox(Widget w);
static void HandlePreRender(Widget w);
static void SetClipRect(ExmSeiswWidget widget);
static void Initialize(
                        Widget request_w,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args );
static void Destroy (
                        Widget w);
static void Realize(
		    Widget w,
		    XtValueMask *p_valueMask,
		    XSetWindowAttributes *attributes );
static void Resize (
                        Widget w);
static void Redisplay (
                        Widget w,
                        XEvent *event,
                        Region region);
static Boolean SetValues (
                        Widget old_w,
                        Widget request_w,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args);
static XtGeometryResult QueryGeometry (
                        Widget w,
                        XtWidgetGeometry *request,
                        XtWidgetGeometry *reply);
static void DrawVisual (
                        Widget w);
static void DrawShadow (
                        Widget w);
static void CreateGC (
                        Widget w);
static void DestroyGC (
                        Widget w);
static GC SelectGC (
                        Widget w);
static void CalcVisualSize (
                        Widget w);
static void CalcWidgetSize (
                        Widget w);
static Boolean WidgetDisplayRect(
                        Widget       w,
                        XRectangle  *displayrect);
static void Reconfigure (
                        WidgetClass  class1,
                        Widget       new_w,
                        Widget       old_w);

static  Boolean HandleRedraw (Widget kid, 
			      Widget cur_parent,
			      Widget new_parent,
			      Mask visual_flag);
static void SetSelectedVisual (Widget wid) ;



/* translations, mouse/key bindings */

//Drag stuff
static void SeiswEnterProc(Widget wid,
          XEvent *event,
          String *params,
          Cardinal *num_params);
static void SeiswLeaveProc(Widget wid,
          XEvent *event,
          String *params,
          Cardinal *num_params);
static void
BrowseScroll(XtPointer closure,
             XtIntervalId *id);

static void create_rubberbox_gc(ExmSeiswWidget sw);
static void real_to_screen(float x1, float x2, int *xb, int *yb, ExmSeiswWidget sw);

static void Btn1MotionProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn3MotionProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn1DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn1UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn2DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn2UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn3DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);
static void Btn3UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params);

static void pick_x1_interval(Display *dpy,Window win,XEvent event,
        int style, int x, int y, int width, int height,
        int *xleft, int *xright);
static double screen_to_time(int pick,
                int offset,int plot_width,float x1begb,float x1endb);
static int get_trace_number(Widget w, float x2raw);
static SeismicPick MouseLocate(Display *dpy, Window win, XEvent event, int style,
        int x, int y, int width, int height,
        float x1begb, float x1endb, float x2begb, float x2endb,
        float p2beg, float p2end);
//in colormap.cc file
extern Status xCreateRGBDefaultMap (Display *dpy, XStandardColormap *scmap);

/* scrolling part */
/* call backs */
static void
_XmDestroyParentCallback_peng(
        Widget w,
        XtPointer client_data,  
        XtPointer call_data );

static void ComputeVizCount(ExmSeiswWidget nw, int * xcnt, int * ycnt);
static Boolean SetVerticalScrollbar(ExmSeiswWidget nw);
static Boolean SetHorizontalScrollbar(ExmSeiswWidget nw);
static void _XmSFUpdateNavigatorsValue(
                                       Widget sf,
                                       XmNavigatorData nav_data,
                                       Boolean notify);


static void update_y_display_range(ExmSeiswWidget sw);
static void update_x_display_range(ExmSeiswWidget sw);
static void SliderMove(
                        Widget w,
                        XtPointer closure,
                        XtPointer cd) ;


/* from SeismicPlot */

static void
xSizeAxesBox_peng (Display *dpy, Window win,
        char *labelfont, char *titlefont, int style,
        int *x, int *y, int *width, int *height);
static void load_z_matrix_peng(vector<TimeSeries> data,
                float *z,
                int n1,
                int n2,
                double t0,
                double dt);
static XImage *RotImage90_peng(Display *dpy, XImage *oldImage);
static double dsinc_peng(double x);
static void stoepd_peng (int n, double r[], double g[], double f[], double a[]);
void mksinc_peng (float d, int lsinc, float sinc[]);
static void intt8r_peng (int ntable, float table[][8],
        int nxin, float dxin, float fxin, float yin[], float yinl, float yinr,
        int nxout, float xout[], float yout[]);
static void ints8r_peng (int nxin, float dxin, float fxin, float yin[],
        float yinl, float yinr, int nxout, float xout[], float yout[]);
static void rfwtva_peng (
        int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian);
void rfwtvaint_peng(
        int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian);
static XImage *newBitmap_peng (Display *dpy, int width, int height,
        int n1, float d1, float f1, int n2, float *x2, float *z,
        float x1beg, float x1end, float x2beg, float x2end,
        float xcur, float clip, int wt, int va,
        float *p2begp, float *p2endp, int endian, int interp,
        int wigclip, int style);
void scaxis_peng (float x1, float x2, int *nxnum, float *dxnum, float *fxnum);
static void
xDrawAxesBox_peng (Display *dpy, Window win,
        int x, int y, int width, int height,
        float x1beg, float x1end, float p1beg, float p1end,
        float d1num, float f1num, int n1tic, int grid1, char *label1,
        float x2beg, float x2end, float p2beg, float p2end,
        float d2num, float f2num, int n2tic, int grid2, char *label2,
        char *labelfont, char *title, char *titlefont,
        char *axescolor, char *titlecolor, char *gridcolor,
        int style, int default_margin_width, int default_margin_height, int * origin);


/* Define translations. */
static char defaultTranslations[] =
"<EnterWindow>:		SeiswEnter()\n\
<LeaveWindow>:		SeiswLeave()\n\
<Btn1Down>:             Btn1DownProc()\n\
<Btn1Up>:                Btn1UpProc()\n\
<Btn2Down>:		 Btn2DownProc()\n\
<Btn2Up>:		 Btn2UpProc()\n\
<Btn3Down>:              Btn3DownProc()\n\
<Btn3Up>:                Btn3UpProc()\n\
<Btn1Motion>:		Btn1MotionProc()\n\
<Btn3Motion>:		Btn3MotionProc()";


/* The following actions will be handled by code inside this file. */
static XtActionsRec Actions[] = {
	{"SeiswEnter",		     SeiswEnterProc},
	{"SeiswLeave",              SeiswLeaveProc},
        {"Btn1DownProc",             Btn1DownProc},
        {"Btn1UpProc",               Btn1UpProc},
        {"Btn2DownProc",             Btn2DownProc},
        {"Btn2UpProc",               Btn2UpProc},
        {"Btn3DownProc",             Btn3DownProc},
        {"Btn3UpProc",               Btn3UpProc},
	{"Btn1MotionProc",	     Btn1MotionProc},
	{"Btn3MotionProc",           Btn3MotionProc}
};


/* Define the resources for the ExmSeisw widget. */
static XtResource resources[] = 
{
    /* scrolling related resources */
    {
        XmNhorizontalScrollBar, XmCHorizontalScrollBar,
        XmRWidget, sizeof(Widget),
        XtOffsetOf(ExmSeiswRec, seisw.hScrollBar),
        XmRImmediate, (XtPointer)NULL
    },
    {
        XmNverticalScrollBar, XmCVerticalScrollBar, XmRWidget, sizeof(Widget),
        XtOffsetOf(ExmSeiswRec, seisw.vScrollBar),
        XmRImmediate, (XtPointer)NULL
    },
    {
        XmNscrollBarDisplayPolicy, XmCScrollBarDisplayPolicy,
        XmRScrollBarDisplayPolicy, sizeof (unsigned char),
        XtOffsetOf(ExmSeiswRec, seisw.ScrollBarDisplayPolicy),
        XmRImmediate,  (XtPointer) XmSTATIC
    },
    /* seisic related part */
    {
        ExmNseiswEnsemble,   /* resource name */
        ExmCSeiswEnsemble,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.seisw_ensemble),  /* offset */
        XmRImmediate,     
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNseiswMetadata,   /* resource name */
        ExmCSeiswMetadata,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.seisw_metadata),  /* offset */
        XmRImmediate,     
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNseiswParameters,   /* resource name */
        ExmCSeiswParameters,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.seisw_parameters),  /* offset */
        XmRImmediate,     /* default value appears on next line */
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNseiswCommonArea,   /* resource name */
        ExmCSeiswCommonArea,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.seisw_ca),  /* offset */
        XmRImmediate,    
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNseiswPick,   /* resource name */
        ExmCSeiswPick,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.seisw_pick),  /* offset */
        XmRImmediate,     
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNcleanupData,   /* resource name */
        ExmCCleanupData,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.cleanup_data),  /* offset */
        XmRImmediate,     
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNdisplayMarkers,   /* resource name */
        ExmCDisplayMarkers,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.display_markers),  /* offset */
        XmRImmediate,     
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNdisplayAttributes,   /* resource name */
        ExmCDisplayAttributes,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.display_attributes),  /* offset */
        XmRImmediate,
        (XtPointer)NULL /* default to NULL */
    },
    {
        ExmNdeletedColor,   /* resource name */
        ExmCDeletedColor,   /* resource class */
        XmRPointer,   /* resource data type */
        sizeof (XtPointer),  /* all representation types have this size */
        XtOffsetOf( ExmSeiswRec, seisw.deleted_color),  /* offset */
        XmRImmediate,
        (XtPointer)NULL /* default to NULL */
    },
    {
	ExmNeditEnable,
	ExmCEditEnable,
	XmRHorizontalDimension,
	sizeof(Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.edit_enable),
	XmRImmediate,
	(XtPointer)0
    },
    {
    	ExmNbtn2Callback, XmCCallback, 
	XmRCallback,
        sizeof(XtCallbackList), 
	XtOffsetOf(ExmSeiswRec, seisw.btn2_callback),
        XmRCallback, 
	(XtPointer)NULL
    },
    {
        ExmNbtn3Callback, XmCCallback, 
	XmRCallback,
        sizeof(XtCallbackList), 
	XtOffsetOf(ExmSeiswRec, seisw.btn3_callback),
        XmRCallback, 
        (XtPointer)NULL
    },
    {
        ExmNdisplayAttrCallback, XmCCallback,
        XmRCallback,
        sizeof(XtCallbackList),
        XtOffsetOf(ExmSeiswRec, seisw.display_attr_callback),
        XmRCallback,
        (XtPointer)NULL
    },
    {
        ExmNdisplayOnly,
        ExmCDisplayOnly,
        XmRHorizontalDimension,
        sizeof(Dimension),
        XtOffsetOf( ExmSeiswRec, seisw.display_only),
        XmRImmediate,
        (XtPointer)0
    },
    {
        ExmNdefaultWidth,
        ExmCDefaultWidth,
        XmRHorizontalDimension,
        sizeof (Dimension),
        XtOffsetOf( ExmSeiswRec, seisw.default_width),
        XmRImmediate,
        (XtPointer)400
    },
    {
        ExmNdefaultHeight,
        ExmCDefaultHeight,
        XmRVerticalDimension,
        sizeof (Dimension),
        XtOffsetOf( ExmSeiswRec, seisw.default_height),
        XmRImmediate,
        (XtPointer)500
    },
    {
        ExmNzoomFactor,
        ExmCZoomFactor,
        XmRInt,
        sizeof (XmRInt),
        XtOffsetOf( ExmSeiswRec, seisw.zoom_factor),
        XmRImmediate,
        (XtPointer)100
    },
    /* original stuff from the Simple demo widget */
    {
 	XmNmarginWidth, 
	XmCMarginWidth, 
	XmRHorizontalDimension, 
	sizeof (Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.margin_width), 
	XmRImmediate,
	(XtPointer)0
    },
    {
	XmNmarginHeight, 
	XmCMarginHeight, 
	XmRVerticalDimension, 
	sizeof (Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.margin_height),
	XmRImmediate,
	(XtPointer)20
    }
};

/* Two of the three resources will also be handled as synthetic resources. */ 
static XmSyntheticResource syn_resources[] =
{
/*
    {
	ExmNseiswEnsemble,
	sizeof(XtPointer),
	XtOffsetOf( ExmSeiswRec, seisw.seisw_ensemble),
	NULL,
	NULL
    },
    {
	ExmNseiswMetadata,
	sizeof(XtPointer),
	XtOffsetOf( ExmSeiswRec, seisw.seisw_metadata),
	NULL,
	NULL
    },
    {
	ExmNdisplayMarkers,
	sizeof(XtPointer),
	XtOffsetOf( ExmSeiswRec, seisw.display_markers),
	NULL,
	NULL
    },
    {
 	ExmNeditEnable,
	sizeof(Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.edit_enable),
	NULL,
	NULL
    },
    {
        ExmNdisplayOnly,
        sizeof(Dimension),
        XtOffsetOf( ExmSeiswRec, seisw.display_only),
        NULL,
        NULL
    },
*/

    { 
	XmNmarginWidth, 
	sizeof (Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.margin_width), 
	XmeFromHorizontalPixels,
	XmeToHorizontalPixels 
    },
    { 
	XmNmarginHeight, 
	sizeof (Dimension),
	XtOffsetOf( ExmSeiswRec, seisw.margin_height),
	XmeFromVerticalPixels, 
	XmeToVerticalPixels 
    },
};



/* Define the widget class record.  See Chapter 3 of the
   "OSF/Motif Widget Writer's Guide" for details. */
 
/* Here is the XmPrimitive class extension record. */
static XmPrimitiveClassExtRec primClassExtRec = {
    /* next_extension */             NULL,
    /* record_type */                NULLQUARK,
    /* version */                    XmPrimitiveClassExtVersion,
    /* record_size */                sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline */            NULL,
    /* widget_display_rect */        NULL, //WidgetDisplayRect,
    /* widget_margins */             NULL,
};    
    
externaldef (exmseiswlassrec) ExmSeiswClassRec exmSeiswClassRec = {
  { /* Here is the core class record. */
    /* superclass */                 (WidgetClass)&xmPrimitiveClassRec,
    /* class_name */                 (char*)(string("ExmSeisw").c_str()),
    /* widget_size */                sizeof(ExmSeiswRec),
    /* class_initialize */           ClassInitialize,
    /* class_part_initialize */      ClassPartInitialize,
    /* class_inited */               FALSE,
    /* initialize */                 Initialize,
    /* initialize_hook */            NULL,
    /* realize */                    Realize,
    /* actions */                    Actions, //NULL,
    /* num_actions */                XtNumber(Actions),
    /* resources */                  resources,
    /* num_resources */              XtNumber(resources),
    /* xrm_class */                  NULLQUARK,
    /* compress_motion */            TRUE,
    /* compress_exposure */          XtExposeCompressMaximal,
    /* compress_enterleave */        TRUE,
    /* visible_interest */           FALSE,
    /* destroy */                    Destroy,
    /* resize */                     Resize,
    /* expose */                     Redisplay,
    /* set_values */                 SetValues,
    /* set_values_hook */            NULL,
    /* set_values_almost */          XtInheritSetValuesAlmost,
    /* get_values_hook */            NULL,
    /* accept_focus */               NULL,
    /* version */                    XtVersion,
    /* callback_private */           NULL,
    /* tm_table */                   NULL, //defaultTranslations, //XtInheritTranslations,
    /* query_geometry */             QueryGeometry,
    /* display_accelerator */        NULL,
    /* extension */                  NULL,
  },    
  { /* Here is the XmPrimitive class record. */
    /* border_highlight */           XmInheritBorderHighlight,
    /* border_unhighlight */         XmInheritBorderUnhighlight,
    /* translations */               NULL, //XtInheritTranslations,
    /* arm_and_activate */           NULL,
    /* syn_resources */              syn_resources,
    /* num_syn_resources */          XtNumber(syn_resources),
    /* extension */                  (XtPointer)&primClassExtRec,
  },    
  { /* Here is the ExmSeisw class record. */
    /* draw_visual */                DrawVisual,
    /* draw_shadow */                NULL, //DrawShadow, We don't need the frame 
					   // when the seismogram is larger than the visual
    /* create_gc */                  CreateGC,
    /* destroy_gc */                 DestroyGC,
    /* select_gc */                  SelectGC,
    /* calc_visual_size */           CalcVisualSize,
    /* calc_widget_size */           CalcWidgetSize,
    /* reconfigure */                Reconfigure,
    /* extension */                  NULL,
  }    
};    
/* Establish the widget class name as an externally accessible symbol.
   Use the "externaldef" macro rather than the "extern" keyword. */ 
  externaldef( exmseiswwidgetclass) WidgetClass exmSeiswWidgetClass =
                                (WidgetClass) &exmSeiswClassRec;


/* Declare trait record variables. */

/* Declare a trait record variable for the XmQTcontainerItem trait. */ 
//static XmConst XmContainerItemTraitRec seiswCIT = {
//  0,		/* version */
//  ContItemSetValues,
//  ContItemGetValues,
//};

/* Declare a trait record variable for the XmQTcareParentVisual trait. */ 
//static XmConst XmCareVisualTraitRec seiswCVT = {
//    0,		/* version */
//    HandleRedraw,
//};


/* Declare any global static variables. */

/* DEBUG function used to deal with scaling problems */
#ifdef DEBUG_WIDGET
//////////////////////////////////////////////////////////
void showscaling(ExmSeiswWidget sw)
{
if(sw==NULL)
{
cerr << "ExmSeiswPart is currently NULL"<<endl;
}
else
{
cerr << "Scaling parameters in ExmSeiswPart struct"<<endl;
cerr << "x1_resolution = "<< sw->seisw.x1_resolution <<endl;
cerr << "x2_resolution = "<< sw->seisw.x2_resolution <<endl;
cerr << "resolution_set = "<< sw->seisw.resolution_set <<endl;
cerr << "zoom_factor = "<< sw->seisw.zoom_factor <<endl;
cerr << "x_top_position = "<< sw->seisw.x_top_position <<endl;
cerr << "previous_x_top_position = "<< sw->seisw.previous_x_top_position <<endl;
cerr << "y_top_position = "<< sw->seisw.y_top_position <<endl;
cerr << "previous_y_top_position = "<< sw->seisw.previous_y_top_position <<endl;
cerr << "from_zoom = "<< sw->seisw.from_zoom <<endl;
cerr << "zoom_not_set_limit = "<< sw->seisw.zoom_not_set_limit <<endl;
cerr << "xItemCount = "<< sw->seisw.xItemCount <<endl;
cerr << "yItemCount = "<< sw->seisw.yItemCount <<endl;
cerr << "xVisibleItemCount = "<< sw->seisw.xVisibleItemCount <<endl;
cerr << "yVisibleItemCount = "<< sw->seisw.yVisibleItemCount <<endl<<endl;
}
}
void showpar(SeiswPar *p)
{
if(p==NULL)
{
cerr << "SeiswPar is currently NULL"<<endl;
}
else
{
cerr << "SeiswPar parameters:"<<endl;
cerr << "xbox = "<< p->xbox <<endl;
cerr << "ybox = "<< p->ybox <<endl;
cerr << "hbox = "<< p->hbox <<endl;
cerr << "wbox = "<< p->wbox <<endl;
cerr << "x1beg = "<< p->x1beg <<endl;
cerr << "x1end = "<< p->x1end <<endl;
cerr << "x2beg = "<< p->x2beg <<endl;
cerr << "x2end = "<< p->x2end <<endl;
cerr << "x1begb = "<< p->x1begb <<endl;
cerr << "x1endb = "<< p->x1endb <<endl;
cerr << "x2begb = "<< p->x2begb <<endl;
cerr << "x2endb = "<< p->x2endb <<endl;
cerr << "perc = "<< p->perc <<endl;
cerr << "clip_data = "<< p->clip_data <<endl;
cerr << "trace_spacing = "<< p->trace_spacing <<endl;
cerr << "first_trace_offset = "<< p->first_trace_offset <<endl;
cerr << "use_variable_trace_spacing = "<< p->use_variable_trace_spacing <<endl<<endl;
}
}
#endif


void compute_and_set_resolution(ExmSeiswWidget sw, SeiswPar *spar)
{
	double dtemp,resx1,resx2;
	dtemp = (spar->x1endb-spar->x1begb>=0.0)
		? (spar->x1endb-spar->x1begb)
		: (spar->x1begb-spar->x1end);
	
	resx1=dtemp
	 / ((double)(sw->core.width
	   -(sw->primitive.shadow_thickness
	     +sw->primitive.highlight_thickness
	     +sw->seisw.margin_width
	     +spar->xbox)
	   ));
	dtemp=spar->x2endb-spar->x2begb>=0.0 
	     ? spar->x2endb-spar->x2begb
             : spar->x2begb-spar->x2endb;

	resx2=dtemp
	 / ((double)(sw->core.height
	   -(sw->primitive.shadow_thickness
	     +sw->primitive.highlight_thickness
	     +sw->seisw.margin_height
	     +spar->ybox)
	   ));
	sw->seisw.x1_resolution=resx1;
	sw->seisw.x2_resolution=resx2;
	sw->seisw.resolution_set=1;
}
///////////////////////////////////////////////////////////////////////


/* Drag stuff */
static void
SeiswEnterProc(Widget wid,
          XEvent *event,
          String *params,
          Cardinal *num_params)
{
  ExmSeiswWidget sw = (ExmSeiswWidget) wid;

//  if (sw->seisw.seisw_ensemble == NULL || sw->seisw.seisw_metadata==NULL) return;

  if (sw->seisw.DragID)
    {
      XtRemoveTimeOut(sw->seisw.DragID);
      sw->seisw.DragID = 0;
    }


}

static void
SeiswLeaveProc(Widget wid,
          XEvent *event,
          String *params,
          Cardinal *num_params) 
{
  ExmSeiswWidget sw = (ExmSeiswWidget) wid;
  int interval = 200;
  Dimension hsize, vsize; //for scrollbar size, horizontal bar is height, vertical bar is width
		 //this is a temp fix, for some reason, at the first time the "get next
		//event" is clicked and the seismic plot is first ploted, the scrollbar is
		//not counted and if a window leave event is generated, event->xcrossing.x
		//would be still less than sw->core.width, so we need to deduct scrollbar width/height
		//out of it to make the correct judgement.

//  if (sw->seisw.seisw_ensemble == NULL || sw->seisw.seisw_metadata==NULL) return;

  hsize=0; vsize=0;

  if (sw->seisw.hScrollBar != NULL)
	XtVaGetValues((Widget)sw->seisw.hScrollBar, XmNheight, &hsize, NULL);
  if (sw->seisw.vScrollBar != NULL)
	XtVaGetValues((Widget)sw->seisw.vScrollBar, XmNwidth, &vsize, NULL);
  hsize=hsize+sizeof(int);
  vsize=vsize+sizeof(int);


  if (sw->seisw.drag_enable != 1) return;

  sw->seisw.LeaveDir = 0;
  if (event->xcrossing.y >= (int)sw->core.height-hsize
                           ) {      /* Bottom */
      sw->seisw.LeaveDir |= BOTTOMLEAVE;
      sw->seisw.previous_y_top_position = sw->seisw.y_top_position;
  }
  if (event->xcrossing.y <= (int)sw->core.y) {           /* Top */
      sw->seisw.LeaveDir |= TOPLEAVE;
      sw->seisw.previous_y_top_position = sw->seisw.y_top_position;
  }
  if (event->xcrossing.x <= (int)sw->core.x) {         /* Left */
      sw->seisw.LeaveDir |= LEFTLEAVE;
      sw->seisw.previous_x_top_position = sw->seisw.x_top_position;
  }
  if (event->xcrossing.x >= (int)sw->core.width-vsize
                          ) {     /* Right */
      sw->seisw.LeaveDir |= RIGHTLEAVE;
      sw->seisw.previous_x_top_position = sw->seisw.x_top_position;
  }

 
  if ((int)sw->seisw.LeaveDir == 0) {
      sw->seisw.DragID = 0;
      return;
  }

  if (sw->seisw.vScrollBar)
    XtVaGetValues((Widget)sw->seisw.vScrollBar,
                  XmNinitialDelay, &interval, NULL);

  sw->seisw.DragID = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)sw),
                                    (unsigned long) interval,
                                    BrowseScroll, (XtPointer) sw);
}

static void
BrowseScroll(XtPointer closure,
             XtIntervalId *id)
{
  ExmSeiswWidget sw = (ExmSeiswWidget) closure;
  Boolean vLeave = TRUE;
  Boolean hLeave = TRUE;
  int interval = 100;
  int vsize,hsize;

#ifdef DEBUG_WIDGET
cerr << "Entering BrowseScroll"<<endl;
showscaling(sw);
#endif

  //These should be the same formula SetVerticalScrollbar and SetHorizontalScrollbar uses
  //but can't figure out how to use scroll trait to make this work, so use this here for
  //a temperary fix, we can see __SFUpdateScrollBar for a clue on how to do this...
  vsize=MAX(MIN(sw->seisw.yVisibleItemCount, sw->seisw.yItemCount)/10,1);
  hsize=MAX(MIN(sw->seisw.xVisibleItemCount, sw->seisw.xItemCount)/10,1);

  //The following has a special logic, the first round, when x1 reaches the end, first time,
  //hLeave is set to false to cause refresh of screen, but when the second time comes around,
  //hLeave is set to true to prevent further screen flash.
  //i.e., at the case of x_top_position >= xItemCount-xVisibleItemCount

  /* See if the user moved out the top of the list */
  if (sw->seisw.LeaveDir & TOPLEAVE) {
      if ((sw->seisw.y_top_position <= 0) ||
          !(sw->seisw.vScrollBar)) {
          vLeave = TRUE;
        } else {
          sw->seisw.y_top_position=sw->seisw.y_top_position-vsize;
          if (sw->seisw.y_top_position <= 0) sw->seisw.y_top_position=0;
          vLeave = FALSE;
        }
  }

  /* Now see if we went off the end and need to scroll up. */
  if (sw->seisw.LeaveDir & BOTTOMLEAVE) {
      if ((sw->seisw.y_top_position+sw->seisw.yVisibleItemCount >= sw->seisw.yItemCount) ||
          !(sw->seisw.vScrollBar)) {
          vLeave = TRUE;
      } else {
          sw->seisw.y_top_position=sw->seisw.y_top_position+vsize;
	  if (sw->seisw.y_top_position >= sw->seisw.yItemCount-sw->seisw.yVisibleItemCount) 
	    sw->seisw.y_top_position=sw->seisw.yItemCount-sw->seisw.yVisibleItemCount;
          vLeave = FALSE;
      }
  }

  /* Now see if we went off the right and need to scroll left. */
  if (sw->seisw.LeaveDir & LEFTLEAVE) {
      if ((sw->seisw.x_top_position <= 0) ||
          !(sw->seisw.hScrollBar)) {
          hLeave = TRUE;
      } else {
	  sw->seisw.x_top_position=sw->seisw.x_top_position-hsize;
          if (sw->seisw.x_top_position <= 0) sw->seisw.x_top_position=0;
          hLeave = FALSE;
      }
  }

  /* Now see if we went off the left and need to scroll right. */
  if (sw->seisw.LeaveDir & RIGHTLEAVE) {
      if ((sw->seisw.x_top_position+sw->seisw.xVisibleItemCount >= sw->seisw.xItemCount) ||
          !(sw->seisw.hScrollBar)) {
          hLeave = TRUE;
      } else {
	  sw->seisw.x_top_position=sw->seisw.x_top_position+hsize;
	  if (sw->seisw.x_top_position >= sw->seisw.xItemCount-sw->seisw.xVisibleItemCount)
	 	sw->seisw.x_top_position = sw->seisw.xItemCount-sw->seisw.xVisibleItemCount;
          hLeave = FALSE;
      }
  }

  if (vLeave && hLeave)
    return;
  if (!vLeave) {
    update_y_display_range(sw);
    SetVerticalScrollbar(sw);
  }
  if (!hLeave) {
    update_x_display_range(sw);
    SetHorizontalScrollbar(sw);
  }

//  XClearArea(XtDisplay((Widget)sw),static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win,0,0,
//	sw->core.width,sw->core.height,True);
  DrawVisual((Widget)sw);

  if (sw->seisw.vScrollBar)
    XtVaGetValues((Widget)sw->seisw.vScrollBar,
                  XmNrepeatDelay, &interval, NULL);

  XSync (XtDisplay (sw), False);
  sw->seisw.DragID = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)sw),
                                    (unsigned long) interval,
                                    BrowseScroll, (XtPointer) sw);
#ifdef DEBUG_WIDGET
cerr << "Exiting BrowseScroll"<<endl;
showscaling(sw);
#endif
}

static void create_rubberbox_gc(ExmSeiswWidget sw)
{
	SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
	Display * dpy=XtDisplay((Widget)sw);
        XGCValues *values=NULL;
        XStandardColormap scmap;
        int scr=DefaultScreen(dpy);
        unsigned long background;

        /* determine typical background color */
        /* +1 added by John Stockwell 23 Jun 1993 */
        /* to shift xwigb rubberbox from light green to red */
        if (xCreateRGBDefaultMap(dpy,&scmap))
                background = (xGetFirstPixel(dpy)+xGetLastPixel(dpy) + 1)/2;
        else
                background = WhitePixel(dpy,scr);


        /* make graphics context */
        sca->rubberbox_gc = XCreateGC(dpy,sca->win,0,values);
        XSetFunction(dpy,sca->rubberbox_gc,GXxor);
        XSetForeground(dpy,sca->rubberbox_gc,BlackPixel(dpy,scr)^background);

}

static void real_to_screen(float x1, float x2, int *xb, int *yb, ExmSeiswWidget sw)
{
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);

        /* convert (x1,x2) coordinates to mouse location */
        if (spar->style==NORMAL) {
		*xb=SEISPP::nint((x1-spar->x1begb)*(float)(sca->width)/
		    	(spar->x1endb-spar->x1begb)+(float)(sca->x));
		*yb=SEISPP::nint((x2-sca->p2end-spar->x2endb)*(float)(sca->height)/
			(sca->p2beg+spar->x2begb-spar->x2endb-sca->p2end)+(float)(sca->y));
        } else {
		*yb=SEISPP::nint((x1-spar->x1begb)*(float)(sca->height)/
			(spar->x1endb-spar->x1begb)+(float)(sca->y));
		*xb=SEISPP::nint((x2-sca->p2beg-spar->x2begb)*(float)(sca->width)/
			(sca->p2end+spar->x2endb-spar->x2begb-sca->p2beg)+(float)(sca->x));		
        }
  
}

/* translation functions */
//assumption here is that we don't have button 1 and button 3 down at the same time,
//otherwise, we don't know what to do..., the event handlers for these two cases share
//the same data structure?(let's separate them)
static void Btn1DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params)
{
	int xb,yb,wb,hb;
	ExmSeiswWidget sw = (ExmSeiswWidget)w;
	SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
  	SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);  
        int leave;
#ifdef DEBUG_WIDGET
cerr << "Enter Btn1DownProc values"<<endl;
showpar(spar);
#endif

	if (spar==NULL || sca==NULL) return;

	if (event->xmotion.x < sca->x || event->xmotion.x > sca->x + sca->width ||
	     event->xmotion.y < sca->y || event->xmotion.y > sca->y + sca->height) return;

	    //We need to record the start of the button press in case the mouse 
 	    //leaves the window
            SeismicPick pick=MouseLocate(XtDisplay(w),sca->win,
                              *event,spar->style,
                              sca->x,sca->y,sca->width,sca->height,spar->x1begb,spar->x1endb,
                              spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end);
	    sca->x1beg_rb=(float)(pick.get_x1());
	    sca->x2beg_rb=(float)(pick.get_x2());
	    real_to_screen(sca->x1beg_rb,sca->x2beg_rb,&(sca->old_xb),&(sca->old_yb),sw);

	    //enable display of rubber box
	    sw->seisw.rubberbox_enable=1;
	    //we set the going out of window flag (another set is in DrawVisual), so that
	    //we make sure the first drawing is skipped.
	    sca->going_out=1;

	    //enable drag
	    sw->seisw.drag_enable=1;

	    //create rubberbox gc
	    create_rubberbox_gc(sw);
#ifdef DEBUG_WIDGET
cerr << "Leaving Btn1DownProc values"<<endl;
showpar(spar);
#endif

}


static void Btn1UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) 
{
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
	float temp_x1, temp_x2;
	int xstart, ystart, xb, yb, x1,y1,x2,y2,bw,h;
#ifdef DEBUG_WIDGET
cerr << "Entering Btn1UpProc values"<<endl;
showpar(spar);
#endif

	if (spar==NULL || sca==NULL) return;

        if (event->xmotion.x < sca->x || event->xmotion.x > sca->x + sca->width ||
             event->xmotion.y < sca->y || event->xmotion.y > sca->y + sca->height) return;

	sw->seisw.drag_enable=0;

	if (sw->seisw.rubberbox_enable==1) {
	    sw->seisw.rubberbox_enable=0;

            //ok, decide if the original point is outside of the current box
            temp_x1=MIN(MAX(sca->x1beg_rb,spar->x1begb),spar->x1endb);
            temp_x2=MIN(MAX(sca->x2beg_rb,spar->x2begb),spar->x2endb);
            real_to_screen(temp_x1,temp_x2,&xstart,&ystart,sw);

            if (xb==sca->old_xb && yb==sca->old_yb) return;

            //erase old box
            x1=MIN(xstart,sca->old_xb);
            y1=MIN(ystart,sca->old_yb);
            x2=MAX(xstart,sca->old_xb);
            y2=MAX(ystart,sca->old_yb);
            bw=x2-x1;
            h=y2-y1;
            XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,y1,bw,h);

	    //release the GC
	    XFreeGC(XtDisplay(w),sca->rubberbox_gc);

            SeismicPick pick=MouseLocate(XtDisplay(w),sca->win,
                              *event,spar->style,
                              sca->x,sca->y,sca->width,sca->height,spar->x1begb,spar->x1endb,
                              spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end);
            spar->x1begb=MIN(pick.get_x1(),sca->x1beg_rb);
	    spar->x1endb=MAX(pick.get_x1(),sca->x1beg_rb);
	    spar->x2begb=MIN(pick.get_x2(),sca->x2beg_rb);
	    spar->x2endb=MAX(pick.get_x2(),sca->x2beg_rb);

	    //ensure that we don't go over bounds, otherwise, data display will wrap, that is
 	    //not what we want
	    if (spar->x1begb <= spar->x1beg) spar->x1begb = spar->x1beg;
	    if (spar->x1endb >= spar->x1end) spar->x1endb = spar->x1end;
            if (spar->x2begb <= spar->x2beg) spar->x2begb = spar->x2beg;
            if (spar->x2endb >= spar->x2end) spar->x2endb = spar->x2end;
	
	    sw->seisw.zoom_not_set_limit=1;
	    //If we are dealing with a really small box here, reset to the original display size
	    if ((event->xmotion.x-xstart >= -4 && event->xmotion.x-xstart <= 4) ||
		(event->xmotion.y-ystart >= -4 && event->xmotion.y-ystart <= 4)) {
                spar->x1begb = sca->x1begb_init;
                spar->x1endb = sca->x1endb_init;
                spar->x2begb = sca->x2begb_init;
                spar->x2endb = sca->x2endb_init;
		sw->seisw.zoom_not_set_limit=0;
	    }

	    //handle changing resolution for resizing
	    compute_and_set_resolution(sw,spar);

	    //heavy lifting stuff that adjust scrollbars and clear the windows
            int ys_size,xs_size,xcnt,ycnt;
            ComputeVizCount(sw,&xcnt,&ycnt);
            ys_size=MIN(ycnt,sw->seisw.yItemCount);

            //The X direction needs more work.
            sw->seisw.x_top_position=(int)((spar->x1begb-spar->x1beg)/(spar->x1end-spar->x1beg)*
                              (float)(sw->seisw.xItemCount));
            sw->seisw.y_top_position=(int)((float)sw->seisw.yItemCount-((spar->x2begb-spar->x2beg)/
                        (spar->x2end-spar->x2beg)*(float)sw->seisw.yItemCount+
                        (float)ys_size));

            //if sw->seisw.y_top_position accidentally less than 0, then it would generate warnings
            if (sw->seisw.y_top_position<0) sw->seisw.y_top_position=0;
            if (sw->seisw.x_top_position<0) sw->seisw.x_top_position=0;

            SetVerticalScrollbar(sw);
            SetHorizontalScrollbar(sw);

	    //set this so that the scrollbar's SliderMove method realized that it needs to draw visual
	    //instead of ignore this since we do not move scrollbar here
            sw->seisw.from_zoom=1;


            //It seems that we don't need an expose event here?
            //found out why, since after zooming, the scrollbar size changes, and the position
            //changes as well, therefore, there is a slider move event, so SliderMove is called,
            //We still need to clear area here before the drawvisual in SliderMove
            /* clear area and force an expose event */
            XClearArea(XtDisplay(w),sca->win,0,0,sw->core.width,sw->core.height,True);

	}
#ifdef DEBUG_WIDGET
cerr << "Leaving Btn1UpProc values"<<endl;
showpar(spar);
#endif
}

static void Btn1MotionProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params)
{
    ExmSeiswWidget sw=(ExmSeiswWidget)w;
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    float temp_x1, temp_x2;
    int xb, yb, xstart, ystart, x1, y1, x2, y2, bw, h;

    if (spar==NULL || sca==NULL) return;

    if (sw->seisw.rubberbox_enable==1) {

        //ok, decide if the original point is outside of the current box
	temp_x1=MIN(MAX(sca->x1beg_rb,spar->x1begb),spar->x1endb);
	temp_x2=MIN(MAX(sca->x2beg_rb,spar->x2begb),spar->x2endb);
	real_to_screen(temp_x1,temp_x2,&xstart,&ystart,sw);

        xb = event->xmotion.x;
        yb = event->xmotion.y;

	if (xb==sca->old_xb && yb==sca->old_yb) return;
	if (xb > sca->x+sca->width) xb=sca->x+sca->width;
	if (xb < sca->x) xb=sca->x;
	if (yb > sca->y+sca->height) yb=sca->y+sca->height;
	if (yb < sca->y) yb=sca->y;

	//erase old box
	if (sca->going_out != 1) {
	x1=MIN(xstart,sca->old_xb);
	y1=MIN(ystart,sca->old_yb);
	x2=MAX(xstart,sca->old_xb);
	y2=MAX(ystart,sca->old_yb);
	bw=x2-x1;
	h=y2-y1;
	XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,y1,bw,h);
	} else sca->going_out=0;

	//draw current box
	x1=MIN(xstart,xb);
        y1=MIN(ystart,yb);
        x2=MAX(xstart,xb);
        y2=MAX(ystart,yb);
        bw=x2-x1;
        h=y2-y1;
        XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,y1,bw,h);

	//remember old position
	sca->old_xb=xb;
	sca->old_yb=yb;

    }
}

static void Btn3MotionProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params)
{
    ExmSeiswWidget sw=(ExmSeiswWidget)w;
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    float temp_x1, temp_x2;
    int xb, yb, xstart, ystart, x1, y1, x2, y2, bw, h;

    if (sw->seisw.display_only!=0) return;

    if (spar==NULL || sca==NULL) return;

    if (sw->seisw.tw_selection_enable==1) {

        //ok, decide if the original point is outside of the current box
        temp_x1=MIN(MAX(sca->x1beg_rb,spar->x1begb),spar->x1endb);
        temp_x2=MIN(MAX(sca->x2beg_rb,spar->x2begb),spar->x2endb);
        real_to_screen(temp_x1,temp_x2,&xstart,&ystart,sw);

        xb = event->xmotion.x;
        yb = event->xmotion.y;

        if (xb==sca->old_xb) return;
        if (xb > sca->x+sca->width) xb=sca->x+sca->width;
        if (xb < sca->x) xb=sca->x;

        //erase old box
        if (sca->going_out != 1) {
            x1=MIN(xstart,sca->old_xb);
            x2=MAX(xstart,sca->old_xb);
            bw=x2-x1;
            XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,sca->y,bw,sca->height);
        } else sca->going_out=0;

        //draw current box
        x1=MIN(xstart,xb);
        x2=MAX(xstart,xb);
        bw=x2-x1;
        XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,sca->y,bw,sca->height);

        //remember old position
        sca->old_xb=xb;
        sca->old_yb=yb;

    }
}


static void Btn2DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params)
{
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
	SeismicPick * spick=static_cast<SeismicPick *>(sw->seisw.seisw_pick);

//	if (sw->seisw.display_only!=0) return;

	if (spar==NULL || sca==NULL) return;

	if (spick==NULL) {
	    //Peng ALLOC
	    spick=new SeismicPick();
	    sw->seisw.seisw_pick=spick;
	}

        *spick=MouseLocate(XtDisplay(w),sca->win,
                                  *event,spar->style,
                                  sca->x,sca->y,sca->width,sca->height,spar->x1begb,spar->x1endb,
                                  spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end);
        spick->trace_number=get_trace_number(w, spick->get_x2()); 

}

static void Btn2UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) 
{
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
	TimeSeriesEnsemble * sensemble=static_cast<TimeSeriesEnsemble *>(sw->seisw.seisw_ensemble);
        SeismicPick * spick=static_cast<SeismicPick *>(sw->seisw.seisw_pick);
	SeiswCallDataRec call_data;

//	if (sw->seisw.display_only!=0) return;

        int tmp=(int)(sw->seisw.edit_enable);
	if (spar==NULL || sca==NULL) return;
	if (spick==NULL) return;
	if (spick->type != POINT) return;
	
        if (tmp == 1) { //individual trace deletion/restore
	  sensemble->member[spick->trace_number].live=!(sensemble->member[spick->trace_number].live);
	  XClearArea(XtDisplay((Widget)sw),static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win,
		0,0,sw->core.width,sw->core.height,True);
	} else if (tmp == 2) { //cutoff/restore
 	  bool btemp=sensemble->member[spick->trace_number].live;
  	  for(int k=0; k<spick->trace_number+1; k++) 
	      sensemble->member[k].live=!btemp;
	  	
	  XClearArea(XtDisplay((Widget)sw),static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win,
                0,0,sw->core.width,sw->core.height,True);
	}

    	call_data.event = event;
    	call_data.params = params;
    	call_data.num_params = *num_params;

    	XtCallCallbackList(w, sw->seisw.btn2_callback, (XtPointer)&call_data);

}


static void Btn3DownProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params)
{
        int ixb,ixe;
        int wnow;
	TimeWindow temp_tw;
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);

	if (sw->seisw.display_only!=0) return;

	if (spar==NULL || sca==NULL) return;

        //We need to record the start of the button press in case the mouse
        //leaves the window
        SeismicPick pick=MouseLocate(XtDisplay(w),sca->win,
                              *event,spar->style,
                              sca->x,sca->y,sca->width,sca->height,spar->x1begb,spar->x1endb,
                              spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end);
        sca->x1beg_rb=pick.get_x1();
        sca->x2beg_rb=pick.get_x2();
        real_to_screen(sca->x1beg_rb,sca->x2beg_rb,&(sca->old_xb),&(sca->old_yb),sw);

        //enable display of rubber box, not sure if we want to use rubberbox_enable or not,
	//but I feel we have to differentiate these two cases anyhow...
        sw->seisw.tw_selection_enable=1;
        //we set going out here so that the first box erase action in Btn3Motion does
	//not take place, otherwise, we will be left with a line drawn on the screen in the end
        sca->going_out=1;

        //enable drag
        sw->seisw.drag_enable=1;

        //create rubberbox gc
        create_rubberbox_gc(sw);

}

static void Btn3UpProc (
                        Widget w,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) 
{
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
        SeismicPick * spick=static_cast<SeismicPick *>(sw->seisw.seisw_pick);
	TimeWindow temp_tw;
        float temp_x1, temp_x2;
        int xstart, ystart, xb, yb, x1,y1,x2,y2,bw,h;
	SeiswCallDataRec call_data;

	if ((int)sw->seisw.display_only!=0) return;

	if (spar==NULL || sca==NULL) return;

	//must remember to disable drag when mouse btn is released
        sw->seisw.drag_enable=0;

        if (sw->seisw.tw_selection_enable==1) {
            sw->seisw.tw_selection_enable=0;

            //ok, decide if the original point is outside of the current box
            temp_x1=MIN(MAX(sca->x1beg_rb,spar->x1begb),spar->x1endb);
            temp_x2=MIN(MAX(sca->x2beg_rb,spar->x2begb),spar->x2endb);
            real_to_screen(temp_x1,temp_x2,&xstart,&ystart,sw);

            //erase old box
            x1=MIN(xstart,sca->old_xb);
            x2=MAX(xstart,sca->old_xb);
            bw=x2-x1;
            XDrawRectangle(XtDisplay(w),sca->win,sca->rubberbox_gc,x1,sca->y,bw,sca->height);

            //release the GC
            XFreeGC(XtDisplay(w),sca->rubberbox_gc);

            SeismicPick pick=MouseLocate(XtDisplay(w),sca->win,
                              *event,spar->style,
                              sca->x,sca->y,sca->width,sca->height,spar->x1begb,spar->x1endb,
                              spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end);

	    temp_tw.start=MIN(pick.get_x1(),sca->x1beg_rb);
	    temp_tw.end=MAX(pick.get_x1(),sca->x1beg_rb);

            if (spick == NULL) {
             	spick=new SeismicPick(temp_tw);
            } else {
             	SeismicPick * ttw=new SeismicPick(temp_tw);
            	*spick=*ttw;
            	delete ttw;
            }
	    sw->seisw.seisw_pick=spick;

	}

//	XClearArea(XtDisplay((Widget)sw),sca->win,0,0,sw->core.width,sw->core.height,True);
//	DrawVisual((Widget)sw);

        call_data.event = event;
        call_data.params = params;
        call_data.num_params = *num_params;

        XtCallCallbackList(w, sw->seisw.btn3_callback, (XtPointer)&call_data);

        
}


/* This function picks an interval and returns the screen coordinates
of the interval in xleft and xright.  Note that xleft and xright
are screen x in NORMAL style and screen y in SEISMIC style.
Caller must be conscious of this.

The procedure was derived from the xRubberBox function immediately
above.  The main difference is that instead of drawing a zoom box
from click to releas, the box is expanded to the limits of the plot.
All the plotting stuff is identical to xRubberBox except it contains
a conditional for SEISMIC versus NORMAL plot style.

Note there is an error in the comments above about xRubberBox.
This function works with release of any mouse button.  It is
not specific to button 1, and in fact at the moment button3
is used for this function.

Author:  Gary Pavlis and Peng Wang
*/
static void pick_x1_interval(Display *dpy,Window win,XEvent event,
        int style, int x, int y, int width, int height,
        int *xleft, int *xright)

{
        GC gc;
        XGCValues *values=NULL;
        XEvent eventb;
        XStandardColormap scmap;
        int scr=DefaultScreen(dpy);
        int xb,yb,w,h,x1,x2,y1,y2,xorig,yorig,xold,yold;
        unsigned long background;

        /* determine typical background color */
        /* +1 added by John Stockwell 23 Jun 1993 */
        /* to shift xwigb rubberbox from light green to red */
        if (xCreateRGBDefaultMap(dpy,&scmap))
                background = (xGetFirstPixel(dpy)+xGetLastPixel(dpy)+1)/2;
        else
                background = WhitePixel(dpy,scr);


        /* make graphics context */
        gc = XCreateGC(dpy,win,0,values);
        XSetFunction(dpy,gc,GXxor);
        XSetForeground(dpy,gc,BlackPixel(dpy,scr)^background);
        if(style==SEISMIC)
        {

            /* track pointer */
            xorig = x;
            yorig = event.xbutton.y;
            xold = xorig;
            yold = yorig;
            x1 = xorig;
            y1 = yorig;
            w = width;
            h = 0;
            while(h|(~h)/*True*/) {
                XNextEvent(dpy,&eventb);
                if (eventb.type==ButtonRelease) {
                        xb = eventb.xbutton.x;
                        yb = eventb.xbutton.y;
                        break;
                } else if (eventb.type==MotionNotify) {
                        xb = eventb.xmotion.x;
                        yb = eventb.xmotion.y;

                        /* if box is the same, continue */
                        if (xb==xold && yb==yold)
                                continue;

                        /* erase old box */
                        x1 = (xold<xorig)?xold:xorig;
                        y1 = (yold<yorig)?yold:yorig;
                        x2 = (xold>xorig)?xold:xorig;
                        y2 = (yold>yorig)?yold:yorig;
                        w = width;
                        h = y2-y1;
                        XDrawRectangle(dpy,win,gc,x,y1,w,h);

                        /* draw current box */
                        x1 = (xb<xorig)?xb:xorig;
                        y1 = (yb<yorig)?yb:yorig;
                        x2 = (xb>xorig)?xb:xorig;
                        y2 = (yb>yorig)?yb:yorig;
                        w = width;
                        h = y2-y1;
                        XDrawRectangle(dpy,win,gc,x,y1,w,h);

                        /* remember current pointer position */
                        xold = x;
                        yold = yb;
                }
            }
        }
        else
        {
            /* track pointer */
            xorig = event.xbutton.x;
            yorig = y;
            xold = xorig;
            yold = yorig;
            x1 = xorig;
            y1 = yorig;
            w = 0;
            h = height;
            while(h|(~h)/*True*/) {
                XNextEvent(dpy,&eventb);
                if (eventb.type==ButtonRelease) {
                        xb = eventb.xbutton.x;
                        yb = eventb.xbutton.y;
                        break;
                } else if (eventb.type==MotionNotify) {
                        xb = eventb.xmotion.x;
                        yb = eventb.xmotion.y;

                        /* if box is the same, continue */
                        if (xb==xold && yb==yold)
                                continue;
                        /* erase old box */
                        x1 = (xold<xorig)?xold:xorig;
                        y1 = (yold<yorig)?yold:yorig;
                        x2 = (xold>xorig)?xold:xorig;
                        y2 = (yold>yorig)?yold:yorig;
                        w = x2-x1;
                        h = height;
                        XDrawRectangle(dpy,win,gc,x1,y,w,h);

                        /* draw current box */
                        x1 = (xb<xorig)?xb:xorig;
                        y1 = (yb<yorig)?yb:yorig;
                        x2 = (xb>xorig)?xb:xorig;
                        y2 = (yb>yorig)?yb:yorig;
                        w = x2-x1;
                        h = height;
                        XDrawRectangle(dpy,win,gc,x1,y,w,h);

                        /* remember current pointer position */
                        xold = xb;
                        yold = y;
                }
            }
        }
        /* erase rubber box */
        if(style==SEISMIC) {
                XDrawRectangle(dpy,win,gc,x,y1,w,h);
        } else {
                XDrawRectangle(dpy,win,gc,x1,y,w,h);
	}
	//Peng Wang, On avidd, we have to draw this line to erase the last piece of the rectangle.
	XDrawLine(dpy,win,gc,x1,y,x1,y+h);

        /* free graphics context */
        XFreeGC(dpy,gc);

        /* set output parameters */
        if(style==SEISMIC)
        {
                *xleft=y1;
                *xright=y1+h;
        }
        else
        {
                *xleft=x1;
                *xright=x1+w;
        }
}
/*  Formula convert screen coordinates to plot units.
Formula is independent of plot style because in X
the upper left corner of the screen is the origin.
See comment below about offset argument.
Arguments:
        pick - int screen value to convert
        offset - screen coordinate of left (NORMAL) or top (SEISMIC)
                of plot box.
        plot_width - width of plotting box in screen coordinates
        x1begb - x1 value of left (top) of plotting box
        x1endb - x1 value of right (bottom) of plotting box
*/
static double screen_to_time(int pick,
                int offset,
                        int plot_width,
                                float x1begb,
                                        float x1endb)

{
        double time;
        time=static_cast<double>(x1begb)
                + static_cast<double>(x1endb-x1begb)
                * static_cast<double>((pick-offset))
                / static_cast<double>(plot_width);
        return(time);
}


static int get_trace_number(Widget w, float x2raw)
{
        int i;
        int result;
        float dx2min;
        ExmSeiswWidget sw = (ExmSeiswWidget)w;
        SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
        SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);

        if(spar->use_variable_trace_spacing) {
                // In this case search x2 array for closest match
                // to x2raw
                result=0;
                dx2min=static_cast<float>(fabs(static_cast<double>((sca->x2)[0]-x2raw)));

		// March 2007:  left this one trusting nmember is correct
		// HOpe I don't get burned on this
                for(i=1;i<sca->nmember;++i) {
                        float dx2;
                        dx2=static_cast<float>(fabs(static_cast<double>((sca->x2)[i]-x2raw)));
                        if(dx2<dx2min) {
                                result=i;
                                dx2min=dx2;
                        }
                }
        } else {
                // Block for regular spacing.  Simple formula will
                // do
                result=SEISPP::nint(x2raw/sca->d2);
        }
        // Return result in C form, not FORTRAN indexing.
        // Plot uses 1,2, ..., but this will be less confusing in C++
        // silently use edges if outside bounds
        if(result<=0)
                return(0);
        else if(result>=sca->nmember)
                return(sca->nmember-1);
        else
                return(result-1);
}




/* Derived from xMouseLoc above.  Instead of posting the pick value
the result is returned in PointPick structure. */
static SeismicPick MouseLocate(Display *dpy, Window win, XEvent event, int style,
        int x, int y, int width, int height,
        float x1begb, float x1endb, float x2begb, float x2endb,
        float p2beg, float p2end)
{
        float x1,x2;

        /* convert mouse location to (x1,x2) coordinates */
        if (style==NORMAL) {
                x1 = x1begb+(x1endb-x1begb)*(event.xmotion.x-x)/width;
                x2 = p2end+x2endb+(p2beg+x2begb-x2endb-p2end)*
                        (event.xmotion.y-y)/height;
        } else {
                x1 = x1begb+(x1endb-x1begb)*(event.xmotion.y-y)/height;
                x2 = p2beg+x2begb+(p2end+x2endb-x2begb-p2beg)*
                        (event.xmotion.x-x)/width;
        }
        return(SeismicPick(x1,x2));
}


/*********************** self documentation **********************/
/*****************************************************************************
RUBBERBOX -  Function to draw a rubberband box in X-windows plots

xRubberBox      Track pointer with rubberband box

******************************************************************************
Function Prototype:
void xRubberBox (Display *dpy, Window win, XEvent event,
        int *x, int *y, int *width, int *height);

******************************************************************************
Input:
dpy             display pointer
win             window ID
event           event of type ButtonPress

Output:
x               x of upper left hand corner of box in pixels
y               y of upper left hand corner of box in pixels
width           width of box in pixels
height          height of box in pixels

******************************************************************************
Notes:
xRubberBox assumes that event is a ButtonPress event for the 1st button;
i.e., it tracks motion of the pointer while the 1st button is down, and
it sets x, y, w, and h and returns after a ButtonRelease event for the
1st button.

Before calling xRubberBox, both ButtonRelease and Button1Motion events
must be enabled.

This is the same rubberbox.c as in Xtcwp/lib, only difference is
that xRubberBox here is XtcwpRubberBox there, and a shift has been
added to make the rubberbox more visible.

******************************************************************************
Author:         Dave Hale, Colorado School of Mines, 01/27/90
*****************************************************************************/
/**************** end self doc ********************************/


static void
xRubberBox (Display *dpy, Window win, XEvent event,
        int *x, int *y, int *width, int *height, int * leave)
/*****************************************************************************
Track pointer with rubber box
******************************************************************************
Input:
dpy             display pointer
win             window ID
event           event of type ButtonPress

Output:
x               x of upper left hand corner of box in pixels
y               y of upper left hand corner of box in pixels
width           width of box in pixels
height          height of box in pixels
******************************************************************************
Notes:
xRubberBox assumes that event is a ButtonPress event for the 1st button;
i.e., it tracks motion of the pointer while the 1st button is down, and
it sets x, y, w, and h and returns after a ButtonRelease event for the
1st button.

Before calling xRubberBox, both ButtonRelease and Button1Motion events
must be enabled.
******************************************************************************
Author:         Dave Hale, Colorado School of Mines, 01/27/90
*****************************************************************************/
{
        GC gc;
        XGCValues *values=NULL;
        XEvent eventb;
        XStandardColormap scmap;
        int scr=DefaultScreen(dpy);
        int xb,yb,w,h,x1,x2,y1,y2,xorig,yorig,xold,yold;
        unsigned long background;
        
        //not leaving the window
        *leave=0;

        /* determine typical background color */
        /* +1 added by John Stockwell 23 Jun 1993 */
        /* to shift xwigb rubberbox from light green to red */
        if (xCreateRGBDefaultMap(dpy,&scmap))
                background = (xGetFirstPixel(dpy)+xGetLastPixel(dpy) + 1)/2;
        else
                background = WhitePixel(dpy,scr);


        /* make graphics context */
        gc = XCreateGC(dpy,win,0,values);
        XSetFunction(dpy,gc,GXxor);
        XSetForeground(dpy,gc,BlackPixel(dpy,scr)^background);

        /* track pointer */
        xorig = event.xbutton.x;
        yorig = event.xbutton.y;
        xold = xorig;
        yold = yorig;
        x1 = xorig;
        y1 = yorig;
        w = 0;
        h = 0;
        while(h|(~h)/*True*/) {
//                XNextEvent(dpy,&eventb);
		XPeekEvent(dpy,&eventb);
                if (eventb.type==ButtonRelease) {
                        xb = eventb.xbutton.x;
                        yb = eventb.xbutton.y;
			XNextEvent(dpy,&eventb);
                        break;
                } else if (eventb.type==MotionNotify) {

                        xb = eventb.xmotion.x;
                        yb = eventb.xmotion.y;

                        /* if box is the same, continue */
                        if (xb==xold && yb==yold)
                                continue;

                        /* erase old box */
                        x1 = (xold<xorig)?xold:xorig;
                        y1 = (yold<yorig)?yold:yorig;
                        x2 = (xold>xorig)?xold:xorig;
                        y2 = (yold>yorig)?yold:yorig;
                        w = x2-x1;
                        h = y2-y1;
                        XDrawRectangle(dpy,win,gc,x1,y1,w,h);

                        /* draw current box */
                        x1 = (xb<xorig)?xb:xorig;
                        y1 = (yb<yorig)?yb:yorig;
                        x2 = (xb>xorig)?xb:xorig;
                        y2 = (yb>yorig)?yb:yorig;
                        w = x2-x1;
                        h = y2-y1;
                        XDrawRectangle(dpy,win,gc,x1,y1,w,h);

                        /* remember current pointer position */
                        xold = xb;
                        yold = yb;

			XNextEvent(dpy,&eventb);
			
                } else if (eventb.type==LeaveNotify) {
			*leave=1;
			break;
		} else if (eventb.type==EnterNotify) {
			break;
		} 
        }

        /* erase rubber box */
        XDrawRectangle(dpy,win,gc,x1,y1,w,h);

        /* free graphics context */
        XFreeGC(dpy,gc);

        /* set output parameters */
        *x = x1;
        *y = y1;
        *width = w;
        *height = h;
}

static void zoomBox (int x, int y, int w, int h,
        int xb, int yb, int wb, int hb,
        float x1, float x2,
        float y1, float y2,
        float *x1b, float *x2b,
        float *y1b, float *y2b,
        int style)
{
        /* if width and/or height of box are zero, just copy values */
        if (wb==0 || hb==0) {
                *x1b = x1; *x2b = x2;
                *y1b = y1; *y2b = y2;
                return;
        }

        /* clip box */
        if (xb<x) {
                wb -= x-xb;
                xb = x;
        }
        if (yb<y) {
                hb -= y-yb;
                yb = y;
        }
        if (xb+wb>x+w) wb = x-xb+w;
        if (yb+hb>y+h) hb = y-yb+h;

        /* determine box limits */
        if (style == SEISMIC) {
                *x1b = x1+(xb-x)*(x2-x1)/w;
                *x2b = x1+(xb+wb-x)*(x2-x1)/w;
                *y1b = y1+(yb-y)*(y2-y1)/h;
                *y2b = y1+(yb+hb-y)*(y2-y1)/h;
        } else {
                *x2b = x2+(yb-y)*(x1-x2)/h;
                *x1b = x2+(yb+hb-y)*(x1-x2)/h;
                *y1b = y1+(xb-x)*(y2-y1)/w;
                *y2b = y1+(xb+wb-x)*(y2-y1)/w;
        }
}




/* scrolling related */
/* call back routines */
static void update_y_display_range(ExmSeiswWidget sw)
{
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    float ftemp, additional;
/*
    ftemp=spar->x2endb-spar->x2begb > 0 ? spar->x2endb-spar->x2begb : -(spar->x2endb-spar->x2begb);
    additional=ftemp*((float)(sw->seisw.margin_height+spar->ybox))/((float)sw->core.height);
*/
    spar->x2begb=spar->x2beg+
        ((float)((float)sw->seisw.yItemCount-sw->seisw.y_top_position-(float)(sw->seisw.yVisibleItemCount))/((float)sw->seisw.yItemCount))*(spar->x2end-spar->x2beg);
    spar->x2endb=spar->x2begb+(spar->x2end-spar->x2beg)*(float)(sw->seisw.yVisibleItemCount)/((float)sw->seisw.yItemCount);

    if (spar->x2endb >= spar->x2end) spar->x2endb = spar->x2end;
    if (spar->x2begb <= spar->x2beg) spar->x2begb = spar->x2beg;
}

static void update_x_display_range(ExmSeiswWidget sw)
{
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    float ftemp, additional;
/*
    ftemp=spar->x1endb-spar->x1begb > 0 ? spar->x1endb-spar->x1begb : -(spar->x1endb-spar->x1begb);
    additional=ftemp*((float)(sw->seisw.margin_width+spar->xbox))/((float)sw->core.width);
*/
    spar->x1begb=spar->x1beg+
        ((float)(sw->seisw.x_top_position)/((float)sw->seisw.xItemCount))*(spar->x1end-spar->x1beg);
    spar->x1endb=spar->x1begb+(spar->x1end-spar->x1beg)*(float)(sw->seisw.xVisibleItemCount)/((float)sw->seisw.xItemCount);

    if (spar->x1begb <= spar->x1beg) spar->x1begb = spar->x1beg;
    if (spar->x1endb >= spar->x1end) spar->x1endb = spar->x1end;
}

static void
SliderMove(
        Widget w,
        XtPointer closure,
        XtPointer cd )
{
  /* w is a navigator widget */

  ExmSeiswWidget sw = (ExmSeiswWidget) closure;
  XmNavigatorDataRec nav_data;
  SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);

  /* get the navigator information using the trait getValue since I
     cannot use a callback struct */

  nav_data.valueMask = NavValue | NavSliderSize;
  ((XmNavigatorTrait)XmeTraitGet((XtPointer) XtClass(w), XmQTnavigator))
    ->getValue(w, &nav_data);

  /* look at the kind of navigator and make the appropriate update */
  if (nav_data.dimMask & NavigDimensionY) {

    //update the previous positions, this is used to ignore the events where
    //you simply click on the scrollbar the screen would try to refresh, which
    //is a terrible waste.
    sw->seisw.previous_y_top_position=sw->seisw.y_top_position;
    sw->seisw.y_top_position = (int) nav_data.value.y;

    //update the x1begb, x1endb, x2begb, x2endb values so that DrawVisual could draw correctly
    //in both slider move case and zoom case. Note, vertical slider starting position is at top 
    //(slider 0 position), so it is kind of reversed view
    update_y_display_range(sw);
/*
    spar->x2begb=spar->x2beg+
	((float)((float)sw->seisw.yItemCount-sw->seisw.y_top_position-(float)(nav_data.slider_size.y)-1*sw->seisw.zoom_factor)/((float)sw->seisw.yItemCount))*(spar->x2end-spar->x2beg);
    spar->x2endb=spar->x2begb+(spar->x2end-spar->x2beg)*(float)(nav_data.slider_size.y+2*sw->seisw.zoom_factor)/((float)sw->seisw.yItemCount);
*/
  }

  if (nav_data.dimMask & NavigDimensionX) {

    //update the previous positions, this is used to ignore the events where
    //you simply click on the scrollbar the screen would try to refresh, which
    //is a terrible waste.
    sw->seisw.previous_x_top_position=sw->seisw.x_top_position;
    sw->seisw.x_top_position = (int) nav_data.value.x;

    //update the x1begb, x1endb, x2begb, x2endb values so that DrawVisual could draw correctly
    //in both slider move case and zoom case.
    update_x_display_range(sw);
/*
    spar->x1begb=spar->x1beg+
        ((float)(sw->seisw.x_top_position-0-1*sw->seisw.zoom_factor)/((float)sw->seisw.xItemCount))*(spar->x1end-spar->x1beg);
    spar->x1endb=spar->x1begb+(spar->x1end-spar->x1beg)*(float)(nav_data.slider_size.x+2*sw->seisw.zoom_factor)/((float)sw->seisw.xItemCount);
*/
  }

  /* now update the other navigator value */
  _XmSFUpdateNavigatorsValue(XtParent((Widget)sw), &nav_data, False);

  //If from Btn1Down event, then we clear area, redraw visual, otherwise, if the slider has not
  //moved, don't waste time.
  if (sw->seisw.from_zoom != 1 && sw->seisw.previous_x_top_position==sw->seisw.x_top_position &&
        sw->seisw.previous_y_top_position==sw->seisw.y_top_position) return;

//  XClearArea(XtDisplay((Widget)sw),static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win,0,0,sw->core.width,sw->core.height,False);
  DrawVisual((Widget)sw);

  sw->seisw.from_zoom=0;
}


static void
xSizeAxesBox_peng (Display *dpy, Window win,
        char *labelfont, char *titlefont, int style,
        int *x, int *y, int *width, int *height)
/*****************************************************************************
determine optimal origin and size for a labeled axes box
******************************************************************************
Input:
dpy             display pointer
win             window
labelfont       name of font to use for axes labels
titlefont       name of font to use for title
int style       NORMAL (axis 1 on bottom, axis 2 on left)
                SEISMIC (axis 1 on left, axis 2 on top)

Output:
x               x coordinate of upper left corner of box
y               y coordinate of upper left corner of box
width           width of box
height          height of box
******************************************************************************
Notes:
xSizeAxesBox is intended to be used prior to xDrawAxesBox.

An "optimal" axes box is one that more or less fills the window,
with little wasted space around the edges of the window.
******************************************************************************
Author:         Dave Hale, Colorado School of Mines, 01/27/90
*****************************************************************************/
{
        XFontStruct *fa,*ft;
        XWindowAttributes attr;
        int labelch,labelcw,titlech,bl,bt,br,bb;

        /* get fonts and determine character dimensions */
        fa = XLoadQueryFont(dpy,labelfont);
        if (fa==NULL) fa = XLoadQueryFont(dpy,"fixed");
        if (fa==NULL) {
                fprintf(stderr,"Cannot load/query labelfont=%s\n",labelfont);
                exit(-1);
        }
        labelch = fa->max_bounds.ascent+fa->max_bounds.descent;
        labelcw = fa->max_bounds.lbearing+fa->max_bounds.rbearing;
        ft = XLoadQueryFont(dpy,titlefont);
        if (ft==NULL) ft = XLoadQueryFont(dpy,"fixed");
        if (ft==NULL) {
                fprintf(stderr,"Cannot load/query titlefont=%s\n",titlefont);
                exit(-1);
        }
        titlech = ft->max_bounds.ascent+ft->max_bounds.descent;

        /* determine axes box origin and size */
        XGetWindowAttributes(dpy,win,&attr);
        bl = 10*labelcw;
        br = attr.width-5*labelcw;
        while (br<=bl) {
                br += labelcw;
                bl -= labelcw;
        }
        if (bl<0) bl = 0;
        if (br>attr.width) br = attr.width;
        if (style==NORMAL) {
                bt = labelch+labelch/2+titlech;
                bb = attr.height-3*labelch;
        } else {
                bt = 3*labelch;
                bb = attr.height-labelch-labelch/2-titlech;
        }
        while (bb<=bt) {
                bb += labelch;
                bt -= labelch;
        }
        if (bt<0) bt = 0;
        if (bb>attr.height) bb = attr.height;

        *x = bl;
        *y = bt;
        *width = br-bl;
        *height = bb-bt;

        XFreeFont(dpy,fa);
        XFreeFont(dpy,ft);
}


/* Internal function need to rasterize traces.  Loads a float fortran-like matrix of
data from an ensemble.   We pass the matrix to avoid need for exception handling
in creation of z, which is not trivial because it interacts with data concepts.
In particular, one could easily request an absurd plot window that would make the
buffer huge or zero length.

The algorithm used here is most efficient if the window defined by z is less than
or equal to the time span of the data because it hits every sample in z.


args:
        data - ensemble of data
        z - float buffer of size n1*n2
        n1 - first dimension used to build z as n1xn2 fortran-like matrix
        n2 - second dimension
        t0 - start time to use for each column in z.
*/
static void load_z_matrix_peng(vector<TimeSeries> data,
                float *z,
                int n1,
                int n2,
                double t0,
                double dt)
{
        int i,j,iz,id;
	// This was found to happen in some situations and needs to be 
	// avoided.
	if(data.size()!=n2)
	{
		char message[256];
                sprintf(message,"SeismicPlot::load_z_matrix():  "
			" ensemble size = %d does not match number expected"
			" for window = %d\n",data.size(),n2);
		throw SeisppError(string(message));
	}
	// Initialize z to zero.  Without this data on right after endtime
	// can be random garbage.
	for(iz=0;iz<(n1*n2);++iz) z[iz]=0.0;
        for(j=0,iz=0;j<n2;++j)
        {
            double t;
	    // Sanity check requires an exception to be thrown in this case.
	    if (data[j].live &&(data[j].s.size() <= 0)) {
		char message[256];
                sprintf(message,"SeismicPlot::load_z_matrix():  "
                                    "time series index %d is empty\n",j);
                throw SeisppError(string(message));
	    }
            //avoid main loop below if this trace has no data in this time window
            //Test for dead data is redundant for now, but better safe than sorry.
            if( (data[j].endtime() < t0)
                || !data[j].live ) {
                        for(i=0;i<n1;++i,++iz) z[iz]=0.0;
            } else {
                for(i=0,t=t0;i<n1;++i,++iz,t+=dt) { 
                    if(data[j].is_gap(t))
                        z[iz]=0.0;
                    else 
		    {
			// when data range is in plot window copy to z
                        id=data[j].sample_number(t);
                        if( (id>=0) && (id<data[j].ns) ) {
                                z[iz]=static_cast<float>(data[j].s[id]);
                        }
                    }
                 }
             }
        }
}

/*********************************************************/
static XImage *RotImage90_peng(Display *dpy, XImage *oldImage)
{
        int     widthpad,x1,y1,x2,y2,i,nbpr;
        unsigned char   *bits;
        XImage  *image;
        int     width1 =                oldImage->width;
        int     width2 =                oldImage->height;
        int     height2 =       oldImage->width;
        int     scr =                   DefaultScreen(dpy);
	/* SU7 fix*/
	int bitmap_pad=0;
	if(BitmapPad(dpy)>16)
		bitmap_pad=16;
	else if(BitmapPad(dpy)<16)
		bitmap_pad=8;
	widthpad = (1+(width2-1)/bitmap_pad)*bitmap_pad;
	nbpr = widthpad -1;

	/* End SU7 fix */
        bits = static_cast<unsigned char *>(calloc(nbpr*height2,sizeof(unsigned char)));
        if(bits==NULL)
                throw SeisppError("SeismicPlot::RotImage90:  Cannot alloc bitmap");
        image = XCreateImage(   (Display *) dpy,
                                (Visual *) DefaultVisual(dpy,scr),
                                (unsigned int) 1,
                                (int) XYBitmap,
                                (int) 0,
                                (char *) bits,
                                (unsigned int) widthpad,
                                (unsigned int) height2,
                                (int) BitmapPad(dpy),
                                (int) nbpr);

        for (i = 0; i < nbpr*height2; i++)      bits[i]=0;
        for (x2 = 0; x2 < width2; x2++) {
                y1 = x2;
                for (y2 = 0; y2 < height2; y2++) {
                        x1 = width1 - 1 - y2;
                        XPutPixel(image,x2,y2,XGetPixel(oldImage,x1,y1));
                }
        }
        return image;
}


/*********************** self documentation **********************/
/*****************************************************************************
SINC - Return SINC(x) for as floats or as doubles

fsinc           return float value of sinc(x) for x input as a float
dsinc           return double precision sinc(x) for double precision x

******************************************************************************
Function Prototype:
double dsinc (double x);

******************************************************************************
Input:
x               value at which to evaluate sinc(x)

Returned:       sinc(x)

******************************************************************************
Notes:
    sinc(x) = sin(PI*x)/(PI*x)

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/


static double dsinc_peng (double x)
/*****************************************************************************
Return sinc(x) = sin(PI*x)/(PI*x) (double version)
******************************************************************************
Input:
x               value at which to evaluate sinc(x)

Returned:       sinc(x)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
        double pix;

        if (x==0.0) {
                return 1.0;
        } else {
                pix = M_PI*x;
                return sin(pix)/pix;
        }
}
/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
STOEP - Functions to solve a symmetric Toeplitz linear system of equations
         Rf=g for f

stoepd          solve a symmetric Toeplitz system - doubles
stoepf          solve a symmetric Toeplitz system - floats

******************************************************************************
Function Prototypes:
void stoepd (int n, double r[], double g[], double f[], double a[]);

******************************************************************************
Input:
n               dimension of system
r               array[n] of top row of Toeplitz matrix
g               array[n] of right-hand-side column vector

Output:
f               array[n] of solution (left-hand-side) column vector
a               array[n] of solution to Ra=v (Claerbout, FGDP, p. 57)

******************************************************************************
Notes:
These routines do NOT solve the case when the main diagonal is zero, it
just silently returns.

The left column of the Toeplitz matrix is assumed to be equal to the top
row (as specified in r); i.e., the Toeplitz matrix is assumed symmetric.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/
static void stoepd_peng (int n, double r[], double g[], double f[], double a[])
/*****************************************************************************
Solve a symmetric Toeplitz linear system of equations Rf=g for f
(double version)
******************************************************************************
Input:
n               dimension of system
r               array[n] of top row of Toeplitz matrix
g               array[n] of right-hand-side column vector

Output:
f               array[n] of solution (left-hand-side) column vector
a               array[n] of solution to Ra=v (Claerbout, FGDP, p. 57)
******************************************************************************
Notes:
This routine does NOT solve the case when the main diagonal is zero, it
just silently returns.

The left column of the Toeplitz matrix is assumed to be equal to the top
row (as specified in r); i.e., the Toeplitz matrix is assumed symmetric.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
        int i,j;
        double v,e,c,w,bot;

        if (r[0] == 0.0) return;

        a[0] = 1.0;
        v = r[0];
        f[0] = g[0]/r[0];
        for (j=1; j<n; j++) {

                /* solve Ra=v as in Claerbout, FGDP, p. 57 */
                a[j] = 0.0;
                f[j] = 0.0;
                for (i=0,e=0.0; i<j; i++)
                        e += a[i]*r[j-i];
                c = e/v;
                v -= c*e;
                for (i=0; i<=j/2; i++) {
                        bot = a[j-i]-c*a[i];
                        a[i] -= c*a[j-i];
                        a[j-i] = bot;
                }

                /* use a and v above to get f[i], i = 0,1,2,...,j */
                for (i=0,w=0.0; i<j; i++)
                        w += f[i]*r[j-i];
                c = (w-g[j])/v;
                for (i=0; i<=j; i++)
                        f[i] -= c*a[j-i];
        }
}

/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
MKSINC - Compute least-squares optimal sinc interpolation coefficients.

mksinc          Compute least-squares optimal sinc interpolation coefficients.

******************************************************************************
Function Prototype:
void mksinc (float d, int lsinc, float sinc[]);

******************************************************************************
Input:
d               fractional distance to interpolation point; 0.0<=d<=1.0
lsinc           length of sinc approximation; lsinc%2==0 and lsinc<=20

Output:
sinc            array[lsinc] containing interpolation coefficients

******************************************************************************
Notes:
The coefficients are a least-squares-best approximation to the ideal
sinc function for frequencies from zero up to a computed maximum
frequency.  For a given interpolator length, lsinc, mksinc computes
the maximum frequency, fmax (expressed as a fraction of the nyquist
frequency), using the following empirically derived relation (from
a Western Geophysical Technical Memorandum by Ken Larner):

        fmax = min(0.066+0.265*log(lsinc),1.0)

Note that fmax increases as lsinc increases, up to a maximum of 1.0.
Use the coefficients to interpolate a uniformly-sampled function y(i)
as follows:

            lsinc-1
    y(i+d) =  sum  sinc[j]*y(i+j+1-lsinc/2)
              j=0

Interpolation error is greatest for d=0.5, but for frequencies less
than fmax, the error should be less than 1.0 percent.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/


void mksinc_peng (float d, int lsinc, float sinc[])
/*****************************************************************************
Compute least-squares optimal sinc interpolation coefficients.
******************************************************************************
Input:
d               fractional distance to interpolation point; 0.0<=d<=1.0
lsinc           length of sinc approximation; lsinc%2==0 and lsinc<=20

Output:
sinc            array[lsinc] containing interpolation coefficients
******************************************************************************
Notes:
The coefficients are a least-squares-best approximation to the ideal
sinc function for frequencies from zero up to a computed maximum
frequency.  For a given interpolator length, lsinc, mksinc computes
the maximum frequency, fmax (expressed as a fraction of the nyquist
frequency), using the following empirically derived relation (from
a Western Geophysical Technical Memorandum by Ken Larner):

        fmax = min(0.066+0.265*log(lsinc),1.0)

Note that fmax increases as lsinc increases, up to a maximum of 1.0.
Use the coefficients to interpolate a uniformly-sampled function y(i)
as follows:

            lsinc-1
    y(i+d) =  sum  sinc[j]*y(i+j+1-lsinc/2)
              j=0

Interpolation error is greatest for d=0.5, but for frequencies less
than fmax, the error should be less than 1.0 percent.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
        int j;
        double s[20],a[20],c[20],work[20],fmax;

        /* compute auto-correlation and cross-correlation arrays */
        fmax = 0.066+0.265*std::log((double)lsinc);
        fmax = (fmax<1.0)?fmax:1.0;
        for (j=0; j<lsinc; j++) {
                a[j] = dsinc_peng(fmax*j);
                c[j] = dsinc_peng(fmax*(lsinc/2-j-1+d));
        }

        /* solve symmetric Toeplitz system for the sinc approximation */
        stoepd_peng(lsinc,a,c,s,work);
        for (j=0; j<lsinc; j++)
                sinc[j] = s[j];
}
/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
INTTABLE8 -  Interpolation of a uniformly-sampled complex function y(x)
                via a table of 8-coefficient interpolators

intt8r  interpolation of a uniformly-sampled real function y(x) via a
                table of 8-coefficient interpolators

******************************************************************************
Function Prototype:
void intt8r (int ntable, float table[][8],
        int nxin, float dxin, float fxin, float yin[],
        float yinl, float yinr, int nxout, float xout[], float yout[]);

******************************************************************************
Input:
ntable          number of tabulated interpolation operators; ntable>=2
table           array of tabulated 8-point interpolation operators
nxin            number of x values at which y(x) is input
dxin            x sampling interval for input y(x)
fxin            x value of first sample input
yin             array of input y(x) values:  yin[0] = y(fxin), etc.
yinl            value used to extrapolate yin values to left of yin[0]
yinr            value used to extrapolate yin values to right of yin[nxin-1]
nxout           number of x values a which y(x) is output
xout            array of x values at which y(x) is output

Output:
yout            array of output y(x) values:  yout[0] = y(xout[0]), etc.

******************************************************************************
NOTES:
ntable must not be less than 2.

The table of interpolation operators must be as follows:

Let d be the distance, expressed as a fraction of dxin, from a particular
xout value to the sampled location xin just to the left of xout.  Then,
for d = 0.0,

table[0][0:7] = 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0

are the weights applied to the 8 input samples nearest xout.
Likewise, for d = 1.0,

table[ntable-1][0:7] = 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0

are the weights applied to the 8 input samples nearest xout.  In general,
for d = (float)itable/(float)(ntable-1), table[itable][0:7] are the
weights applied to the 8 input samples nearest xout.  If the actual sample
distance d does not exactly equal one of the values for which interpolators
are tabulated, then the interpolator corresponding to the nearest value of
d is used.

Because extrapolation of the input function y(x) is defined by the left
and right values yinl and yinr, the xout values are not restricted to lie
within the range of sample locations defined by nxin, dxin, and fxin.

******************************************************************************
AUTHOR:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

static void intt8r_peng (int ntable, float table[][8],
        int nxin, float dxin, float fxin, float yin[], float yinl, float yinr,
        int nxout, float xout[], float yout[])
{
        int ioutb,nxinm8,ixout,ixoutn,kyin,ktable,itable;
        float xoutb,xoutf,xouts,xoutn,frac,fntablem1,yini,sum,
                *yin0,*table00,*pyin,*ptable;

        /* compute constants */
        ioutb = -3-8;
        xoutf = fxin;
        xouts = 1.0/dxin;
        xoutb = 8.0-xoutf*xouts;
        fntablem1 = (float)(ntable-1);
        nxinm8 = nxin-8;
        yin0 = &yin[0];
        table00 = &table[0][0];

        /* loop over output samples */
        for (ixout=0; ixout<nxout; ixout++) {

                /* determine pointers into table and yin */
                xoutn = xoutb+xout[ixout]*xouts;
                ixoutn = (int)xoutn;
                kyin = ioutb+ixoutn;
                pyin = yin0+kyin;
                frac = xoutn-(float)ixoutn;
		if(frac>=0.0)
			ktable=static_cast<int>(frac*fntablem1+0.5);
		else
			ktable = static_cast<int>((frac+1.0)*fntablem1-0.5);
                ptable = table00+ktable*8;
                /* if totally within input array, use fast method */
                if (kyin>=0 && kyin<=nxinm8) {
                        yout[ixout] =
                                pyin[0]*ptable[0]+
                                pyin[1]*ptable[1]+
                                pyin[2]*ptable[2]+
                                pyin[3]*ptable[3]+
                                pyin[4]*ptable[4]+
                                pyin[5]*ptable[5]+
                                pyin[6]*ptable[6]+
                                pyin[7]*ptable[7];

                /* else handle end effects with care */
                } else {

                        /* sum over 8 tabulated coefficients */
                        for (itable=0,sum=0.0; itable<8; itable++,kyin++) {
                                if (kyin<0)
                                        yini = yinl;
                                else if (kyin>=nxin)
                                        yini = yinr;
                                else
                                        yini = yin[kyin];
                                sum += yini*(*ptable++);
                        }
                        yout[ixout] = sum;
                }
        }
}
/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */




/*********************** self documentation **********************/
/*****************************************************************************
INTSINC8 - Functions to interpolate uniformly-sampled data via 8-coeff. sinc
                approximations:

ints8r  Interpolation of a uniformly-sampled real function y(x) via a
                table of 8-coefficient sinc approximations

******************************************************************************
Function Prototypes:
void ints8r (int nxin, float dxin, float fxin, float yin[],
        float yinl, float yinr, int nxout, float xout[], float yout[]);

******************************************************************************
Input:
nxin            number of x values at which y(x) is input
dxin            x sampling interval for input y(x)
fxin            x value of first sample input
yin             array[nxin] of input y(x) values:  yin[0] = y(fxin), etc.
yinl            value used to extrapolate yin values to left of yin[0]
yinr            value used to extrapolate yin values to right of yin[nxin-1]
nxout           number of x values a which y(x) is output
xout            array[nxout] of x values at which y(x) is output

Output:
yout            array[nxout] of output y(x):  yout[0] = y(xout[0]), etc.

******************************************************************************
Notes:
Because extrapolation of the input function y(x) is defined by the
left and right values yinl and yinr, the xout values are not restricted
to lie within the range of sample locations defined by nxin, dxin, and
fxin.

The maximum error for frequiencies less than 0.6 nyquist is less than
one percent.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/


/* these are used by ints8r */
#define LTABLE 8
#define NTABLE 513

static void ints8r_peng (int nxin, float dxin, float fxin, float yin[],
        float yinl, float yinr, int nxout, float xout[], float yout[])
/*****************************************************************************
Interpolation of a uniformly-sampled real function y(x) via a
table of 8-coefficient sinc approximations; maximum error for frequiencies
less than 0.6 nyquist is less than one percent.
******************************************************************************
Input:
nxin            number of x values at which y(x) is input
dxin            x sampling interval for input y(x)
fxin            x value of first sample input
yin             array[nxin] of input y(x) values:  yin[0] = y(fxin), etc.
yinl            value used to extrapolate yin values to left of yin[0]
yinr            value used to extrapolate yin values to right of yin[nxin-1]
nxout           number of x values a which y(x) is output
xout            array[nxout] of x values at which y(x) is output

Output:
yout            array[nxout] of output y(x):  yout[0] = y(xout[0]), etc.
******************************************************************************
Notes:
Because extrapolation of the input function y(x) is defined by the
left and right values yinl and yinr, the xout values are not restricted
to lie within the range of sample locations defined by nxin, dxin, and
fxin.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/

{
        static float table[NTABLE][LTABLE];
        static int tabled=0;
        int jtable;
        float frac;

        /* tabulate sinc interpolation coefficients if not already tabulated */
        if (!tabled) {
                for (jtable=1; jtable<NTABLE-1; jtable++) {
                        frac = (float)jtable/(float)(NTABLE-1);
                        mksinc_peng(frac,LTABLE,&table[jtable][0]);
                }
                for (jtable=0; jtable<LTABLE; jtable++) {
                        table[0][jtable] = 0.0;
                        table[NTABLE-1][jtable] = 0.0;
                }
                table[0][LTABLE/2-1] = 1.0;
                table[NTABLE-1][LTABLE/2] = 1.0;
                tabled = 1;
        }

        /* interpolate using tabulated coefficients */
        intt8r_peng(NTABLE,table,nxin,dxin,fxin,yin,yinl,yinr,nxout,xout,yout);
}

/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
RFWTVA - Rasterize a Float array as Wiggle-Trace-Variable-Area.

rfwtva  rasterize a float array as wiggle-trace-variable-area.

******************************************************************************
Function Prototype:
void rfwtva (int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian);

******************************************************************************
Input:
n               number of samples in array to rasterize
z               array[n] to rasterize
zmin            z values below zmin will be clipped
zmax            z values above zmax will be clipped
zbase           z values between zbase and zmax will be filled (see notes)
yzmin           horizontal raster coordinate corresponding to zmin
yzmax           horizontal raster coordinate corresponding to zmax
xfirst          vertical raster coordinate of z[0] (see notes)
xlast           vertical raster coordinate of z[n-1] (see notes)
wiggle          =0 for no wiggle (VA only); =1 for wiggle (with VA)
                wiggle 2<=wiggle<=5 for solid/grey coloring of VA option
                shade of grey: wiggle=2 light grey, wiggle=5 black
nbpr            number of bytes per row of bits
bits            pointer to first (top,left) byte in image
endian          byte order  =1 big endian  =0 little endian
Output:
bits            pointer to first (top,left) byte in image

******************************************************************************
Notes:
The raster coordinate of the (top,left) bit in the image is (0,0).
In other words, x increases downward and y increases to the right.
Raster scan lines run from left to right, and from top to bottom.
Therefore, xfirst, xlast, yzmin, and yzmax should not be less than 0.
Likewise, yzmin and yzmax should not be greater than nbpr*8-1, and
care should be taken to ensure that xfirst and xlast do not cause bits
to be set outside (off the bottom) of the image.

Variable area fill is performed on the right-hand (increasing y) side
of the wiggle.  If yzmin is greater than yzmax, then z values between
zmin will be plotted to the right of zmax, and z values between zbase
and zmin are filled.  Swapping yzmin and yzmax is an easy way to
reverse the polarity of a wiggle.

The variable "endian" must have a value of 1 or 0. If this is
not a case an error is returned.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/01/89
MODIFIED:  Paul Michaels, Boise State University, 29 December 2000
           Added solid/grey shading scheme, wiggle>=2 option for peaks/troughs
*****************************************************************************/
/**************** end self doc ********************************/

static void rfwtva_peng (
        int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian)
/*****************************************************************************
Rasterize a float array as wiggle-trace-variable-area.
******************************************************************************
Input:
n               number of samples in array to rasterize
z               array[n] to rasterize
zmin            z values below zmin will be clipped
zmax            z values above zmax will be clipped
zbase           z values between zbase and zmax will be filled (see notes)
yzmin           horizontal raster coordinate corresponding to zmin
yzmax           horizontal raster coordinate corresponding to zmax
xfirst          vertical raster coordinate of z[0] (see notes)
xlast           vertical raster coordinate of z[n-1] (see notes)
wiggle          =0 for no wiggle (VA only); =1 for wiggle (with VA)
                wiggle 2<=wiggle<=5 for solid/grey coloring of VA option
                shade of grey: wiggle=2 light grey, wiggle=5 black
nbpr            number of bytes per row of bits
bits            pointer to first (top,left) byte in image

Output:
bits            pointer to first (top,left) byte in image
******************************************************************************
Notes:
The raster coordinate of the (top,left) bit in the image is (0,0).
In other words, x increases downward and y increases to the right.
Raster scan lines run from left to right, and from top to bottom.
Therefore, xfirst, xlast, yzmin, and yzmax should not be less than 0.
Likewise, yzmin and yzmax should not be greater than nbpr*8-1, and
care should be taken to ensure that xfirst and xlast do not cause bits
to be set outside (off the bottom) of the image.

Variable area fill is performed on the right-hand (increasing y) side
of the wiggle.  If yzmin is greater than yzmax, then z values between
zmin will be plotted to the right of zmax, and z values between zbase
and zmin are filled.  Swapping yzmin and yzmax is an easy way to
reverse the polarity of a wiggle.

The variable "endian" must have a value of 1 or 0. If this is
not a case an error is returned.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/01/89
Modified:  Craig Artley, Colorado School of Mines, 04/14/92
           Fixed bug in computing yoffset.  Previously, when zmin==zmax
           the rasterized trace was shifted to the left by one trace.
MODIFIED:  Paul Michaels, Boise State University, 29 December 2000
           Added solid/grey color scheme, wiggle=2 option for peaks/troughs
*****************************************************************************/
{
        int iscale,xscale,dx,dy,i,x,y,
                ymin,ymax,ybase,ythis,ynext,xthis,xnext,xstep;
        int igrey,ideci;
        float yscale,yoffset,zthis,znext;
        register int bit;
        register unsigned char *byte;

        /* if solid/grey coloring desired      */
        if (wiggle>=2)
        {  igrey=abs(wiggle); wiggle=1; }
        else
        {  igrey=0; }

        /* determine min and max y coordinates */
        ymin = (yzmin<yzmax)?yzmin:yzmax;
        ymax = (yzmax>yzmin)?yzmax:yzmin;

        /* restrict min and max y coordinates */
        ymin = (ymin>0)?ymin:0;
        ymax = (ymax<nbpr*8-1)?ymax:nbpr*8-1;

        /* determine sample index scale factor */
        iscale = n-1;

        /* determine y scale factor and offset */
        yscale = (zmax!=zmin)?(yzmax-yzmin)/(zmax-zmin):1.0;
        yoffset = (zmax!=zmin)?yzmin-zmin*yscale:0.5*(yzmin+yzmax);

        /* determine x scale factor and step */
        xscale = (n>1)?xlast-xfirst:0;
        xstep = (xlast>xfirst)?1:-1;

        /* determine base y coordinate */
        ybase = static_cast<int>(yoffset+zbase*yscale);
        ybase = (ybase>ymin)?ybase:ymin;
        ybase = (ybase<ymax)?ybase:ymax;

        /* initialize next values of x, y, and z */
        znext = *z;
        ynext = static_cast<int>(yoffset+znext*yscale);
        xnext = xfirst;

        /* loop over samples */
        for (i=0; i<n; i++,z++) {

                /* determine x coordinate for this sample */
                xthis = xnext;

                /* determine x coordinate for next sample */
                xnext = (i<iscale)?xfirst+(i+1)*xscale/iscale:xthis+xstep;

                /* skip sample if next sample falls on same x coordinate */
                if (xnext==xthis) continue;

                /* determine difference in x coordinates */
                dx = xnext-xthis;

                /* determine this sample value */
                zthis = znext;

                /* determine next sample value */
                znext = (i<n-1)?*(z+1):zthis;

                /* determine y coordinate for this sample */
                ythis = ynext;

                /* determine y coordinate for next sample */
                ynext = static_cast<int>(yoffset+znext*yscale);

                /* determine difference in y coordinates */
                dy = ynext-ythis;

                /* loop over x coordinates */
                for (x=xthis,y=ythis; x!=xnext;
                        x+=xstep,y=ythis+(x-xthis)*dy/dx) {

                        /* apply clip */
                        if (y<ymin) y = ymin;
                        if (y>ymax) y = ymax;

                        /* determine the bit and byte */
                        /* original: bit = 7-y&7; */
                        bit = (7-y)&7;

                        byte = bits+x*nbpr+(y>>3);

                        /* if wiggle or filling, then set the bit */
                        if (wiggle || y>ybase) {
                                if (endian==0)
                                        *byte |= 1<<(-bit+7);
                                else if (endian==1)
                                        *byte |= 1<<bit;
                                else
                                        fprintf(stderr,"endian must equal either 0 or 1\n");
                        }

                        /* while y greater than base, set more bits (SOLID FILL PEAKS) */
                        while (y>ybase) {
                                y-=1;
                                bit+=1;
                                if (bit>=8) {
                                        byte--;
                                        bit = 0;
                                }
                                if (endian==0)
                                        *byte |= 1<<(-bit+7);
                                else if (endian==1)
                                        *byte |= 1<<bit;
                                else
                                        fprintf(stderr,"endian must equal either 0 or 1\n");
                        }  /* endwhile */

                        /* while y less than base, set more bits (GREY FILL TROUGHS) */
                        if (igrey>0)
                        {
                        ideci=6-igrey;
                        if (ideci<1) ideci=1;

                                while (y<ybase) {
                                        y+=ideci;
                                        bit-=ideci;
                                        if (bit<0) {
                                                byte++;
                                                bit = 7;
                                        }
                                        if (endian==0)
                                                *byte |= 1<<(-bit+7);
                                        else if (endian==1)
                                                *byte |= 1<<bit;
                                        else
                                                fprintf(stderr,"endian must equal either 0 or 1\n");
                                }  /* endwhile  */
                        }  /*  endif igrey   */

                }  /* next x  */
        }   /* next sample  */
}   /* end rfwtva   */
/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
RFWTVAINT - Rasterize a Float array as Wiggle-Trace-Variable-Area, with
            8 point sinc INTerpolation.

rfwtvaint       rasterize a float array as wiggle-trace-variable-area, and
                apply sinc interploation for display purposes.

******************************************************************************
Function Prototype:
void rfwtvaint (int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian);

******************************************************************************
Input:
n               number of samples in array to rasterize
z               array[n] to rasterize
zmin            z values below zmin will be clipped
zmax            z values above zmax will be clipped
zbase           z values between zbase and zmax will be filled (see notes)
yzmin           horizontal raster coordinate corresponding to zmin
yzmax           horizontal raster coordinate corresponding to zmax
xfirst          vertical raster coordinate of z[0] (see notes)
xlast           vertical raster coordinate of z[n-1] (see notes)
wiggle          =0 for no wiggle (VA only); =1 for wiggle (with VA)
                wiggle 2<=wiggle<=5 for solid/grey coloring of VA option
                shade of grey: wiggle=2 light grey, wiggle=5 black
nbpr            number of bytes per row of bits
bits            pointer to first (top,left) byte in image
endian          byte order  =1 big endian  =0 little endian

Output:
bits            pointer to first (top,left) byte in image

******************************************************************************
Notes:
The raster coordinate of the (top,left) bit in the image is (0,0).
In other words, x increases downward and y increases to the right.
Raster scan lines run from left to right, and from top to bottom.
Therefore, xfirst, xlast, yzmin, and yzmax should not be less than 0.
Likewise, yzmin and yzmax should not be greater than nbpr*8-1, and
care should be taken to ensure that xfirst and xlast do not cause bits
to be set outside (off the bottom) of the image.

Variable area fill is performed on the right-hand (increasing y) side
of the wiggle.  If yzmin is greater than yzmax, then z values between
zmin will be plotted to the right of zmax, and z values between zbase
and zmin are filled.  Swapping yzmin and yzmax is an easy way to
reverse the polarity of a wiggle.

The variable "endian" must have a value of 1 or 0. If this is
not a case an error is returned.

The interpolation is by the 8 point sinc interpolation routine s8r.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/01/89
        Memorial University of Newfoundland: Tony Kocurko, Sept 1995.
         Added sinc interpolation.
MODIFIED: Paul Michaels, Boise State University, 29 December 2000
          added solid/grey color scheme for peaks/troughs  wiggle=2 option
*****************************************************************************/
/**************** end self doc ********************************/


void rfwtvaint_peng(
        int n, float z[], float zmin, float zmax, float zbase,
        int yzmin, int yzmax, int xfirst, int xlast,
        int wiggle, int nbpr, unsigned char *bits, int endian)
/*****************************************************************************
Rasterize a float array as wiggle-trace-variable-area.
******************************************************************************
Input:
n               number of samples in array to rasterize
z               array[n] to rasterize
zmin            z values below zmin will be clipped
zmax            z values above zmax will be clipped
zbase           z values between zbase and zmax will be filled (see notes)
yzmin           horizontal raster coordinate corresponding to zmin
yzmax           horizontal raster coordinate corresponding to zmax
xfirst          vertical raster coordinate of z[0] (see notes)
xlast           vertical raster coordinate of z[n-1] (see notes)
wiggle          =0 for no wiggle (VA only); =1 for wiggle (with VA)
                wiggle 2<=wiggle<=5 for solid/grey coloring of VA option
                shade of grey: wiggle=2 light grey, wiggle=5 black
nbpr            number of bytes per row of bits
bits            pointer to first (top,left) byte in image

Output:
bits            pointer to first (top,left) byte in image
******************************************************************************
Notes:
The raster coordinate of the (top,left) bit in the image is (0,0).
In other words, x increases downward and y increases to the right.
Raster scan lines run from left to right, and from top to bottom.
Therefore, xfirst, xlast, yzmin, and yzmax should not be less than 0.
Likewise, yzmin and yzmax should not be greater than nbpr*8-1, and
care should be taken to ensure that xfirst and xlast do not cause bits
to be set outside (off the bottom) of the image.

Variable area fill is performed on the right-hand (increasing y) side
of the wiggle.  If yzmin is greater than yzmax, then z values between
zmin will be plotted to the right of zmax, and z values between zbase
and zmin are filled.  Swapping yzmin and yzmax is an easy way to
reverse the polarity of a wiggle.

The variable "endian" must have a value of 1 or 0. If this is
not a case an error is returned.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/01/89
Modified:  Craig Artley, Colorado School of Mines, 04/14/92
           Fixed bug in computing yoffset.  Previously, when zmin==zmax
           the rasterized trace was shifted to the left by one trace.
MODIFIED: Paul Michaels, Boise State University, 29 December 2000
          added solid/grey color scheme for peaks/troughs  wiggle=2 option
*****************************************************************************/
{
        int i,y,
                ymin,ymax,ybase;
        int igrey,ideci;
        float yscale,yoffset,zthis;
        register int bit;
        register unsigned char *byte;

        static float *xout  , *yout;
        static int    nx = 0;
        float        *yin   ,  fxin , yinl, yinr, dxin, x0, deltax;
        int           nxin  ,  nxout;

        /* if solid/grey coloring desired      */
        if (wiggle>=2)
        {  igrey=abs(wiggle); wiggle=1; }
        else
        {  igrey=0; }

        /* Compute the number of raster scan lines. */
        nxout = ABS(xlast - xfirst + 1);

        /* If the # of scan lines has increased from a previous call,
           allocate more*/
        if ( nxout > nx ) {

          /* If a previous call allocated space for output values,
             free them. */
          if ( nx > 0 ) {
            free (xout);
            free (yout);
          }
          /* Allocate space for the scan line x values and interpolated
             z values. */

                xout = (float *)calloc ((size_t)nxout, sizeof(float));
                yout = (float *)calloc ((size_t)nxout, sizeof(float));
          nx   = nxout;
        }

        nxin   = n       ; /* There are n z-values.  */
        dxin   = 1.0     ; /* We go from index 0 to index n - 1 in steps
                              of 1.0  */
        fxin   = 0.0     ; /* The first index is 0.  */
        yin    = z       ; /* The input array is the z array.  */
        yinl   = z[0]    ; /* Set the values to the left of the array
                              to z[0]. */
        yinr   = z[n - 1]; /* Set the values to the right of the array
                              to z[n-1].*/

        deltax = (float)(n - 1) / (float)(nxout - 1);
        x0     =  0.0;
        if ( xfirst > xlast ) { /* If the z array is to be output backwards, */
                x0 = (float)(n - 1);  /* Then the first output index is n-1, */
                deltax = -deltax;     /*   and we decrement rather than
                                           increment. */
        }
        for (i = 0; i < nxout; i++) /* Load the indices of the output values.*/
                xout[i] = x0 + (float)i * deltax;

        ints8r_peng (nxin, dxin, fxin, yin, yinl, yinr, nxout, xout, yout);

        /* determine min and max y coordinates */
        ymin = (yzmin<yzmax)?yzmin:yzmax;
        ymax = (yzmax>yzmin)?yzmax:yzmin;

        /* restrict min and max y coordinates */
        ymin = (ymin>0)?ymin:0;
        ymax = (ymax<nbpr*8-1)?ymax:nbpr*8-1;

        /* determine y scale factor and offset */
        yscale = (zmax!=zmin)?(yzmax-yzmin)/(zmax-zmin):1.0;
        yoffset = (zmax!=zmin)?yzmin-zmin*yscale:0.5*(yzmin+yzmax);

        /* determine base y coordinate */
        ybase = static_cast<int>(yoffset+zbase*yscale);
        ybase = (ybase>ymin)?ybase:ymin;
        ybase = (ybase<ymax)?ybase:ymax;

        /* loop over scan lines */
        for (i = 0; i < nxout; i++) {
                zthis = yout[i];
                y     = static_cast<int>(yoffset + zthis * yscale);

                /* apply clip */
                if (y < ymin) y = ymin;
                if (y > ymax) y = ymax;

                /* determine the bit and byte */
                /* original: bit = 7-y&7; */
                bit = (7-y)&7;

                /* Tony Kocurko: Had been "bits+x*nbpr+(y>>3)".*/
                byte = bits+i*nbpr+(y>>3);

                /* if wiggle or filling, then set the bit */
                if (wiggle || y>ybase) {
                        if (endian==0)
                                *byte |= 1<<(-bit+7);
                        else if (endian==1)
                                *byte |= 1<<bit;
                        else
                                fprintf(stderr,"endian must equal either 0 or 1\n");
                }


                /* while y greater than base, set more bits (SOLID FILL PEAKS) */
                while (y>ybase) {
                        y-=1;
                        bit+=1;
                        if (bit>=8) {
                                byte--;
                                bit = 0;
                        }
                        if (endian==0)
                                *byte |= 1<<(-bit+7);
                        else if (endian==1)
                                *byte |= 1<<bit;
                        else
                                fprintf(stderr,"endian must equal either 0 or 1\n");
                }  /*  endwhile  */

                /* while y less than base, set more bits (GREY FILL TROUGHS) */
                if (igrey>0)
                {
                ideci=6-igrey;
                if (ideci<1) ideci=1;

                        while (y<ybase) {
                                y+=ideci;
                                bit-=ideci;
                                if (bit<0) {
                                        byte++;
                                        bit = 7;
                                }
                                if (endian==0)
                                        *byte |= 1<<(-bit+7);
                                else if (endian==1)
                                        *byte |= 1<<bit;
                                else
                                        fprintf(stderr,"endian must equal either 0 or 1\n");
                        }  /* endwhile */
                }  /* endif igrey  */

        } /* next scan line  */
}  /*   end rfwtvaint   */


/* return pointer to new image bitmap of rasterized wiggles */
static XImage *newBitmap_peng (Display *dpy, int width, int height,
        int n1, float d1, float f1, int n2, float *x2, float *z,
        float x1beg, float x1end, float x2beg, float x2end,
        float xcur, float clip, int wt, int va,
        float *p2begp, float *p2endp, int endian, int interp,
        int wigclip, int style)
{
        int widthpad,nbpr,i1beg,i1end,if1r,n1r,b1fz,b1lz,i2,i,n2in;
        float x2min,x2max,p2beg,p2end,bscale,boffset,bxcur,bx2;
        unsigned char *bits;
        int scr=DefaultScreen(dpy);
        XImage *image,*image2;
        float   x2margin,clip1,clip2;
        int     bx1max,bx2min,bx2max,b2f,b2l;
        int     width1,height1;
	/* Fix suggested CWP in SU:  here and below calls this SU7 fix.
	SU update says this problem was created by Xorg 7.0 update for 
	security.  */
	int bitmap_pad=0;
	if(BitmapPad(dpy)>16)
		bitmap_pad=16;
	else if(BitmapPad(dpy)<16)
		bitmap_pad=8;


        /* determine bitmap dimensions and allocate space for bitmap */
        width1 =  (style==SEISMIC) ? width : height;
        height1 = (style==SEISMIC) ? height : width;
	/* SU7 fix
        widthpad = (1+(width1-1)/(BitmapPad(dpy)/8))*BitmapPad(dpy)/8;
        nbpr = 1+(widthpad-1)/8;
	*/
	widthpad = (1+(width1-1)/bitmap_pad)*bitmap_pad;
	nbpr = widthpad -1;
        bits = static_cast<unsigned char *>(calloc(nbpr*height1,sizeof(unsigned char)));
        if(bits==NULL)
                throw SeisppError("SeismicPlot::newBitmap:  allocation failure for bitmap array");

        for (i=0; i<nbpr*height1; ++i) bits[i] = 0;

        /* determine number of traces that fall within axis 2 bounds */
        x2min = MIN(x2beg,x2end);
        x2max = MAX(x2beg,x2end);
        for (i2=0,n2in=0; i2<n2; i2++)
                if (x2[i2]>=x2min && x2[i2]<=x2max) n2in++;

        bx2min = 0;
        bx2max = width1 - 1;
        bx1max = height1 - 1;
	/* Handle the case of one trace specially as we just
	fill the box for both regular and irregular spacing */
	if(n2in<=1)
	{
		x2margin=0.5;
		p2beg = -x2margin;
		p2end = x2margin;
		bscale=bx2max/2.0;
		boffset=0;
		bxcur=bscale;
	}
	else
	{
        	//xcur = fabs(xcur)*(x2max-x2min)/(n2in-1);
        	xcur = fabs(xcur)*(x2max-x2min)/(n2in+1);
		if(wigclip)
			//x2margin=(x2max-x2min)/(2*(n2in-1));
			x2margin=xcur/2.0;
		else
			x2margin=xcur;
		if(x2end>=x2beg)
			p2beg=-x2margin;
		else
			p2beg=x2margin;
		p2end=-p2beg;
        	/* determine scale and offset to map x2 units to bitmap units */
        	bscale = bx2max/(x2end+p2end-x2beg-p2beg);
        	boffset = -(x2beg+p2beg)*bscale;
        	bxcur = xcur*bscale;
	}



        /* adjust x1beg and x1end to fall on sampled values */
        i1beg = SEISPP::nint((x1beg-f1)/d1);
        //i1beg = MAX(0,MIN(n1-1,i1beg));  //Peng Wang
        x1beg = f1+i1beg*d1;
        i1end = SEISPP::nint((x1end-f1)/d1);
        //i1end = MAX(0,MIN(n1-1,i1end));  //Peng Wang, when x scrolling, this will cause trouble
        x1end = f1+i1end*d1;

        /* determine first sample and number of samples to rasterize */
        if1r = MIN(i1beg,i1end);
        n1r = MAX(i1beg,i1end)-if1r+1;

        /* determine bits corresponding to first and last samples */
        b1fz = (x1end > x1beg) ? 0 : bx1max;
        b1lz = (x1end > x1beg) ? bx1max : 0;
/*
#ifdef DEBUG_WIDGET
cerr << "Rasterizing "<<n2<<" traces"<<endl;
cerr << "boffset="<<boffset<<" bscale="<<bscale<<endl
<< "Image size = "<<width << " by "  << height<<endl;
cerr << "(b2f,b2l,b1fz,b1lz)"<<endl;
*/

        /* rasterize traces */
        for (i2=0; i2<n2; i2++,z+=n1) {

                /* skip traces not in bounds */
                if (x2[i2]<x2min || x2[i2]>x2max) continue;

                /* determine bitmap coordinate of trace */
                bx2 = boffset+x2[i2]*bscale;
                b2f = (int)(bx2-bxcur);
                b2l = (int)(bx2+bxcur);
                clip1 = -clip;
                clip2 = clip;
                if (b2f < bx2min) {
                        clip1 *= ((bx2-bx2min) / bxcur);
                        b2f = bx2min;
                }
                if (b2l > bx2max) {
                        clip2 *= ((bx2max-bx2) / bxcur);
                        b2l = bx2max;
                }
#ifdef DEBUG_WIDGET
cerr << b2f << " "
	<< b2l << " "
	<< b1fz << " "
	<< b1lz << endl;
#endif



                /* rasterize one trace */
                if (interp==0) { /* don't use interpolation */
                        rfwtva_peng(n1r,&z[if1r],clip1,clip2,va?0:clip2,
                                b2f,b2l,b1fz,b1lz,
                                wt,nbpr,bits,endian);
                } else { /* use 8 point sinc interpolation */
                        rfwtvaint_peng(n1r,&z[if1r],clip1,clip2,va?0:clip2,
                                b2f,b2l,b1fz,b1lz,
                                wt,nbpr,bits,endian);
                }

        }

        /* return axis 2 pads */
        *p2begp = p2beg;  *p2endp = p2end;

        /* get pointer to image */
        image = XCreateImage(   (Display *) dpy,
                                (Visual *) DefaultVisual(dpy,scr),
                                (unsigned int) 1,
                                (int) XYBitmap,
                                (int) 0,
                                (char *) bits,
                                (unsigned int) widthpad,
                                (unsigned int) height1,
				bitmap_pad,
                                (int) nbpr);
	if(image==NULL)
	{
		cerr << "XCreateImage failed in seismic display widget."
			<< "   Fatal error."<<endl;
		exit(-1);
	}

        if (style == NORMAL) {
                image2 = RotImage90_peng(dpy,image);
                XDestroyImage(image);
                image = image2;
        }

        return image;

}

/*********************** self documentation **********************/
/*****************************************************************************
SCAXIS - compute a readable scale for use in plotting axes

scaxis          compute a readable scale for use in plotting axes

******************************************************************************
Function Prototype:
void scaxis (float x1, float x2, int *nxnum, float *dxnum, float *fxnum);

******************************************************************************
Input:
x1              first x value
x2              second x value
nxnum           desired number of numbered values

Output:
nxnum           number of numbered values
dxnum           increment between numbered values (dxnum>0.0)
fxnum           first numbered value

******************************************************************************
Notes:
scaxis attempts to honor the user-specified nxnum.  However, nxnum
will be modified if necessary for readability.  Also, fxnum and nxnum
will be adjusted to compensate for roundoff error; in particular,
fxnum will not be less than xmin-eps, and fxnum+(nxnum-1)*dxnum
will not be greater than xmax+eps, where eps = 0.0001*(xmax-xmin).
xmin is the minimum of x1 and x2.  xmax is the maximum of x1 and x2.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
/**************** end self doc ********************************/



void scaxis_peng (float x1, float x2, int *nxnum, float *dxnum, float *fxnum)
/*****************************************************************************
compute a readable scale for use in plotting axes
******************************************************************************
Input:
x1              first x value
x2              second x value
nxnum           desired number of numbered values

Output:
nxnum           number of numbered values
dxnum           increment between numbered values (dxnum>0.0)
fxnum           first numbered value
******************************************************************************
Notes:
scaxis attempts to honor the user-specified nxnum.  However, nxnum
will be modified if necessary for readability.  Also, fxnum and nxnum
will be adjusted to compensate for roundoff error; in particular,
fxnum will not be less than xmin-eps, and fxnum+(nxnum-1)*dxnum
will not be greater than xmax+eps, where eps = 0.0001*(xmax-xmin).
xmin is the minimum of x1 and x2.  xmax is the maximum of x1 and x2.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
{
        int n,i,iloga;
        float d,f,rdint[4],eps,a,b,xmin,xmax;

        /* set readable intervals */
        rdint[0] = 1.0;  rdint[1] = 2.0;  rdint[2] = 5.0;  rdint[3] = 10.0;

        /* handle x1==x2 as a special case */
        if  (x1==x2) {
                *nxnum = 1;
                *dxnum = 1.0;
                *fxnum = x1;
                return;
        }

        /* determine minimum and maximum x */
        xmin = (x1<x2)?x1:x2;
        xmax = (x1>x2)?x1:x2;

        /* get desired number of numbered values */
        n = *nxnum;
        n = (2>n)?2:n;

        /* determine output parameters, adjusted for roundoff */
        a = (xmax-xmin)/(float)(n-1);
        iloga = (int)log10(a);
        if (a<1.0) iloga = iloga - 1;
        b = a/pow(10.0,(double)iloga);
        for (i=0; i<3 && b>=sqrt(rdint[i]*rdint[i+1]); i++);
        d = rdint[i]*static_cast<float>(pow(10.0,(double)iloga));
        f = ((int)(xmin/d))*d-d;
        eps = 0.0001*(xmax-xmin);
        while(f<(xmin-eps))
                 f = f+d;
        n = 1+(int)((xmax+eps-f)/d);

        /* set output parameters before returning */
        *nxnum = n;
        *dxnum = d;
        *fxnum = f;
}
/* Copyright (c) Colorado School of Mines, 2005.*/
/* All rights reserved.                       */


/*****************************************************************************
Notes:
xDrawAxesBox:
will determine the numbered tic incremenet and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding
axes values.  These pads are useful when the contents of the axes box
requires more space than implied by the axes values.  For example,
the first and last seismic wiggle traces plotted inside an axes box
will typically extend beyond the axes values corresponding to the
first and last traces.  However, all tics will lie within the limits
specified in the axes values (x1beg, x1end, x2beg, x2end).

xSizeAxesBox:
is intended to be used prior to xDrawAxesBox.

An "optimal" axes box is one that more or less fills the window,
with little wasted space around the edges of the window.

******************************************************************************
Author:         Dave Hale, Colorado School of Mines, 01/27/90
*****************************************************************************/
/**************** end self doc ********************************/


static void
xDrawAxesBox_peng (Display *dpy, Window win,
        int x, int y, int width, int height,
        float x1beg, float x1end, float p1beg, float p1end,
        float d1num, float f1num, int n1tic, int grid1, char *label1,
        float x2beg, float x2end, float p2beg, float p2end,
        float d2num, float f2num, int n2tic, int grid2, char *label2,
        char *labelfont, char *title, char *titlefont,
        char *axescolor, char *titlecolor, char *gridcolor,
        int style, int default_margin_width, int default_margin_height, int * origin)
/*****************************************************************************
draw a labeled axes box
******************************************************************************
Input:
dpy             display pointer
win             window
x               x coordinate of upper left corner of box
y               y coordinate of upper left corner of box
width           width of box
height          height of box
x1beg           axis value at beginning of axis 1
x1end           axis value at end of axis 1
p1beg           pad value at beginning of axis 1
p1end           pad value at end of axis 1
d1num           numbered tic increment for axis 1 (0.0 for automatic)
f1num           first numbered tic for axis 1
n1tic           number of tics per numbered tic for axis 1
grid1           grid code for axis 1:  NONE, DOT, DASH, or SOLID
label1          label for axis 1
x2beg           axis value at beginning of axis 2
x2end           axis value at end of axis 2
p2beg           pad value at beginning of axis 2
p2end           pad value at end of axis 2
d2num           numbered tic increment for axis 2 (0.0 for automatic)
f2num           first numbered tic for axis 2
n2tic           number of tics per numbered tic for axis 2
grid2           grid code for axis 2:  NONE, DOT, DASH, or SOLID
label2          label for axis 2
labelfont       name of font to use for axes labels
title           axes box title
titlefont       name of font to use for title
axescolor       name of color to use for axes
titlecolor      name of color to use for title
gridcolor       name of color to use for grid
int style       NORMAL (axis 1 on bottom, axis 2 on left)
                SEISMIC (axis 1 on left, axis 2 on top)
******************************************************************************
Notes:
xDrawAxesBox will determine the numbered tic incremenet and first
numbered tic automatically, if the specified increment is zero.

Pad values must be specified in the same units as the corresponding
axes values.  These pads are useful when the contents of the axes box
requires more space than implied by the axes values.  For example,
the first and last seismic wiggle traces plotted inside an axes box
will typically extend beyond the axes values corresponding to the
first and last traces.  However, all tics will lie within the limits
specified in the axes values (x1beg, x1end, x2beg, x2end).
******************************************************************************
Author:         Dave Hale, Colorado School of Mines, 01/27/90
*****************************************************************************/
{
        GC gca,gct,gcg;
        XGCValues *values=NULL;
        XColor scolor,ecolor;
        XFontStruct *fa,*ft;
        XWindowAttributes wa;
        Colormap cmap;
        int labelca,labelcd,labelch,labelcw,titleca,
                ntic,xa,ya,tw,ticsize,ticb,numb,labelb,lstr,grided,grid,
                n1num,n2num;
        float dnum,fnum,dtic,amin,amax,base,scale,anum,atic,azero;
        char str[256],dash[2],*label;
        /* create graphics contexts */
        gca = XCreateGC(dpy,win,0,values);
        gct = XCreateGC(dpy,win,0,values);
        gcg = XCreateGC(dpy,win,0,values);


        /* get and set fonts and determine character dimensions */
        fa = XLoadQueryFont(dpy,labelfont);
        if (fa==NULL) fa = XLoadQueryFont(dpy,"fixed");
        if (fa==NULL) {
                fprintf(stderr,"Cannot load/query labelfont=%s\n",labelfont);
                exit(-1);
        }
        XSetFont(dpy,gca,fa->fid);
        labelca = fa->max_bounds.ascent;
        labelcd = fa->max_bounds.descent;
        labelch = fa->max_bounds.ascent+fa->max_bounds.descent;
        labelcw = fa->max_bounds.lbearing+fa->max_bounds.rbearing;
        ft = XLoadQueryFont(dpy,titlefont);
        if (ft==NULL) ft = XLoadQueryFont(dpy,"fixed");
        if (ft==NULL) {
                fprintf(stderr,"Cannot load/query titlefont=%s\n",titlefont);
                exit(-1);
        }
        XSetFont(dpy,gct,ft->fid);
        titleca = ft->max_bounds.ascent;

        /* determine window's current colormap */
        XGetWindowAttributes(dpy,win,&wa);
        cmap = wa.colormap;
        /* get and set colors */
        if (XAllocNamedColor(dpy,cmap,axescolor,&scolor,&ecolor))
                XSetForeground(dpy,gca,ecolor.pixel);
        else
                XSetForeground(dpy,gca,1L);
        if (XAllocNamedColor(dpy,cmap,titlecolor,&scolor,&ecolor))
                XSetForeground(dpy,gct,ecolor.pixel);
        else
                XSetForeground(dpy,gct,1L);
        if (XAllocNamedColor(dpy,cmap,gridcolor,&scolor,&ecolor))
                XSetForeground(dpy,gcg,ecolor.pixel);
        else
                XSetForeground(dpy,gcg,1L);

        /* determine tic size */
        ticsize = labelcw;

        /* determine numbered tic intervals */
        if (d1num==0.0) {
                n1num = (style==NORMAL ? width : height)/(8*labelcw);
                scaxis_peng(x1beg,x1end,&n1num,&d1num,&f1num);
        }
        if (d2num==0.0) {
                n2num = (style==NORMAL ? height : width)/(8*labelcw);
                scaxis_peng(x2beg,x2end,&n2num,&d2num,&f2num);
        }

        /* draw horizontal axis */
        if (style==NORMAL) {
                amin = (x1beg<x1end)?x1beg:x1end;
                amax = (x1beg>x1end)?x1beg:x1end;
                dnum = d1num;  fnum = f1num;  ntic = n1tic;
                scale = width/(x1end+p1end-x1beg-p1beg);
                base = x-scale*(x1beg+p1beg);
                ya = y+height;
                ticb = ticsize;
                numb = ticb+labelca;
                labelb = numb+labelch;
                grid = grid1;
                label = label1;
        } else {
                amin = (x2beg<x2end)?x2beg:x2end;
                amax = (x2beg>x2end)?x2beg:x2end;
                dnum = d2num;  fnum = f2num;  ntic = n2tic;
                scale = width/(x2end+p2end-x2beg-p2beg);
                base = x-scale*(x2beg+p2beg);
                ya = y;
                ticb = -ticsize;
                numb = ticb-labelcd;
                labelb = numb-labelch;
                grid = grid2;
                label = label2;
        }
        if (grid==SOLID)
                grided = True;
        else if (grid==DASH) {
                grided = True;
                XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
                dash[0] = 8;  dash[1] = 4;
                XSetDashes(dpy,gcg,0,dash,2);
        } else if (grid==DOT) {
                grided = True;
                XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
                dash[0] = 1;  dash[1] = 4;
                XSetDashes(dpy,gcg,0,dash,2);
        } else
                grided = False;
        azero = 0.0001*(amax-amin);

	//Peng Wang, this is to clear area around the axis so that we don't clear the whole area
	//30--default margin_width 20-default margin height
	XClearArea(dpy,win,x-default_margin_width,y+height,width+default_margin_width+ticb,
		ticb+default_margin_height,False);
//	XClearArea(dpy,win,x-ticb-default_margin_width,y,ticb+default_margin_width,height-ticb,False);

        for (anum=fnum; anum<=amax; anum+=dnum) {
                if (anum<amin) continue;
                xa = static_cast<int>(base+scale*anum);
                if (grided) XDrawLine(dpy,win,gcg,xa,y,xa,y+height);
                XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb);
                if (anum>-azero && anum<azero)
                        sprintf(str,"%1.5g",0.0);
                else
                        sprintf(str,"%1.5g",anum);
                lstr = (int) strlen(str);
                tw = XTextWidth(fa,str,lstr);
                XDrawString(dpy,win,gca,xa-tw/2,ya+numb,str,lstr);
        }
        dtic = dnum/ntic;
        for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
                if (atic<amin) continue;
                xa = static_cast<int>(base+scale*atic);
                XDrawLine(dpy,win,gca,xa,ya,xa,ya+ticb/2);
        }
        lstr = (int) strlen(label);
        tw = XTextWidth(fa,label,lstr);

	//Put it here since we need to clear the axis legends as well
        //Well, why do we want default_margin_width here?
	XClearArea(dpy,win,x-ticb-default_margin_width-tw,y,ticb+default_margin_width+tw,height-ticb,False);

        XDrawString(dpy,win,gca,x+width-tw,ya+labelb,label,lstr);

        /* draw vertical axis */
        if (style==NORMAL) {
                amin = (x2beg<x2end)?x2beg:x2end;
                amax = (x2beg>x2end)?x2beg:x2end;
                dnum = d2num;  fnum = f2num;  ntic = n2tic;
                scale = -height/(x2end+p2end-x2beg-p2beg);
                base = y+height-scale*(x2beg+p2beg);
                grid = grid2;
                label = label2;
        } else {
                amin = (x1beg<x1end)?x1beg:x1end;
                amax = (x1beg>x1end)?x1beg:x1end;
                dnum = d1num;  fnum = f1num;  ntic = n1tic;
                scale = height/(x1end+p1end-x1beg-p1beg);
                base = y-scale*(x1beg+p1beg);
                grid = grid1;
                label = label1;
        }
        xa = x;
        ticb = -ticsize;
        numb = ticb-ticsize/4;
        if (grid==SOLID)
                grided = True;
        else if (grid==DASH) {
                grided = True;
                XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
                dash[0] = 8;  dash[1] = 4;
                XSetDashes(dpy,gcg,0,dash,2);
        } else if (grid==DOT) {
                grided = True;
                XSetLineAttributes(dpy,gcg,1L,LineOnOffDash,CapButt,JoinMiter);
                dash[0] = 1;  dash[1] = 4;
                XSetDashes(dpy,gcg,0,dash,2);
        } else
                grided = False;
        azero = 0.0001*(amax-amin);
        for (anum=fnum; anum<=amax; anum+=dnum) {
                if (anum<amin) continue;
                ya = static_cast<int>(base+scale*anum);
                if (grided) XDrawLine(dpy,win,gcg,x,ya,x+width,ya);
                XDrawLine(dpy,win,gca,xa,ya,xa+ticb,ya);
                if (anum>-azero && anum<azero)
                        sprintf(str,"%1.5g",0.0);
                else
                        sprintf(str,"%1.5g",anum);
                lstr = (int) strlen(str);
                tw = XTextWidth(fa,str,lstr);
                XDrawString(dpy,win,gca,xa+numb-tw,ya+labelca/sizeof(int),str,lstr);
        }

	if (origin != NULL) {
	        for(anum=amin; anum <= amax; anum++) {
		    origin[(int)(anum-amin)]
			 =static_cast<int>(base+scale*anum-labelch/2);
		}
	}

        dtic = dnum/ntic;
        for (atic=fnum-ntic*dtic-dtic; atic<=amax; atic+=dtic) {
                if (atic<amin) continue;
                ya = static_cast<int>(base+scale*atic);
                XDrawLine(dpy,win,gca,xa,ya,xa+ticb/2,ya);
        }
        lstr = (int) strlen(label);
        if (style==NORMAL)
                XDrawString(dpy,win,gca,
                        x+ticb-9*labelcw,
                        y+labelca/sizeof(int)-labelch,label,lstr);
        else
                XDrawString(dpy,win,gca,
                        x+ticb-9*labelcw,
                        y+height+labelca/sizeof(int)+labelch,label,lstr);

        /* draw title */
        lstr = (int) strlen(title);
        tw = XTextWidth(ft,title,lstr);
        if (style==NORMAL)
                XDrawString(dpy,win,gct,
                        x+width/2-tw/2,
			y+labelca/sizeof(int)-labelch,title,lstr);
//                        y+labelca/4-labelch-labelch,title,lstr);
        else
                XDrawString(dpy,win,gct,
                        x+width/2-tw/2,
                        y+height+labelca/sizeof(int)+labelch+titleca,title,lstr);

        /* draw axes box */
        XDrawRectangle(dpy,win,gca,x,y,width,height);

        /* free resources before returning */
        XFreeGC(dpy,gca);
        XFreeGC(dpy,gct);
        XFreeGC(dpy,gcg);
        XFreeFont(dpy,fa);
        XFreeFont(dpy,ft);
}




/******************************************************************************
 *
 *  ClassInitialize:
 *     Called the first time a widget of this class is instantiated. 
 *
 *****************************************************************************/
static void 
ClassInitialize( void )
{
 /* The ExmNseiswShape resource requires a value of data type ExmRSeiswShape.
    Since this is not a standard Motif data type, we must create this
    data type just for the ExmSeisw widget.
    We want ExmRSeiswShape to hold an enumerated value, either 
    XmSIMPLE_OVAL or XmSIMPLE_RECTANGLE.  The best way to accomplish
    this is to register the new data type with representation type facility. */ 

}



/******************************************************************************
 *
 *  ClassPartInitialize:
 *      Called when this widget or a subclass of this widget is instantiated.
 *
 *****************************************************************************/
static void 
ClassPartInitialize (
        WidgetClass widgetClass
                    )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)widgetClass;
 ExmSeiswWidgetClass sc = (ExmSeiswWidgetClass) wc->core_class.superclass;
 char *xlats;

 xlats = (char *)
    XtMalloc(strlen(defaultTranslations) + 1);
 strcpy(xlats,defaultTranslations);
 wc->core_class.tm_table =(String) XtParseTranslationTable(xlats);
 XtFree((char *)xlats);

 /* The following code allows subclasses of ExmSeisw to inherit certain 
    methods of ExmSeisw. */ 
   if (wc->seisw_class.draw_visual == ExmInheritDrawVisual)
       wc->seisw_class.draw_visual = sc->seisw_class.draw_visual;
   if (wc->seisw_class.draw_shadow == ExmInheritDrawShadow)
       wc->seisw_class.draw_shadow = sc->seisw_class.draw_shadow;
   if (wc->seisw_class.create_gc == ExmInheritCreateGC)
       wc->seisw_class.create_gc = sc->seisw_class.create_gc;
   if (wc->seisw_class.destroy_gc == ExmInheritDestroyGC)
       wc->seisw_class.destroy_gc = sc->seisw_class.destroy_gc;
   if (wc->seisw_class.select_gc == ExmInheritSelectGC)
       wc->seisw_class.select_gc = sc->seisw_class.select_gc;
   if (wc->seisw_class.calc_visual_size == ExmInheritCalcVisualSize)
       wc->seisw_class.calc_visual_size = sc->seisw_class.calc_visual_size;
   if (wc->seisw_class.calc_widget_size == ExmInheritCalcWidgetSize)
       wc->seisw_class.calc_widget_size = sc->seisw_class.calc_widget_size;
   if (wc->seisw_class.reconfigure == ExmInheritReconfigure)
       wc->seisw_class.reconfigure = sc->seisw_class.reconfigure;

}


int is_resolution_set(Widget w)
{
    ExmSeiswWidget sw = (ExmSeiswWidget)w;
    return sw->seisw.resolution_set;
}


//This is for the redisplay after a SetValues either reset SeiswEnsemble or Metadata

static void ReInitialize(
        Widget request_w,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
{
	ExmSeiswWidget rw = (ExmSeiswWidget)request_w;
	ExmSeiswWidget nw = (ExmSeiswWidget)new_w;

	int i, j, n;
	/* These are convenient shorthands for these two private members of the widget */
	SeiswCA *ca=static_cast<SeiswCA *>(nw->seisw.seisw_ca);
	SeiswPar *para=static_cast<SeiswPar *>(nw->seisw.seisw_parameters);
#ifdef DEBUG_WIDGET
cerr << "Entering ReInitialize values"<<endl;
showpar(para);
#endif

	/* This is a minor inefficiency.  Could test this against a static pointer
	but the overhead is not worth the headache */
	if (nw->seisw.seisw_metadata != NULL)
	{
 		para->SetParameters(static_cast<Metadata *>(nw->seisw.seisw_metadata));
	}
	TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(nw->seisw.seisw_ensemble);
	int nmember;
	ca->nmember=nmember=tse->member.size();
	if(nmember<=0) 
		throw SeisppError("no data to plot\n");
	(ca->curvecolor).clear();
	for(i=0;i<nmember;++i) (ca->curvecolor).push_back(para->default_curve_color);
	if((ca->x2)==NULL)
		ca->x2=new float[nmember];
	else
	{
		delete [] ca->x2;
		ca->x2=new float[nmember];
	}
	if (para->use_variable_trace_spacing) {
            try {
                for(i=0;i<nmember;++i)
                    (ca->x2)[i]=static_cast<double>(tse->member[i].get_double(para->trace_axis_attribute));
            } catch (MetadataGetError mde) {
                    mde.log_error();
                    cerr << "Reverting to equal space tracing" << endl;
                    for(i=0;i<nmember;++i) (ca->x2)[i]=static_cast<float>(i+1);
            }
	}
	else
	{
                 for(i=0;i<nmember;++i) (ca->x2)[i]=static_cast<float>(i+1);
	}
	

	SetBox((Widget)nw);

	para->x1begb=para->x1beg;
	para->x1endb=para->x1end;
	para->x2begb=para->x2beg;
	para->x2endb=para->x2end;
	/* Sanity check.  Could cause mysterious behaviour, but 
	better than aborting.  Hard coding these defaults is not idea. */
	if((para->x1endb)<=(para->x1begb) || (para->x2endb<=para->x2begb) )
	{
		if((para->x1endb)<=(para->x1begb))
		{
			para->x1begb=0.0;
			para->x1endb=60.0;
		}
		else
		{
			para->x2begb=0.0;
			para->x2endb=24.0;
		}
		compute_and_set_resolution(nw,para);
	}
	/* Special case for one trace in the window */
	if(nmember<=1)
	{
		para->x2begb=0.5;
		para->x2endb=1.5;
		compute_and_set_resolution(nw,para);
	}
	/* Since this is an initialization we force these
	to always match here */
	para->x1beg=para->x1begb;
	para->x1end=para->x1endb;
	para->x2beg=para->x2begb;
	para->x2end=para->x2endb;
	/* These parameters set scale that will exist when returned
	with a click with mb1. */
	ca->x1begb_init=para->x1begb;
	ca->x1endb_init=para->x1endb;
	ca->x2begb_init=para->x2begb;
	ca->x2endb_init=para->x2endb;
	/* Always reset resolution here even if it was set before.
	Necessary because we can call ReInitialize from a previously set resolution
	state that becomes stale. */
	compute_and_set_resolution(nw,para);
	double x1limit=(para->x1end-para->x1beg >= 0.0 
			? para->x1end-para->x1beg 
			: para->x1beg-para->x1end);
	double x2limit=(para->x2end-para->x2beg >= 0.0 
			? para->x2end-para->x2beg 
			: para->x2beg-para->x2end);
	
	nw->seisw.xItemCount=(int)(x1limit*nw->seisw.zoom_factor);
	nw->seisw.yItemCount=(int)(x2limit*nw->seisw.zoom_factor);
	/* These are set in Btn1UpProc which set scale correctly.
	assume we need this here too. */
        int ys_size,xcnt,ycnt;
        ComputeVizCount(nw,&xcnt,&ycnt);
        ys_size=MIN(ycnt,nw->seisw.yItemCount);

        /*The X direction needs more work.  */
	/* This was the old code.  It seems unnecessarily complicated for a initialize
	where these variables should always end up  0 */
	/***********************************
        nw->seisw.x_top_position=(int)((para->x1begb-para->x1beg)/(para->x1end-para->x1beg)*
                          (float)(nw->seisw.xItemCount));
        nw->seisw.y_top_position=(int)((float)nw->seisw.yItemCount-((para->x2begb-para->x2beg)/
                    (para->x2end-para->x2beg)*(float)nw->seisw.yItemCount+
                    (float)ys_size));

        if (nw->seisw.y_top_position<0) nw->seisw.y_top_position=0;
        if (nw->seisw.x_top_position<0) nw->seisw.x_top_position=0;
	*/
	nw->seisw.x_top_position=0;
	nw->seisw.y_top_position=0;
	

      	SetVerticalScrollbar(nw);
      	SetHorizontalScrollbar(nw);
	DisplayAttributes tda;
	if (nw->seisw.display_attributes == NULL) 
	{
		tda=new DisplayAttributesRec;
		nw->seisw.display_attributes=static_cast<XtPointer>(tda);
	}
	else
		tda=static_cast<DisplayAttributes>
				(nw->seisw.display_attributes);

	tda->str_origin=new int[(int)(para->x2end-para->x2beg+1)];
	tda->x2begb=para->x2beg;
	tda->x2endb=para->x2end;

#ifdef DEBUG_WIDGET
cerr << "Leaving ReInitialize values"<<endl;
showpar(para);
#endif
}


/******************************************************************************
 *
 *  Initialize:
 *     Called by the Intrinsics when this widget is instantiated. 
 *
 *****************************************************************************/

static void 
Initialize(
        Widget request_w,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(new_w);
 ExmSeiswWidget rw = (ExmSeiswWidget)request_w;
 ExmSeiswWidget nw = (ExmSeiswWidget)new_w;

 int i, j, n;
 

 XmScrollFrameTrait scrollFrameTrait;
  XmImRegister(new_w, 0);
/* This initializes both of these to defaults.  Commonly changed
 later, but the user should assume all attributes of both of these
 classes are initialized.  Note other methods should only change
 SeiswPar through the SetParameters(Metadata *) method. */
 SeiswPar *para=new SeiswPar();
 SeiswCA *ca=new SeiswCA();
 if(para==NULL || ca==NULL)
	throw SeisppError("Initialize:  Control structure creation failed");
 nw->seisw.seisw_parameters=para;
 nw->seisw.seisw_ca=ca; 
 /* Earlier versions called SetParameters if seisw_metadata was set, but 
 I have intentionally NOT done that here to prevent this initialization 
 confusion.  ReInitialize DOES doe this. */
 nw->seisw.seisw_metadata=NULL;
 /* same for pick pointer*/
 nw->seisw.seisw_pick=NULL;
 /* I'm not sure what this one was supposed to do, but we'll initialize it NULL
 It is a dangerous pointer that is later deleted if not NULL */
 nw->seisw.cleanup_data=NULL;
 /* This initializes the display markers data structure.*/
 nw->seisw.display_markers=new DisplayMarkerDataRec;
 /* These should eventually be be variable through the X resources or from SeiswPar.
 For now we fix them as these colors in this initialization */
 DisplayMarkerData dm=static_cast<DisplayMarkerData>(nw->seisw.display_markers);
 dm->beam_color=string("red");
 dm->robust_color=string("blue");
 dm->title=string("SeismicPlot");
/* We always initialize the data pointer to a NULL at REQUIRE the caller to manage
 * the memory held by data.  */
 nw->seisw.seisw_ensemble=NULL;
 
 /* Call a function that creates the GC's needed by the widget. */ 
   if (wc->seisw_class.create_gc) 
     (*(wc->seisw_class.create_gc))((Widget)nw);
 /* This initializes the messy negotiation for size.  We initially 
 make this something assuming it will be set properly in ReInitialize*/
 nw->seisw.need_to_compute_width = True;
 nw->seisw.need_to_compute_height=True;
 nw->seisw.default_width=para->wbox;
 nw->seisw.default_height=para->hbox;
 nw->seisw.pref_width=para->wbox;
 nw->seisw.pref_height=para->hbox;
 nw->seisw.x1_resolution=100.0;
 nw->seisw.x2_resolution=100.0;
 nw->seisw.resolution_set=0;
 /* mark us as not selected to start with */
 nw->seisw.saved_foreground = XmUNSPECIFIED_PIXEL ;
 /* These are X resources so we set them this way. */
 n=0;
 Arg loc_args[5];
 XtSetArg(loc_args[n], XmNwidth, nw->seisw.default_width);n++;
 XtSetArg(loc_args[n], XmNheight, nw->seisw.default_height);n++;
 XtSetValues(new_w->core.parent,loc_args,n);

 DisplayAttributes da=static_cast<DisplayAttributes>(nw->seisw.display_attributes);
 da=new DisplayAttributesRec;
 da->str_origin=NULL;

 nw->seisw.x_top_position=0;
 nw->seisw.y_top_position=0;
 nw->seisw.previous_x_top_position=0;
 nw->seisw.previous_y_top_position=0;
 nw->seisw.from_zoom=0;
 nw->seisw.zoom_not_set_limit=1;

   //Drag stuff
   nw->seisw.drag_enable = 0;
   nw->seisw.DragID=0;
   nw->seisw.rubberbox_enable=0;
   nw->seisw.tw_selection_enable=0;

   nw->seisw.ScrollBarDisplayPolicy=XmSTATIC;

  double x1limit=(para->x1end-para->x1beg >= 0.0 ? para->x1end-para->x1beg : para->x1beg-para->x1end);
   double x2limit=(para->x2end-para->x2beg >= 0.0 ? para->x2end-para->x2beg : para->x2beg-para->x2end);

  nw->seisw.xItemCount=(int)(x1limit*nw->seisw.zoom_factor);
  nw->seisw.yItemCount=(int)(x2limit*nw->seisw.zoom_factor);
  //depending on the size of the widget, we need to modify x1begb, x1endb, x2begb, and 
  //x2endb to only show the plot within the widget visual
  nw->core.width=nw->seisw.default_width;
  nw->core.height=nw->seisw.default_height;

  if (nw->core.width >= 
	(nw->primitive.shadow_thickness+nw->primitive.highlight_thickness)
	  +(nw->seisw.margin_width+para->xbox) 
		&& nw->core.height >= (
		  nw->primitive.shadow_thickness+nw->primitive.highlight_thickness)
		  +(nw->seisw.margin_height+para->ybox)) {
	    if (nw->core.width+nw->seisw.margin_width+para->xbox < para->wbox) {
		para->x1begb=para->x1beg;
		para->x1endb=para->x1begb+x1limit*
			(float)(nw->core.width-(
			nw->primitive.shadow_thickness+nw->primitive.highlight_thickness)-
			(nw->seisw.margin_width+para->xbox))/
			(float)(para->wbox);
	    if (para->x1endb > para->x1end) para->x1endb=para->x1end;
	//	if (para->x1begb < para->x1beg) para->x1begb=para->x1beg;
	    } else {

		para->x1begb=para->x1beg;
		para->x1endb=para->x1end;

	    }
	    if (nw->core.height < para->hbox) {

                para->x2endb=para->x2end;
		para->x2begb=para->x2endb-x2limit*
                        (float)(nw->core.height-(
                        nw->primitive.shadow_thickness+nw->primitive.highlight_thickness)
			-(nw->seisw.margin_height+para->ybox))/
			(float)(para->hbox);
 		if (para->x2begb < para->x2beg) para->x2begb=para->x2beg;
	    } else {
		para->x2endb=para->x2end;
		para->x2begb=para->x2beg;
	    }


	} 

	SeiswCA * sca=static_cast<SeiswCA *>(nw->seisw.seisw_ca);
	sca->x1begb_init=para->x1begb;
        sca->x1endb_init=para->x1endb;
        sca->x2begb_init=para->x2begb;
        sca->x2endb_init=para->x2endb;



    //check if the scroll trait has been inited or not, since it is possible that
    //we are going to call Initialized from SetValues as well, in there, we don't
    //want to recreate the scrollbar again...
    Cardinal num_nav_list ;
    Widget * nav_list ;
    Boolean inited ;

    inited =  ((XmScrollFrameTrait)
               XmeTraitGet((XtPointer) XtClass(nw->core.parent), XmQTscrollFrame))
        ->getInfo(nw->core.parent, NULL, &nav_list, &num_nav_list);


   if (!inited) {


  /* set up the scroll frame trait */
  scrollFrameTrait = (XmScrollFrameTrait)
  	XmeTraitGet((XtPointer) XtClass(nw->core.parent), XmQTscrollFrame);
  if (scrollFrameTrait == NULL ||
      scrollFrameTrait->getInfo (nw->core.parent, NULL, NULL, NULL))
    {
      nw->seisw.Mom = NULL;
      return;
    }

  /*
   * Set up the default move callback so that our navigator gets
   * associated nicely by the scrollFrame.
   */
  scrollFrameTrait->init (nw->core.parent, SliderMove, (Widget)nw);
  nw->seisw.Mom = (XmScrolledWindowWidget) nw->core.parent;


    /**********************
     * vertical scrollbar */
    Arg vSBArgs[11];

    i = 0;
    XtSetArg (vSBArgs[i], XmNorientation, XmVERTICAL), i++;
    XtSetArg (vSBArgs[i], XmNunitType, XmPIXELS), i++;
    XtSetArg (vSBArgs[i], XmNshadowThickness, 0);
//              nw->primitive.shadow_thickness), i++;
    XtSetArg (vSBArgs[i], XmNhighlightThickness, 0), i++;
    XtSetArg(vSBArgs[i], XmNtraversalOn, FALSE), i++;

    nw->seisw.vScrollBar = (XmScrollBarWidget)
      XmCreateScrollBar((Widget) nw->seisw.Mom, (char*)(string("VertScrollBar").c_str()), vSBArgs, i);
     
    SetVerticalScrollbar(nw);

    /************************
     * horizontal scrollbar */
    Arg hSBArgs[11];

    j = 0;

    XtSetArg (hSBArgs[j], XmNorientation, XmHORIZONTAL), j++;
    XtSetArg (hSBArgs[j], XmNunitType, XmPIXELS), j++;
    XtSetArg (hSBArgs[j], XmNshadowThickness, 0);
//             nw->primitive.shadow_thickness), j++;
    XtSetArg (hSBArgs[j], XmNhighlightThickness, 0), j++;
    XtSetArg(hSBArgs[j], XmNtraversalOn, FALSE), j++;
//    XtSetArg(hSBArgs[i], XmNprocessingDirection, XmMAX_ON_RIGHT), i++;

    nw->seisw.hScrollBar = (XmScrollBarWidget)
        XmCreateScrollBar((Widget)nw->seisw.Mom, (char*)(string("HorScrollBar").c_str()), hSBArgs,j);

    SetHorizontalScrollbar(nw);

    }

 /* In order not to resize the widget at each level of the Initialize
    chain, the actual class is passed to this method */
   if (wc->seisw_class.reconfigure) 
     (*(wc->seisw_class.reconfigure))(exmSeiswWidgetClass, new_w, NULL);

#ifdef DEBUG_WIDGET
cerr << "Leaving Initialize values"<<endl;
showpar(para);
#endif
}

/* Computes the number of pixels in the current zoom box.  

Algorithm uses range in x1 and x2 data converting to pixels using 
the seisw variable xItemCount and yItemCount.  Basic algorithm is
to get this count as fraction (endb-begb)/(end-beg) where b 
denotes current zoom box.  Note this returns only the size of
the box in pixels.  It's position is determined elsewhere.

Arguments:
	nw - Seisw widget making this request.
	xnct and yncnt are the returned counts in x1 and x2
		directions respectively.
*/
static void 
ComputeVizCount(ExmSeiswWidget nw, int * xcnt, int * ycnt)
{
    int sheight, hborder;
    int swidth, wborder;
    float per_item_height, per_item_width;
    SeiswPar * spar=static_cast<SeiswPar *>(nw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(nw->seisw.seisw_ca);

    try {
	    if (spar == NULL || sca==NULL) throw SeisppError("invalid common area or parameters");

	    float x1_additional, x2_additional, f2temp, f1temp, f2limit, f1limit;
  
	    f2temp=spar->x2endb-spar->x2begb >= 0.0 ? spar->x2endb-spar->x2begb : spar->x2begb-spar->x2endb;
	    f1temp=spar->x1endb-spar->x1begb >= 0.0 ? spar->x1endb-spar->x1begb : spar->x1begb-spar->x1endb;
	    f2limit=spar->x2end-spar->x2beg >= 0.0 ? spar->x2end-spar->x2beg : spar->x2beg-spar->x2end;
            f1limit=spar->x1end-spar->x1beg >= 0.0 ? spar->x1end-spar->x1beg : spar->x1beg-spar->x1end;
/*
	    x1_additional=((float)(nw->seisw.margin_width+spar->xbox))*f1temp
		/((float)(nw->core.width));
            x2_additional=((float)(nw->seisw.margin_height+spar->ybox))*f2temp
                /((float)(nw->core.height));
*/

	    *ycnt=SEISPP::nint((float)((f2temp)/(float)(f2limit))*(float)nw->seisw.yItemCount);
            if (*ycnt > nw->seisw.yItemCount) *ycnt=nw->seisw.yItemCount;
	    if (*ycnt <= 0) *ycnt=1;

	    *xcnt=SEISPP::nint((float)((f1temp)/(float)(f1limit))*(float)nw->seisw.xItemCount);
	    if (*xcnt > nw->seisw.xItemCount) *xcnt=nw->seisw.xItemCount;
	    if (*xcnt <= 0) *xcnt=1;


    } catch (...) {
	throw SeisppError( "error computing vis count");
    }    
}

static void
_XmSFUpdateNavigatorsValue(
        Widget sf,
        XmNavigatorData nav_data,
        Boolean notify)
{
    Cardinal i, num_nav_list ;
    Widget * nav_list ;
    Boolean inited ;

    /* there is a possibility that the SW was not inited for
       navigation business: APP_DEFINED where no scrollbar have
       been added yet */
    inited =  ((XmScrollFrameTrait)
               XmeTraitGet((XtPointer) XtClass(sf), XmQTscrollFrame))
        ->getInfo(sf, NULL, &nav_list, &num_nav_list);

    if (!inited) return ;

    /* loop over the associated navigator list and call the change value
       method for each navigator */
    /* Updating the first navigator only if notify is True is not
       enough, since the dimension is pertinent */

    for (i=0; i < num_nav_list; i++) {
        Widget nav = nav_list[i] ;
        XmNavigatorSetValueProc nav_setValue =
            ((XmNavigatorTrait)
             XmeTraitGet((XtPointer) XtClass(nav), XmQTnavigator))->setValue;

        nav_setValue(nav, nav_data, notify);
    }
}


/************************************************************************
 *                                                                      *
 * SetVerticalScrollbar - set up all the vertical scrollbar stuff.      *
 *                                                                      *
 * Set up on an item basis. Min is 0, max is ItemCount, origin is       *
 * top_position, extent is visibleItemCount.                            *
 *                                                                      *
 ************************************************************************/

static Boolean
SetVerticalScrollbar(ExmSeiswWidget nw)
{
  int vizx, vizy;
  XmNavigatorDataRec nav_data;
  Boolean was_managed, is_managed;
#ifdef DEBUG_WIDGET
cerr << "Top of SetVerticalScrollbar"<<endl;
showscaling(nw);
#endif


  if ((!nw->seisw.Mom) ||
      (!nw->seisw.vScrollBar)) return true; 

  ComputeVizCount(nw, &vizx, &vizy);

  was_managed = XtIsManaged((Widget) nw->seisw.vScrollBar);
  /* Turn off the scrollbar if it isn't needed for this display.  Defined
  her when the requested visual area can fit inside the whole area and
  the "top_position" variable is zero */
  if (nw->seisw.ScrollBarDisplayPolicy == XmAS_NEEDED)
    {
      if (((nw->seisw.yItemCount <= vizy) && (nw->seisw.y_top_position == 0)) ||
          (nw->seisw.yItemCount == 0))
        XtUnmanageChild((Widget) nw->seisw.vScrollBar);
      else
        XtManageChild((Widget) nw->seisw.vScrollBar);
    }
  else
    XtManageChild((Widget) nw->seisw.vScrollBar);
  is_managed = XtIsManaged((Widget) nw->seisw.vScrollBar);


  if (nw->seisw.yItemCount) {
      int vmax = nw->seisw.yItemCount;
      int vOrigin = nw->seisw.y_top_position; 
      int vExtent = MIN(vizy, nw->seisw.yItemCount);

      /* CR 8889: Size slider based on visible item count. */
      ASSIGN_MAX(vmax, vExtent + vOrigin);

      nw->seisw.yVisibleItemCount=vExtent;
	
      nav_data.value.y = vOrigin;
      nav_data.minimum.y = 0;
      nav_data.maximum.y = vmax;
      nav_data.slider_size.y = vExtent;
      nav_data.increment.y = MAX(vExtent/10,1); //1*nw->seisw.zoom_factor;
      nav_data.page_increment.y = ((vizy > 1) ?
                                   (vizy - 1)  : 1);

      nav_data.dimMask = NavigDimensionY;
      nav_data.valueMask = (NavValue | NavMinimum | NavMaximum |
                            NavSliderSize | NavIncrement | NavPageIncrement);
      _XmSFUpdateNavigatorsValue(XtParent((Widget)nw), &nav_data, True);
    }
    else if (XtIsManaged((Widget) nw->seisw.vScrollBar)) {
      nav_data.value.y = 0;
      nav_data.minimum.y = 0;
      nav_data.maximum.y = 1;
      nav_data.slider_size.y = 1;
      nav_data.increment.y = 1;
      nav_data.page_increment.y = 1;

      nav_data.dimMask = NavigDimensionY;
      nav_data.valueMask = (NavValue | NavMinimum | NavMaximum |
                            NavSliderSize | NavIncrement | NavPageIncrement);
      _XmSFUpdateNavigatorsValue(XtParent((Widget)nw), &nav_data, True);
    }
#ifdef DEBUG_WIDGET
cerr << "Exiting SetVerticalScrollbar"<<endl;
showscaling(nw);
#endif

    return (was_managed != is_managed);

}


static Boolean
SetHorizontalScrollbar(ExmSeiswWidget nw)
{
  int vizx, vizy;
  XmNavigatorDataRec nav_data;
  Boolean was_managed, is_managed;

  if ((!nw->seisw.Mom) ||
      (!nw->seisw.hScrollBar)) return true;

  ComputeVizCount(nw, &vizx, &vizy);

  was_managed = XtIsManaged((Widget) nw->seisw.hScrollBar);
  if (nw->seisw.ScrollBarDisplayPolicy == XmAS_NEEDED)
    {
      if (((nw->seisw.xItemCount <= vizx) && (nw->seisw.x_top_position == 0)) ||
          (nw->seisw.xItemCount == 0))
        XtUnmanageChild((Widget) nw->seisw.hScrollBar);
      else
        XtManageChild((Widget) nw->seisw.hScrollBar);
    }
  else
    XtManageChild((Widget) nw->seisw.hScrollBar);
  is_managed = XtIsManaged((Widget) nw->seisw.hScrollBar);


  if (nw->seisw.xItemCount) {
      int hmax = nw->seisw.xItemCount;
      int hOrigin = nw->seisw.x_top_position;
      int hExtent = MIN(vizx, nw->seisw.xItemCount);

      /* CR 8889: Size slider based on visible item count. */
      ASSIGN_MAX(hmax, hExtent + hOrigin);

      nw->seisw.xVisibleItemCount=hExtent;

      nav_data.value.x = hOrigin;
      nav_data.minimum.x = 0;
      nav_data.maximum.x = hmax;
      nav_data.slider_size.x = hExtent;
      nav_data.increment.x = MAX(hExtent/10,1); //1*nw->seisw.zoom_factor;
      nav_data.page_increment.x = ((vizx > 1) ?
                                   vizx - 1  : 1);

      nav_data.dimMask = NavigDimensionX;
      nav_data.valueMask = (NavValue | NavMinimum | NavMaximum |
                            NavSliderSize | NavIncrement | NavPageIncrement);
      _XmSFUpdateNavigatorsValue(XtParent((Widget)nw), &nav_data, True);
    }
    else if (XtIsManaged((Widget) nw->seisw.hScrollBar)) {
      nav_data.value.x = 0;
      nav_data.minimum.x = 0;
      nav_data.maximum.x = 1;
      nav_data.slider_size.x = 1;
      nav_data.increment.x = 1;
      nav_data.page_increment.x = 1;

      nav_data.dimMask = NavigDimensionX;
      nav_data.valueMask = (NavValue | NavMinimum | NavMaximum |
                            NavSliderSize | NavIncrement | NavPageIncrement);
      _XmSFUpdateNavigatorsValue(XtParent((Widget)nw), &nav_data, True);
    }

    return (was_managed != is_managed);

}



/******************************************************************************
 *
 *  Destroy: 
 *      Called by the Intrinsics whenever this widget is deallocated. 
 *
 *****************************************************************************/
static void
Destroy (Widget w)
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass) XtClass(w);
 ExmSeiswWidget sw=(ExmSeiswWidget)w;

 /* The ExmSeisw widget allocates two internal GC's.  In order to prevent 
    memory leaks, we must destroy these GC's.  */ 
   if (wc->seisw_class.destroy_gc) 
     (*(wc->seisw_class.destroy_gc))(w);

   if (sw->seisw.seisw_parameters != NULL) 
	delete static_cast<SeiswPar*>(sw->seisw.seisw_parameters);
   if (sw->seisw.seisw_ca != NULL)
	delete static_cast<SeiswCA *>(sw->seisw.seisw_ca);
   if (sw->seisw.seisw_pick != NULL) 
	delete static_cast<SeismicPick *>(sw->seisw.seisw_pick);
 
  /****
  * This was commented out because it seemed to do nothing.  Peng Wang
  * added this resource to this widget and I (glp) do not know for sure what
  * it was to be used for.  I am guessing it was a stash for assorted 
  * debris cast to an opaque pointer that Peng discovered was not really
  * needed and failed to remove it. In this current code this line does
  * nothing beause it is set to NULL in Initialize and nothing else
  * touches it.  It is generating a compiler error so I'm commenting it
  * out.  If some poor soul is tempted to set this resource and load
  * data here it will definitely create a memory leak. 
  */
   //if (sw->seisw.cleanup_data != NULL) delete sw->seisw.cleanup_data;

   if (sw->seisw.display_attributes != NULL) {
	int * ipointer=(static_cast<DisplayAttributes>(sw->seisw.display_attributes))->str_origin;
	if (ipointer != NULL) delete ipointer;
	delete static_cast<DisplayAttributes *>(sw->seisw.display_attributes);
   }
}

/*****************************************************************************
 * 
 * Realize
 *      Called by the Intrinsics to create the window for the widget.  This
 *      class's realize method creates a propagating window for this 
 *      exact class,  but uses the default window otherwise 
 *
 *****************************************************************************/
static void 
Realize(Widget w,
        XtValueMask *p_valueMask,
        XSetWindowAttributes *attributes )
{
   Mask valueMask = *p_valueMask;
   Window win, win1;
   Display * dpy;
   ExmSeiswWidget sw = (ExmSeiswWidget)w;
   int scr;
   unsigned long black, white;
   SeiswPar * spar;

   if (sw->seisw.seisw_ca==NULL)
        sw->seisw.seisw_ca=(XtPointer)(new SeiswCA);

   /* First call Primitive's method */
   xmPrimitiveClassRec.core_class.realize(w, p_valueMask, attributes);

   dpy=XtDisplay(w);
   scr = DefaultScreen(dpy);
   black = BlackPixel(dpy,scr);
   white = WhitePixel(dpy,scr);

   // Simple window launch with SU procedure
   //right now there is a temperary fix that we don't put spar->xbox and spar->ybox in,
   //because it looks weird with the slider.
/*
   win=xNewWindow_peng(dpy,XtWindow(w),1,1,
	MAX(spar->wbox, sw->seisw.default_width),MAX(spar->hbox, sw->seisw.default_height),
       (int) black,(int) white,spar->windowtitle);
*/
   /* We can't create a new window like before, otherwise we won't be able to do our
      mouse binding on the new window anymore */
     XtCreateWindow(w,(unsigned int) InputOutput, (Visual *) CopyFromParent,0,NULL);
     win=XtWindow(w);

   /* set endian for display.  Original a bit more complex and involved
      an apparent default passed as a parameter.  This allowed bypass of
      BitmapBitOrder call below.*/
   if(BitmapBitOrder(dpy)==LSBFirst)
        static_cast<SeiswCA *>(sw->seisw.seisw_ca)->endian=0;
   else if(BitmapBitOrder(dpy)==MSBFirst)
        static_cast<SeiswCA *>(sw->seisw.seisw_ca)->endian=1;

   /* set normal event mask, this MUST be set to make mouse bindings work */
   XSelectInput(dpy,win,
	 EnterWindowMask |
	 LeaveWindowMask |
         StructureNotifyMask |
         ExposureMask |
         KeyPressMask |
         PointerMotionMask |
         ButtonPressMask |
         ButtonReleaseMask |
         Button1MotionMask |
         Button2MotionMask |
         Button3MotionMask);

   //This has to be done, otherwise, the window won't be viewable.
   XMapWindow(dpy,win);


   /* ExmSeisw wants to propagate all unused events to its hierarchy, which
      Primitive.Realize doesn't do.  So if this is an ExmSeisw widget, 
      we fix the do_not_propagate window attribute */

   if (XtClass(w) == exmSeiswWidgetClass) {
     Mask adjustMask;
     XSetWindowAttributes xswa;

     adjustMask = CWDontPropagate;
     xswa.do_not_propagate_mask = NoEventMask;

     XChangeWindowAttributes(XtDisplay(w), win, adjustMask, &xswa);
   }


   XClearWindow(dpy,win);

   static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win=win;   

}

/******************************************************************************
 *
 *  Resize: 
 *      Called by the Intrinsics whenever a parent resizes a child.
 *      Also called by the Reconfigure method. 
 *
 *****************************************************************************/
static void 
Resize (
        Widget w
       ) 
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(w);
 ExmSeiswWidget sw = (ExmSeiswWidget)w;
 Dimension  mw, mh;
 Dimension  window_decoration_thickness;
 Dimension  total_target_widget_width, total_target_widget_height;

 SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
 SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);

 if (spar==NULL) return;

/*
   if (wc->seisw_class.calc_visual_size)
     (*(wc->seisw_class.calc_visual_size))((Widget)sw);
*/
   
    if (!XtIsRealized((Widget)sw)) return;    

            if (sw->core.width < spar->wbox) {

		spar->x1endb=spar->x1begb+sw->seisw.x1_resolution*(float)(sw->core.width-
		  sw->primitive.shadow_thickness-sw->primitive.highlight_thickness+(sw->seisw.margin_width+spar->xbox));
		
		if (spar->x1endb > spar->x1end) spar->x1endb=spar->x1end;

            } else {
		spar->x1endb=spar->x1end;
	    }
            if (sw->core.height < spar->hbox) {

		spar->x2begb=spar->x2endb-sw->seisw.x2_resolution*(float)(sw->core.height-
		    sw->primitive.shadow_thickness-sw->primitive.highlight_thickness+(sw->seisw.margin_height+spar->ybox));
		if (spar->x2begb < spar->x2beg) spar->x2begb=spar->x2beg;

            } else {
		spar->x2begb=spar->x2beg;
	    }

	//reset the initial stuff so that it remember to go back to this state instead of
  	//the very beginning one.
	if(sw->seisw.zoom_not_set_limit==0)
	{
	        sca->x1begb_init=spar->x1begb;
	        sca->x1endb_init=spar->x1endb;
	        sca->x2begb_init=spar->x2begb;
	        sca->x2endb_init=spar->x2endb;
	}


    //set up the gc, it uses the core.width and core.height
    SetClipRect(sw);

    //set the scroll bars
    SetVerticalScrollbar(sw);    
    SetHorizontalScrollbar(sw);

    //clear the area and send an expose event and thus draw visual
    XClearArea(XtDisplay((Widget)sw),static_cast<SeiswCA *>(sw->seisw.seisw_ca)->win,0,0,sw->core.width,sw->core.height,True);
    
}

/******************************************************************************
 *
 *  Redisplay:
 *     Called by the Intrinsics whenever a portion of the widget that was
 *     obscured becomes exposed.   
 *
 *****************************************************************************/
static void 
Redisplay (
        Widget w,
        XEvent *event,
        Region region 
          ) 
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(w);

 /* Call the function that draws the widget visual. */ 
   if (wc->seisw_class.draw_visual) 
     (*wc->seisw_class.draw_visual) (w);

 /* Call the function that draws the widget shadow. */
   if (wc->seisw_class.draw_shadow) 
     (*wc->seisw_class.draw_shadow) (w);

 /* Envelop our superclass expose method.  The superclass expose
    method of XmPrimitive knows how to draw the border highlight. */ 
   (*(xmPrimitiveClassRec.core_class.expose))(w, event, region);

}



/******************************************************************************
 *
 *  SetValues:
 *     Called by the Intrinsics when an application attempts to
 *     change the value of a resource.  
 *
 *****************************************************************************/
static Boolean 
SetValues (
        Widget old_w,
        Widget request_w,
        Widget new_w,
        ArgList args,
        Cardinal *num_args
          )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(new_w);
 ExmSeiswWidget cw = (ExmSeiswWidget)old_w;
 ExmSeiswWidget rw = (ExmSeiswWidget)request_w;
 ExmSeiswWidget nw = (ExmSeiswWidget)new_w;
 Boolean redisplayFlag = False;

 /* Validate the value of ExmNseiswShape by calling XmRepTypeValidValue. */

 /* Redisplay on change in sensitivity */
 if (XtIsSensitive(new_w) != XtIsSensitive(old_w))
   redisplayFlag = True;

 /* If the widget's foreground or background color changes, 
    then we must update the GC. */ 
   if (nw->primitive.foreground != cw->primitive.foreground ||
       nw->core.background_pixel != cw->core.background_pixel) {
     if (wc->seisw_class.destroy_gc)
       (*(wc->seisw_class.destroy_gc))((Widget)cw);
     if (wc->seisw_class.create_gc)
       (*(wc->seisw_class.create_gc))((Widget)nw);
     redisplayFlag = True;
   }

 /* Check for application geometry settings. '0' means 'ideal size' */
   if (rw->core.width == FIND_NATURAL_SIZE) {
     nw->core.width = FIND_NATURAL_SIZE;
     nw->seisw.need_to_compute_width = True;
   }
   else if (rw->core.width != cw->core.width) {
     nw->core.width = rw->core.width;
     nw->seisw.pref_width = rw->core.width;
     nw->seisw.need_to_compute_width = False;
   }

   if (rw->core.height == FIND_NATURAL_SIZE) {
     nw->core.height = FIND_NATURAL_SIZE;
     nw->seisw.need_to_compute_height = True;
   }
   else if (rw->core.height != cw->core.height) {
     nw->core.height = rw->core.height;
     nw->seisw.pref_height = rw->core.height;
     nw->seisw.need_to_compute_height = False;
   }

   //we re-initialize if metadata or/and ensemble are reset
   for(int i=0; i<*num_args; i++) {
	if (string(args[i].name)==string(ExmNseiswEnsemble) ||
	    string(args[i].name)==string(ExmNseiswMetadata)) {
	    TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(cw->seisw.seisw_ensemble);
	    SeiswPar * para=static_cast<SeiswPar *>(cw->seisw.seisw_parameters);
	    ReInitialize((Widget)cw,(Widget)nw,NULL,0);
	    redisplayFlag=True;
	    return redisplayFlag;
	} else if (string(args[i].name)==string(ExmNdisplayMarkers)) {
	    redisplayFlag=True;
	    return redisplayFlag;
	}
   }
   return (redisplayFlag);
}



/******************************************************************************
 *
 *  QueryGeometry:
 *     Called by the Intrinsics in response to a proposed changed in geometry.
 *
 *****************************************************************************/
static XtGeometryResult 
QueryGeometry (
        Widget widget,
        XtWidgetGeometry *parent_request,
        XtWidgetGeometry *child_reply
              ) 
{
 ExmSeiswWidget sw = (ExmSeiswWidget) widget;

   if (!XtIsRealized(widget)) {   /* Seisw has not yet been realized. */ 
     child_reply->width  = sw->seisw.default_width; //XtWidth(widget);   /* might be 0 */
     child_reply->height = sw->seisw.default_height; //XtHeight(widget);  /* might be 0 */
   } else {                       /* Seisw has been realized. */
     if (parent_request->request_mode & CWWidth) {
	child_reply->width=parent_request->width;
     } 
     if (parent_request->request_mode & CWHeight) {
        child_reply->height=parent_request->height;
     }
   }

 /* Return Seisw's preferred size */
   return XmeReplyToQueryGeometry(widget, parent_request, child_reply);
}

static void SetBox(Widget w)
{
    int i,j;
    float x1min,x1max,x2min,x2max;
    ExmSeiswWidget sw = (ExmSeiswWidget)w;

    TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(sw->seisw.seisw_ensemble);
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    /* Modified March 2007:  Never trust nmember. Previous was this:
    int nmember=sca->nmember;
    *  Changed to the following */
    int nmember;
    if(sca->nmember != tse->member.size())
    {
	sca->nmember=tse->member.size();
    }
    nmember=sca->nmember;

    int is;
    for(is=0;is<nmember;++is) 
    {
	if(tse->member[is].live)
	{
    	  x1min=(tse->member)[is].t0;
    	  x1max=(tse->member)[is].endtime();
	  break;
	}
    }
    // Frozen constants, but we don't want to abort in this condition here
    if(is==nmember)
    {
	x1min=-10.0;
	x2max=50.0;
    }
    else
    {
        for(i=0;i<nmember;++i) {
	    if(tse->member[i].live)
	    {
              x1min=MIN(tse->member[i].t0,x1min);
              x1max=MAX(tse->member[i].endtime(),x1max);
	    }
        }
    }

    if (spar->use_variable_trace_spacing) {
        x2min=sca->x2[0];
        x2max=sca->x2[0];
        for(i=1;i<nmember;++i) {
            x2min=MIN(x2min,sca->x2[i]);
            x2max=MAX(x2max,sca->x2[i]);
        }
    } else {
            x2min=sca->x2[0]-0.5;
	    if(nmember==1)
		x2max=2.0;
	    else
            	x2max=sca->x2[nmember-1]+0.5;
    }

    //
    // Set x1beg and x1end for auto scaling.  Not else currently
    // because in that case assume x1beg and x1end are set in construction
    //
    if (spar->time_scaling=="auto")
    {
         spar->x1begb=x1min;
         spar->x1endb=x1max;
         spar->x1beg=x1min;
         spar->x1end=x1max;
    } else {
         spar->x1begb=spar->x1beg;
         spar->x1endb=spar->x1end;
    }

    if(spar->trace_axis_scaling=="auto")
    {
         spar->x2begb=x2min;
         spar->x2endb=x2max;
         spar->x2beg=x2min;
         spar->x2end=x2max;
    } else {
         spar->x2begb=spar->x2beg;
         spar->x2endb=spar->x2end;
    }

    sca->f1=spar->x1beg;
    spar->x2endb = static_cast<float>(spar->x2end);

}

static void HandlePreRender(Widget w)
{
    int i,j;
    float x1min,x1max,x2min,x2max;
    ExmSeiswWidget sw = (ExmSeiswWidget)w;

    TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(sw->seisw.seisw_ensemble);
    /* Return immediately if there is not data to process */
    if(tse==NULL) return;
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    /* changed March 2007:  Again don't trust nmember stored here
	and reset it if necessary.  Previous:
    int nmember=sca->nmember;
    *  Changed to the following: */
    int nmember;
    if(sca->nmember != tse->member.size())
    {
	sca->nmember=tse->member.size();
    }
    nmember=sca->nmember;
/*
    x1min=(tse->member)[0].t0;
    x1max=(tse->member)[0].endtime();
    for(i=0;i<nmember;++i) {
	x1min=MIN(tse->member[i].t0,x1min);
	x1max=MAX(tse->member[i].endtime(),x1max);
    }

    if (spar->use_variable_trace_spacing) {
	x2min=sca->x2[0];
 	x2max=sca->x2[0];
	for(i=1;i<nmember;++i) {
	    x2min=MIN(x2min,sca->x2[i]);
	    x2max=MAX(x2max,sca->x2[i]);
	}
    } else {
	    x2min=sca->x2[0];
	    x2max=sca->x2[nmember-1];
    }
*/
    //
    // Set x1beg and x1end for auto scaling.  Not else currently
    // because in that case assume x1beg and x1end are set in construction
    //
/*    if (spar->time_scaling=="auto")
    {
         spar->x1begb=x1min;
         spar->x1endb=x1max;
         spar->x1beg=x1min;
         spar->x1end=x1max;
    } else {
         spar->x1begb=spar->x1beg;
         spar->x1endb=spar->x1end;
    }

    if(spar->trace_axis_scaling=="auto")
    {
         spar->x2begb=x2min;
         spar->x2endb=x2max;
         spar->x2beg=x2min;
         spar->x2end=x2max;
    } else {
         spar->x2begb=spar->x2beg;
         spar->x2endb=spar->x2end;
    }

    sca->f1=spar->x1beg;
    spar->x2endb = static_cast<float>(spar->x2end);
*/
    int tempx, tempy;
    /* determine good size for axes box */
    xSizeAxesBox_peng(XtDisplay(sw),sca->win,
        spar->labelfont,spar->titlefont,spar->style,
        &tempx,&tempy,&(sca->width),&(sca->height));
    sca->imageOutOfDate = 1;

    sca->x=spar->xbox+tempx;
    sca->y=spar->ybox+tempy;

    int width1=sca->width;
    int height1=sca->height;
    // Determine size.  Depends on setting of time_scaling. If auto get it from
    // the data, otherwise use the internal parameters.
    sca->d1=static_cast<float>(tse->member[0].dt);

    // Get the number of samples from x1beg and x1end.
    // With above logic this should work if time_scaling is auto or manual
    sca->n1=SEISPP::nint((spar->x1end-spar->x1beg)/(sca->d1));
    sca->n2=sca->nmember;

    // Sanity checks on n1
/*
    if(sca->n1<2) {
        char message[256];
        sprintf(message,"SeismicPlot::refresh():  Plot range error.\n%d samples in requested time range of %lf to %lf\n", sca->n1, spar->x1begb, spar->x1endb);
        throw SeisppError(string(message));
    }
    else if(sca->n1>MAXPLOTSAMPLES) {
        char message[512];
        sprintf(message,"SeismicPlot::refresh(): Plot range request too large \nTime range of %lf to %lf requires %n samples.  Sanity check overrides request.  Check input parameters\n",spar->x1begb,spar->x1endb,sca->n1);
        throw SeisppError(string(message));
    }
*/

    // d2 and f2 are defined in this object by less obscure names
    sca->d2=static_cast<float>(spar->trace_spacing);
    sca->f2=static_cast<float>(spar->first_trace_offset);

    // This buffer is needed in plot loop below.  We construct a fortran-type matrix
    // of floats with the data aligned by member[i].t0 values. Traces are in columns
    // of the matrix.  Depend on new throwing an exception if alloc fails.
    if(sca->z!=NULL) { delete [] sca->z; sca->z=NULL; }
    sca->z=new float[(sca->n1)*(sca->n2)];

    // internal function used here
    int iz,nz=(sca->n1)*(sca->n2);
    //should this be this instead of spar->x1begb?
    load_z_matrix_peng(tse->member,sca->z,sca->n1,sca->n2,spar->x1beg,sca->d1);

    // handle clip stuff.
    // Modified from xwigb to set clip levels
    // Percentage determines percent of samples to be left unclipped.
    // This is as in xwigb.  Difference here is we use STL vector
    // and standard STL nth_element algorithm
    // instead of SU's internal quick sort routine.
    vector<float> temp;
    temp.reserve(nz);
    if(!spar->clip_data) spar->perc=100.0;
    for (iz=0; iz<nz; iz++)temp.push_back(fabs(sca->z[iz]));
    vector<float>::iterator iziter;
    iz = static_cast<int>((static_cast<float>(nz)*(spar->perc)/100.0));
    if (iz<0) iz = 0;
    if (iz>nz-1) iz = nz-1;
    iziter=temp.begin()+iz;
    nth_element(temp.begin(),iziter,temp.end());
    sca->clip = *iziter;

    /* main event loop */
    sca->p2beg=0.0,sca->p2end=0.0;
    if(sca->image==NULL)
        sca->image=NULL;
    else {
        XDestroyImage(sca->image);
        sca->image=NULL;
    }

}


static void
SetClipRect(ExmSeiswWidget widget)
{
  ExmSeiswWidget sw = widget;
  Position x,y;
  Dimension w,h;
  XRectangle rect;

  x = sw->primitive.shadow_thickness + sw->primitive.highlight_thickness;
  y = sw->primitive.shadow_thickness + sw->primitive.highlight_thickness;
  w = ((int)sw->core.width <= 2 * x) ? 1 : (sw->core.width - (2 * x));
  h = ((int)sw->core.height <= 2 * y) ? 1 : (sw->core.height - (2 * y));

  rect.x = 0;
  rect.y = 0;
  rect.width = w;
  rect.height = h;

  if (sw->seisw.normal_gc)
    XSetClipRectangles(XtDisplay(sw), sw->seisw.normal_gc, x, y,
                       &rect, 1, Unsorted);

  if (sw->seisw.insensitive_gc)
    XSetClipRectangles(XtDisplay(sw), sw->seisw.insensitive_gc, x, y,
                       &rect, 1, Unsorted);

  /* Set highlight clip in DrawHighlight */
}

//Note, this function assumes that y direction of the ensemble display
//is regular in the sense that it is "0,1,2...", i.e., it is n-1 for the
//nth seismogram
static void color_deleted_traces(ExmSeiswWidget sw)
{
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(sw->seisw.seisw_ensemble);
    Colormap screen_colormap;
    Display *dpy=XtDisplay((Widget)sw);
    XColor red;
    GC dgc;
    XGCValues *values=NULL;
    Status rc;
    int istart1, istart2, iend1, iend2;
    int scr;
    char * dcolor=static_cast<char *>(sw->seisw.deleted_color);

    scr = DefaultScreen(dpy);
    unsigned long black = BlackPixel(XtDisplay((Widget)sw),scr);

    if (spar==NULL || tse ==NULL) return;
    if (sw->seisw.display_markers == NULL) return;
    dgc= XCreateGC(XtDisplay((Widget)sw),sca->win,0,values);

    screen_colormap = DefaultColormap(dpy, DefaultScreen(dpy));
    if(dcolor==NULL)
	rc = XAllocNamedColor(dpy, screen_colormap, string("red").c_str(),
			&red, &red);
    else
	rc = XAllocNamedColor(dpy, screen_colormap, dcolor, &red, &red);
    XSetForeground(dpy, dgc, red.pixel);

    //assuming here n-1 for the nth (index n-1) seismogram
    for(int i=(int)spar->x2begb-1; i<(int)spar->x2endb+1; i++) {
	if (i>=0 && i<tse->member.size()) {
	    if (!tse->member[i].live) {
		real_to_screen(spar->x1begb,(float)(i+1), &istart1, &istart2, sw);
		real_to_screen(spar->x1endb,(float)(i+1), &iend1, &iend2, sw);
		XDrawLine(dpy, sca->win, dgc, istart1, istart2, iend1, iend2);		
	    }
	}
    }

    XSetForeground(dpy, dgc, black);
    XFreeGC(XtDisplay((Widget)sw),dgc);
}

static void do_display_markers(ExmSeiswWidget sw)
{
    SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
    SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
    Colormap screen_colormap;
    Display *dpy=XtDisplay((Widget)sw);
    XColor red, green;
    GC marker_gc;
    XGCValues *values=NULL;
    Status rc;
    float start, end;
    int istart, iend;
    int scr;

    scr = DefaultScreen(dpy);
    unsigned long black = BlackPixel(XtDisplay((Widget)sw),scr);

    if (spar==NULL) return;
    if (sw->seisw.display_markers == NULL) return;
    DisplayMarkerData dm=static_cast<DisplayMarkerData>(sw->seisw.display_markers);
    marker_gc= XCreateGC(XtDisplay((Widget)sw),sca->win,0,values);
 
    screen_colormap = DefaultColormap(dpy, DefaultScreen(dpy));
    rc = XAllocNamedColor(dpy, screen_colormap, dm->beam_color.c_str(), &red, &red);
    rc = XAllocNamedColor(dpy, screen_colormap, dm->robust_color.c_str(), &green, &green);

    start=dm->robust_tw.start;
    end=dm->robust_tw.end;
    XSetForeground(dpy, marker_gc, green.pixel);
    if (start >= spar->x1begb && start <= spar->x1endb) {
	real_to_screen(start,end,&istart,&iend,sw);
    	XDrawLine(dpy, sca->win, marker_gc, istart, sca->y, istart, sca->y+sca->height);
    }
    if (end >= spar->x1begb && end <= spar->x1endb) {
	real_to_screen(end,start,&iend,&istart,sw);
        XDrawLine(dpy, sca->win, marker_gc, iend, sca->y, iend, sca->y+sca->height);
    }

    start=dm->beam_tw.start;
    end=dm->beam_tw.end;
    XSetForeground(dpy, marker_gc, red.pixel);
    if (start >= spar->x1begb && start <= spar->x1endb) {
	real_to_screen(start,end,&istart,&iend,sw);
        XDrawLine(dpy, sca->win, marker_gc, istart, sca->y, istart, sca->y+sca->height);
    }
    if (end >= spar->x1begb && end <= spar->x1endb) {
	real_to_screen(end,start,&iend,&istart,sw);
        XDrawLine(dpy, sca->win, marker_gc, iend, sca->y, iend, sca->y+sca->height);
    }

    XSetForeground(dpy, marker_gc, black);
    XSetForeground(dpy, sw->seisw.normal_gc, black);

    XFreeGC(XtDisplay((Widget)sw),marker_gc);
}

/******************************************************************************
 *
 *  DrawVisual:
 *     Called by ExmSeisw whenever it needs to draw or redraw the
 *     widget visual (either an oval or a rectangle).
 *
 *****************************************************************************/
static void 
DrawVisual (
        Widget w
           )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(w);
 ExmSeiswWidget sw = (ExmSeiswWidget)w;

 TimeSeriesEnsemble * tse=static_cast<TimeSeriesEnsemble *>(sw->seisw.seisw_ensemble);
 SeiswPar * spar=static_cast<SeiswPar *>(sw->seisw.seisw_parameters);
 SeiswCA * sca=static_cast<SeiswCA *>(sw->seisw.seisw_ca);
 int xposition, yposition;
 char *title;
#ifdef DEBUG_WIDGET
cerr << "Entering DrawVisual"<<endl;
showscaling(sw);
#endif


    if (!XtIsRealized((Widget)sw))
    return;

    if (spar==NULL || sca==NULL || tse==NULL) return;
 
    if (sw->seisw.xItemCount && sw->seisw.yItemCount) {
	SetClipRect(sw);
    }

 /* Draw the visual, space permitting. */
//        try {

            /*  since we are not dealing with a static window anymore, we need to adjust
                sca->width and sca->height */

	    HandlePreRender(w);
	/* Handle scaling */
	if(!is_resolution_set(w))
	{
		CalcVisualSize(w);
		compute_and_set_resolution(sw,spar);
	}

	    sca->width=sw->core.width-(sw->primitive.highlight_thickness+sw->primitive.shadow_thickness
			+spar->xbox+sw->seisw.margin_width);
	    sca->height=sw->core.height-(sw->primitive.highlight_thickness+sw->primitive.shadow_thickness
			+spar->ybox+sw->seisw.margin_height);

                spar->x1endb=spar->x1begb+sw->seisw.x1_resolution*(float)(sw->core.width-
                  sw->primitive.shadow_thickness-sw->primitive.highlight_thickness+(sw->seisw.margin_width+spar->xbox));
                spar->x2begb=spar->x2endb-sw->seisw.x2_resolution*(float)(sw->core.height-
                    sw->primitive.shadow_thickness-sw->primitive.highlight_thickness+(sw->seisw.margin_height+spar->ybox));

		if (spar->x1endb-spar->x1begb > spar->x1end-spar->x1beg) {
			spar->x1endb=spar->x1end;
			spar->x1begb=spar->x1beg;
			sca->width=static_cast<int>((spar->x1end-spar->x1beg)
					/sw->seisw.x1_resolution);
		}
		if (spar->x2endb-spar->x2begb > spar->x2end-spar->x2beg) {
			spar->x2endb=spar->x2end;
			spar->x2begb=spar->x2beg;
			sca->height=static_cast<int>((spar->x2end-spar->x2beg)
				/sw->seisw.x2_resolution);
		}
		


            /* Since we are repainting every time, so image is always out of date*/
//            if (sca->imageOutOfDate) {

            if (sca->image!=NULL) {
                XDestroyImage(sca->image);
            }

/*
            if (sw->seisw.x_top_position==0) xposition=sca->x; 
            else xposition=sca->x; //sw->seisw.margin_width;

            if (sw->seisw.y_top_position==0) {
                yposition=sca->y;                                                   
                title=spar->title;
		sca->height=sca->height-sw->seisw.margin_height-sca->y; //need to consider the title
            } else {
                yposition=sca->y; //0;
                sca->height=sca->height-sw->seisw.margin_height-sca->y; //x doesn't need this since x axis is  
						// displayed at the beginning while y axis is at the 
						// bottom
                title=strdup("");
            }
*/

	    xposition=sca->x;
	    yposition=sca->y;
            title=spar->title;
	    //This is for the case that the widget display area is bigger than the seismogram display
	    if (sca->width > spar->wbox-xposition) sca->width=spar->wbox-xposition;
	    else sca->width=sca->width-xposition;
	    if (sca->height > spar->hbox-yposition) sca->height=spar->hbox-yposition;
  	    else sca->height=sca->height-yposition;
	    
	    if (sca->width < 0) sca->width=0;
	    if (sca->height < 0) sca->height=0;

            sca->image = newBitmap_peng(XtDisplay(w),sca->width,sca->height,
                    sca->n1,sca->d1,sca->f1,sca->n2,sca->x2,sca->z,
                    spar->x1begb,spar->x1endb, 
		    spar->x2begb,spar->x2endb,
                    spar->xcur,sca->clip,spar->wt,spar->va,
                    &(sca->p2beg),&(sca->p2end),sca->endian,spar->interp,
                    spar->wigclip,spar->style);

//                sca->imageOutOfDate = 1;
//            }
		
	    //This is to update the global variable to reflect starting position change, so that
 	    //Btn1Down, etc... could display properly
	    sca->x=xposition;
	    sca->y=yposition;

            /* draw image (before axes so grid lines visible) */
            XPutImage(XtDisplay(w),sca->win,sw->seisw.normal_gc,sca->image,0,0,xposition,yposition,
                sca->image->width,sca->image->height);

	    /* for the deleted seismograms, we mark them with different color here */
	    color_deleted_traces(sw);    

	    if (sw->seisw.display_attributes != NULL) {
	        DisplayAttributes da=static_cast<DisplayAttributes>(sw->seisw.display_attributes);
	        da->x2begb=spar->x2begb;
	        da->x2endb=spar->x2endb;
	    
                /* draw axes on top of image */
                xDrawAxesBox_peng(XtDisplay(w),sca->win,
                  xposition,yposition,sca->width,sca->height,
                  spar->x1begb,spar->x1endb, 
		  0.0,0.0,
                  spar->d1num,spar->f1num,spar->n1tic,spar->grid1,spar->label1,
                  spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end,
                  spar->d2num,spar->f2num,spar->n2tic,spar->grid2,spar->label2,
                  spar->labelfont,title,spar->titlefont,
                  spar->labelcolor,spar->titlecolor,spar->gridcolor,
                          spar->style, xposition,yposition,da->str_origin);
		   //sw->seisw.margin_width,sw->seisw.margin_height,da->str_origin);
	    } else 
                xDrawAxesBox_peng(XtDisplay(w),sca->win,
                  xposition,yposition,sca->width,sca->height,
                  spar->x1begb,spar->x1endb,
                  0.0,0.0,
                  spar->d1num,spar->f1num,spar->n1tic,spar->grid1,spar->label1,
                  spar->x2begb,spar->x2endb,sca->p2beg,sca->p2end,
                  spar->d2num,spar->f2num,spar->n2tic,spar->grid2,spar->label2,
                  spar->labelfont,title,spar->titlefont,
                  spar->labelcolor,spar->titlecolor,spar->gridcolor,
                          spar->style, xposition,yposition,NULL);
	      // sw->seisw.margin_width,sw->seisw.margin_height,NULL);
 
  	    do_display_markers(sw);

	    //assuming that whenever a visual is re-drawn under the case of button 1 down, 
	    //that must mean that the mouse has gone out of the window area, therefore,
	    //we need to skip drawing the old rubber box, instead, only draw the new one. 
	    if (sw->seisw.rubberbox_enable==1) sca->going_out=1;
	    //same thing for btn3 down time window selection rubberbox
	    if (sw->seisw.tw_selection_enable==1) sca->going_out=1;

//        } catch (SeisppError) {
//            cerr << "Image Rendering Error in render_image"<<endl;
//	    throw SeisppError("Image render failure");
//        }

	    SetVerticalScrollbar(sw);
	    SetHorizontalScrollbar(sw);


	if(sw->seisw.display_attributes!=NULL)
		XtCallCallbackList(w, sw->seisw.display_attr_callback, NULL);
#ifdef DEBUG_WIDGET
cerr << "Exiting DrawVisual"<<endl;
showscaling(sw);
#endif

}



/******************************************************************************
 *
 *  DrawShadow:
 *      Called by Redisplay.
 *
 *****************************************************************************/
static void 
DrawShadow (
        Widget w
           )
{
 ExmSeiswWidget sw = (ExmSeiswWidget)w;

 /* If there is enough room in the widget to draw a shadow, 
    and if the shadow thickness resource is nonzero, then draw a 
    Motif-style shadow in the appropriate place around the widget's border. */ 
   if (sw->core.width  > (2 * sw->primitive.highlight_thickness) &&
       sw->core.height > (2 * sw->primitive.highlight_thickness) &&
       sw->primitive.shadow_thickness > 0) 
     XmeDrawShadows (XtDisplay (sw), XtWindow (sw),
      		     sw->primitive.top_shadow_GC,
      		     sw->primitive.bottom_shadow_GC,
      		     sw->primitive.highlight_thickness,
      		     sw->primitive.highlight_thickness,
      		     sw->core.width - (2 * sw->primitive.highlight_thickness),
      		     sw->core.height - (2 * sw->primitive.highlight_thickness),
      		     sw->primitive.shadow_thickness, 
                     XmSHADOW_ETCHED_OUT);
}



/******************************************************************************
 *
 *  CreateGC:
 *      Called by Initialize and by SetValues.
 *
 *****************************************************************************/
static void 
CreateGC (
        Widget w
         )
{
 ExmSeiswWidget sw = (ExmSeiswWidget)w;
 XGCValues   values;
 XtGCMask    valueMask;
 Arg         args[2];
 Pixmap      insensitiveStippleBitmap;

 unsigned long black, white;
 Display *dpy;
 int scr;

 /* This function creates two GC's: one to render a sensitive widget
    and the other to render an insensitive widget. */


 /* First, create the sensitive GC (named normal_GC). */ 
   valueMask = GCForeground | GCBackground | GCGraphicsExposures;
/*
   values.foreground = sw->primitive.foreground;
   values.background = sw->core.background_pixel;
*/
   dpy=XtDisplay(sw);
   scr = DefaultScreen(dpy);
   black = BlackPixel(dpy,scr);
   white = WhitePixel(dpy,scr);

   values.foreground = black;
   values.background = white;
   values.graphics_exposures = False;
   sw->seisw.normal_gc = XtGetGC ((Widget)sw, valueMask, &values);

   /* The following is from SeismicPlot */
   /* make sure foreground/background are black/white */
   //XSetForeground(dpy,sw->seisw.normal_gc,black);
   //XSetBackground(dpy,sw->seisw.normal_gc,white);


 /* Next, create the insensitive GC.  This GC will share the same
    foreground, background, and graphics exposures as the sensitive
    GC, but will hold a different fill style and stipple pattern. 
    The stipple pattern is stored in the XmNinsensitiveStippleBitmap
    resource of XmScreen. */
   valueMask |= GCFillStyle | GCStipple;
   values.fill_style = FillStippled;

 /* Gather the Motif-appropriate insensitive stipple bitmap. */
   XtSetArg(args[0], XmNinsensitiveStippleBitmap, &insensitiveStippleBitmap); 
   XtGetValues(XmGetXmScreen(XtScreen(w)), args, 1);
   values.stipple = insensitiveStippleBitmap;

   sw->seisw.insensitive_gc = XtGetGC((Widget)sw, valueMask, &values);
}



/******************************************************************************
 *
 *  DestroyGC:
 *      Called by the Destroy method.
 *
 *****************************************************************************/
static void 
DestroyGC ( 
        Widget w
          )
{
 ExmSeiswWidget sw = (ExmSeiswWidget)w;

 /* Deallocate the resources used by the two GC's. */ 
   XtReleaseGC ((Widget)sw, sw->seisw.normal_gc);
   XtReleaseGC ((Widget)sw, sw->seisw.insensitive_gc);
}



/******************************************************************************
 *
 *  SelectGC:
 *     Called by DrawVisual. 
 *
 *****************************************************************************/
static GC 
SelectGC (
        Widget w
         )
{
 ExmSeiswWidget sw = (ExmSeiswWidget)w;
 GC drawGC;

 /* Select between the normal (sensitive) GC and the insensitive GC. */ 
   drawGC = XtIsSensitive(w)  ? sw->seisw.normal_gc 
                              : sw->seisw.insensitive_gc;
   return (drawGC);
}



/******************************************************************************
 *
 *  CalcVisualSize: 
 *      Called by CalcWidgetSize.
 *
 *****************************************************************************/
static void 
CalcVisualSize (
        Widget w
               )
{
 ExmSeiswWidget sw = (ExmSeiswWidget)w;

 /* Calculate the ideal size of class's visual */

 if (!XtIsRealized((Widget)sw)) {
   sw->seisw.visual.width = sw->seisw.default_width-2*(
                           sw->primitive.shadow_thickness +
                           sw->primitive.highlight_thickness);
   sw->seisw.visual.height = sw->seisw.default_height-2*(
                           sw->primitive.shadow_thickness +
                           sw->primitive.highlight_thickness);

 } else {
   sw->seisw.visual.width = sw->core.width-2*(
                           sw->primitive.shadow_thickness +
                           sw->primitive.highlight_thickness);
   sw->seisw.visual.height =sw->core.height-2*(
                           sw->primitive.shadow_thickness +
                           sw->primitive.highlight_thickness);

 }
}



/******************************************************************************
 *
 *  CalcWidgetSize:
 *      Called by Reconfigure 
 *
 *****************************************************************************/
static void 
CalcWidgetSize(
        Widget w
              )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)XtClass(w);
 ExmSeiswWidget sw = (ExmSeiswWidget)w;

 /* Call CalcVisualSize */
   if (wc->seisw_class.calc_visual_size)
     (*(wc->seisw_class.calc_visual_size))((Widget)sw);

 /* Compute the widget's width if asked to.  Otherwise, set the
    widget's width to the preferred width. */ 
   if (sw->seisw.need_to_compute_width == True)
     sw->core.width = sw->seisw.visual.width + 
                     (2 * (
		           sw->primitive.shadow_thickness +
		           sw->primitive.highlight_thickness));
   else
     sw->core.width = sw->seisw.pref_width;

 /* Compute the widget's height if asked to.  Otherwise, set the
    widget's height to the preferred height. */ 

   if (sw->seisw.need_to_compute_height == True)
     sw->core.height = sw->seisw.visual.height + 
                      (2 * (
		            sw->primitive.shadow_thickness +
		            sw->primitive.highlight_thickness));
   else
     sw->core.height = sw->seisw.pref_height;

}



/******************************************************************************
 *
 *  WidgetDisplayRect:
 *      Called by several Motif managers to determine how to align the visuals 
 *      drawn by primitives.  In addition, an application can access this 
 *      method by calling XmWidgetGetDisplayRect. 
 *
 *****************************************************************************/
static Boolean  
WidgetDisplayRect(
        Widget       w,
        XRectangle  *displayrect
                 )
{
 ExmSeiswWidget  my_widget = (ExmSeiswWidget) w;

   if ((my_widget->seisw.visual.width > 0) && 
       (my_widget->seisw.visual.height > 0)) {
   /* The widget is currently displaying a visual.  Write the dimensions
      of the visual's bounding box into displayrect. */ 

     displayrect->x =       my_widget->seisw.visual.x; 
     displayrect->y =       my_widget->seisw.visual.y; 
     displayrect->width =   my_widget->seisw.visual.width; 
     displayrect->height =  my_widget->seisw.visual.height; 

     return True;  /* Yes, this widget contains something displayable. */
   }
   else  {
     /* The widget is not currently displaying a visual. */
     return False; 
   }
}

XtGeometryResult
_XmMakeGeometryRequest(
        Widget w,
        XtWidgetGeometry *geom )
{
  XtWidgetGeometry allowed ;
  XtGeometryResult answer ;
/****************/

  answer = XtMakeGeometryRequest( w, geom, &allowed) ;

  /* On an almost, accept the returned value and make
   *   a second request to get an XtGeometryYes returned.
   */
  if(    answer == XtGeometryAlmost    )
    {
      /* The Intrinsics protocol guarantees a Yes response
       * to a request with identical geometry to that which
       * was returned by a previous request returning almost.
       */
      *geom = allowed ;
      answer = XtMakeGeometryRequest( w, geom, &allowed) ;
    }
  return answer ;
}


/******************************************************************************
 *
 *  Reconfigure:
 *      Called by the Initialize and SetValues methods originally.
 *      Largely a placeholder now (Sept 2007).  May need to be removed,
 *      but Reconfigure seems to be a special name required of a widget
 *      much like ClassInitialize.
 *
 *****************************************************************************/
static void
Reconfigure (
        WidgetClass class1,
        Widget new_w,
        Widget old_w
                          )
{
 ExmSeiswWidgetClass wc = (ExmSeiswWidgetClass)class1;
 ExmSeiswWidget nw = (ExmSeiswWidget)new_w;
 ExmSeiswWidget cw = (ExmSeiswWidget)old_w;

 /* If Reconfigure is called from a leaf chained method, the following code 
    calls CalcWidgetSize.   If Reconfigure is not called from a leaf chained
    method, wait until the final leaf method is called.  (That is because
    the subclass CalcWidgetSize method may need information derived in its 
    SetValues method.) */

 /* The Reconfigure method can only reconfigure an ExmSeisw widget
    or an ExmSeisw subclass. */
/*
   if (!ExmIsSeisw((Widget)nw)) 
     return;

   if (class1 == XtClass(nw)) {
     if (wc->seisw_class.calc_widget_size)
       (*(wc->seisw_class.calc_widget_size))((Widget)nw);

     nw->seisw.pref_width  = nw->core.width;
     nw->seisw.pref_height = nw->core.height;

   if (cw == NULL ||
      (nw->core.width  == cw->core.width && 
       nw->core.height == cw->core.height)
      ) {

     if (wc->core_class.resize) 
       (*(wc->core_class.resize))((Widget)nw);

     }

   }
   else 
     nw->seisw.need_to_reconfigure = True;
*/

}



/******************************************************************************
 *
 *  SetSelectedVisual
 *      Get the select color of the parent and set our foreground 
 *
 *****************************************************************************/

static void
SetSelectedVisual (
	Widget wid)
{
    /* our parent is notifying us that its select color has changed
       so let's try to get the new value using the Container trait
       getValue method and then change our foreground - somehow, there 
       is the  assumption that if this method gets called with SelectColor
       on, then it must be a Container, which is ok in 2.0 */
    XmContainerDataRec container_data ;
    XmContainerTrait container_trait ;
    Pixel select_color = XmUNSPECIFIED_PIXEL;
    ExmSeiswWidget sw = (ExmSeiswWidget)wid;

    /* get the container trait record */
    container_trait = (XmContainerTrait) 
	XmeTraitGet((XtPointer)
		    XtClass(XtParent(wid)), 
		    XmQTcontainer) ;

    if (container_trait) {
	/* get the container information */
	container_data.valueMask = ContSelectColor ;
	container_trait->getValues(XtParent(wid), &container_data);

	if (container_data.valueMask & ContSelectColor) {
	    /* if the Container uses reverse ground, convert that
	       to using black pixel */
	    if (container_data.select_color == XmREVERSED_GROUND_COLORS)
		select_color = BlackPixelOfScreen(XtScreen(wid));
	    else
		select_color = container_data.select_color;
	} 
    }

    /* only set the foreground of the one selected */
    if ((select_color != XmUNSPECIFIED_PIXEL) &&
	(sw->seisw.saved_foreground != XmUNSPECIFIED_PIXEL))
	XtVaSetValues(wid, XmNforeground, select_color, NULL) ;

}


/******************************************************************************
 *
 *  HandleRedraw 
 *      This is a trait method of the XmQTcareParentVisual trait. 
 *
 *****************************************************************************/

static Boolean 
HandleRedraw (
	Widget kid, 	       
	Widget cur_parent,
	Widget new_parent,
	Mask visual_flag)
{
    Boolean redraw = False;
    XmCareVisualTrait care_visual ;

    if (visual_flag & VisualSelectColor) {

	SetSelectedVisual (kid) ;

	/* no need to set redraw to True since SetSelectedVisual calls
	   XtSetValues which already re-exposes when foreground changes */
    }

    /* at this point we need to envelop our superclass trait method
       since we are overiding it but we still want its job to be done */
    care_visual = (XmCareVisualTrait)
	XmeTraitGet(xmPrimitiveWidgetClass, XmQTcareParentVisual) ;

    /* care_visual never NULL on Primitive ! */
    redraw |= care_visual->redraw(kid, cur_parent, new_parent, visual_flag) ;

    return redraw ;
}


static void
_XmDestroyParentCallback_peng(
        Widget w,
        XtPointer client_data,  /* unused */
        XtPointer call_data )   /* unused */
{
   XtDestroyWidget (XtParent (w));
}


/******************************************************************************
 *
 *  ExmCreateSeisw:
 *       Called by an application.
 *
 *****************************************************************************/
Widget
ExmCreateSeisw (
        Widget parent,
        char *name,
        Arg *arglist,
        Cardinal argCount
                       )
{
  Widget sw, nw;
  ArgList Args;
  Arg my_args[6];
  Cardinal nargs;

  nargs = 0;
  XtSetArg(my_args[nargs], XmNscrollingPolicy, XmAPPLICATION_DEFINED), nargs++;
  XtSetArg(my_args[nargs], XmNscrollBarPlacement, XmBOTTOM_RIGHT), nargs++;
  XtSetArg(my_args[nargs], XmNvisualPolicy, XmVARIABLE), nargs++;
  XtSetArg(my_args[nargs], XmNscrollBarDisplayPolicy, XmSTATIC), nargs++;
  XtSetArg(my_args[nargs], XmNshadowThickness, 0), nargs++;
  Args = XtMergeArgLists(arglist, argCount, my_args, nargs);
  sw = XtCreateManagedWidget(name , xmScrolledWindowWidgetClass, parent,
                             Args, argCount + nargs);
  /* Older versions had this free.  Seems evil and valgrind complained 
     so removed.  May create leaks as a side effect */
  //XtFree((char *) Args);

  nw = XtCreateWidget(name, exmSeiswWidgetClass, sw, arglist, argCount);

  XtAddCallback (nw, XmNdestroyCallback, _XmDestroyParentCallback_peng, NULL);
  return (nw);

}

