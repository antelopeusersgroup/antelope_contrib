/* $XConsortium: SeiswP.h /main/5 1995/07/15 20:41:32 drk $ */
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

/*******************************************************************************
 *
 * SeiswP.h: The widget private header file for the ExmSeisw demonstration
 *            widget. 
 *
 ******************************************************************************/


/* Ensure that the file be included only once. */
#ifndef _ExmSeiswP_h
#define _ExmSeiswP_h

/* Include appropriate files. */
#include "Seisw.h" /* public header file for the ExmSeisw widget */
#include <Xm/PrimitiveP.h> /* private header file for the XmPrimitive widget */

/* scroll bar */
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollFrameT.h>
#include <Xm/DrawingA.h>

/* Allow for C++ compilation. */
/*
#ifdef __cplusplus
extern "C" {
#endif 
*/

/* Make the following seven methods inheritable by subclasses of ExmSeisw. */ 
#define ExmInheritDrawVisual	 ((XtWidgetProc) _XtInherit)
#define ExmInheritDrawShadow	 ((XtWidgetProc) _XtInherit)
#define ExmInheritCreateGC	 ((XtWidgetProc) _XtInherit)
#define ExmInheritDestroyGC	 ((XtWidgetProc) _XtInherit)
#define ExmInheritSelectGC	 ((ExmSelectGCProc) _XtInherit)
#define ExmInheritCalcVisualSize ((XtWidgetProc) _XtInherit)
#define ExmInheritCalcWidgetSize ((XtWidgetProc) _XtInherit)
#define ExmInheritReconfigure    ((ExmReconfigureProc) _XtInherit)

/* Provide typedefs for inheritable methods that do not already
   have an appropriate data type. */
typedef GC   (*ExmSelectGCProc)(
                        Widget);
typedef void (*ExmReconfigureProc)(
			WidgetClass,
			Widget,
			Widget) ;

/* Define the widget class part. */
typedef struct _ExmSeiswClassPart
{
	XtWidgetProc		draw_visual;
	XtWidgetProc		draw_shadow;
	XtWidgetProc		create_gc;
	XtWidgetProc		destroy_gc;
	ExmSelectGCProc		select_gc;
	XtWidgetProc		calc_visual_size;
	XtWidgetProc		calc_widget_size;
        ExmReconfigureProc      reconfigure; 
	XtPointer		extension;
} ExmSeiswClassPart;

/* Define the full class record. */
typedef struct _ExmSeiswClassRec
{
	CoreClassPart		core_class;
	XmPrimitiveClassPart	primitive_class;

	ExmSeiswClassPart	seisw_class;
} ExmSeiswClassRec;


typedef struct {
  XEvent *event;                /* the event causing the SashAction */
  String *params;               /* the TranslationTable params */
  Cardinal num_params;          /* count of params */
} SeiswCallDataRec, *SeiswCallData;


/* Define the subclassing level index to be used with ResolvePartOffset */
#define ExmSeiswIndex (XmPrimitiveIndex + 1)

/* Make the full class record externally accessible. */
externalref ExmSeiswClassRec exmSeiswClassRec;

/* Define the widget instance part. */
typedef struct _ExmSeiswPart
{
/*
	XtPointer		seisw_ensemble;
 	XtPointer		seisw_metadata;
 	XtPointer		seisw_parameters;
	XtPointer		seisw_ca;
	XtPointer		seisw_pick;
	XtPointer		cleanup_data;
  	XtPointer		display_markers;
	XtCallbackList		btn2_callback;
	XtCallbackList		btn3_callback;
 	Dimension               edit_enable;

        XmScrollBarWidget    hScrollBar;
        XmScrollBarWidget    vScrollBar;
        unsigned char ScrollBarDisplayPolicy;
*/ 

//------------
 
   double x1_resolution, x2_resolution; //how much real length in each direction represented
                                        //by each pixel, useful when resizing.
   int resolution_set;

   int zoom_factor;

   /* srolling part */
   int x_top_position;
   int previous_x_top_position;
   int y_top_position;   //204-208
   int previous_y_top_position; //208-212
   int from_zoom; //212-216
   /* Added Feb 2007:  used to disable resetting limit when resizing
    after a zoom (used in Btn1UpProc and Resize)*/
   int zoom_not_set_limit;  

   int xItemCount; //216-220
   int yItemCount; //220-224
   int xVisibleItemCount; //224-228
   int yVisibleItemCount; //228-232

//-------------------

   //This is for dragging stuff
   XtIntervalId      DragID; //232-236
   int     LeaveDir; //236-240
   int     drag_enable; //240-244


   //This is for displaying single beam plot and also correlation results, we don't want anything
   //other than a rubber box
   //This is a resource, so don't set it in Initialize
   //unsigned char display_only;

   //This is for rubber box stuff when 1st btn down
   int rubberbox_enable; //244-248
   //This is for tw window selection stuff when 3rd btn down
   int tw_selection_enable; //248-252
   //This is for the edit mode of deleting traces
   //int edit_enable;
   //This is for displaying results
   Dimension display_only;  //252-254
   Dimension edit_enable; //254-256

//-------------------

  	Dimension		default_width;
	Dimension		default_height;
	Dimension		margin_height;
        Dimension		margin_width;  //264

 /* Provide space for the other protected fields of ExmSeisw. */
 /* normal_gc holds a Graphics Context for rendering the visual
    when the value of XmNsensitive is True. */
	GC			normal_gc;

 /* insensitive_gc holds a Graphics Context for rendering the visual
    when the value of XmNsensitive is False. */
	GC			insensitive_gc;

 /* pref_width holds an integral value representing the widget's 
    current preferred width. */
	Dimension		pref_width;

 /* pref_width holds an integral value representing the widget's 
    current preferred height. */
	Dimension		pref_height;

 /* need_to_compute_width is a flag.  If its value is True, then the widget
    needs to renegotiate its width. */
	Boolean			need_to_compute_width;

 /* need_to_compute_height is a flag.  If its value is True, then the widget
    needs to renegotiate its height. */
	Boolean			need_to_compute_height;

 /* visual is an XRectangle value representing the bounding box of the
    widget's visual. */ 
	XRectangle              visual;

 /* need_to_reconfigure is a flag.  If its value is True, then the widget
    needs to call the reconfigure method. */ 
	Boolean			need_to_reconfigure;

 /* saved_foreground is used for the management of the selected state */ 
	Pixel			saved_foreground;


        XtPointer               seisw_ensemble;
        XtPointer               seisw_metadata;
        XtPointer               seisw_parameters;
        XtPointer               seisw_ca;
        XtPointer               seisw_pick;
        XtPointer               cleanup_data;
        XtPointer               display_markers;
	XtPointer		deleted_color;
	XtPointer		display_attributes;

//------------

        XmScrollBarWidget    hScrollBar;
        XmScrollBarWidget    vScrollBar;

   	XmScrolledWindowWidget  Mom;   // this is for the scrolled window that is parent of this widget
                                  // in ExmCreateSeisw call for setting up of scrollFrame trait.

        XtCallbackList          btn2_callback;
        XtCallbackList          btn3_callback;
	XtCallbackList		display_attr_callback;


        unsigned char ScrollBarDisplayPolicy;

} ExmSeiswPart;


/* Define the full instance record. */
typedef struct _ExmSeiswRec
{
	CorePart		core;
	XmPrimitivePart		primitive;

	ExmSeiswPart		seisw;
} ExmSeiswRec;

/* Allow for C++ compilation. */ 
/*
#ifdef __cplusplus
} */  /* Close scope of 'extern "C"' declaration which encloses file. */
/*
#endif 
*/

/* Ensure that the file be included only once. */
#endif /* _ExmSeiswP_h */
/* Don't add anything after this #endif */
