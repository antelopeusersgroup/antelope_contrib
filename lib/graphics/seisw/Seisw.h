/* $XConsortium: Seisw.h /main/5 1995/07/15 20:41:28 drk $ */
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
 * Seisw.h: The widget public header file for the ExmSeisw demonstration
 *           widget.
 *
 ******************************************************************************/


/* Ensure that the file be included only once. */
#ifndef _ExmSeisw_h
#define _ExmSeisw_h


/* Include appropriate files. */
#include <Xm/Xm.h>  /* public header file for the XmPrimitive widget */

/* Allow for C++ compilation. */
#ifdef __cplusplus
extern "C" {
#endif

/* Define the widget class and widget record. */
externalref WidgetClass exmSeiswWidgetClass;

typedef struct _ExmSeiswClassRec *ExmSeiswWidgetClass;
typedef struct _ExmSeiswRec *ExmSeiswWidget;


/* Define an IsSubclass macro. */
#ifndef ExmIsSeisw
#define ExmIsSeisw(w) XtIsSubclass(w, exmSeiswWidgetClass)
#endif

/* Define string equivalents of new resource names. */
/* Peng Wang:
   We decide to define ensemble first, but not
   meta data resource for parameters in test.pf file.
   We put in defaults first since we don't know how
   much we actually needs.

   Not sure what we need to do to define a custom 
   representation type that is not enumerative. 

*/

#define ExmNseiswEnsemble "seiswEnsemble" 
#define ExmCSeiswEnsemble "SeiswEnsemble"
#define ExmNseiswMetadata "seiswMetadata"
#define ExmCSeiswMetadata "SeiswMetadata"
#define ExmNseiswParameters "seiswParameters"
#define ExmCSeiswParameters "SeiswParameters"
#define ExmNseiswCommonArea "seiswCommonArea"
#define ExmCSeiswCommonArea "SeiswCommonArea"
#define ExmNdefaultWidth "seiswDefaultWidth"
#define ExmCDefaultWidth "SeiswDefaultWidth"
#define ExmNdefaultHeight "seiswDefaultHeight"
#define ExmCDefaultHeight "SeiswDefaultHeight"
#define ExmNzoomFactor "seiswZoomFactor"
#define ExmCZoomFactor "SeiswZoomFactor"
#define ExmNseiswPick "seiswPick"
#define ExmCSeiswPick "SeiswPick"
#define ExmNcleanupData "seiswCleanupData"
#define ExmCCleanupData "SeiswCleanupData"
#define ExmNdisplayMarkers "seiswDisplayMarkers"
#define ExmCDisplayMarkers "SeiswDisplayMarkers"
#define ExmNdisplayAttributes "seiswDisplayAttributes"
#define ExmCDisplayAttributes "SeiswDisplayAttributes"
#define ExmNdeletedColor "seiswDeletedColor"
#define ExmCDeletedColor "SeiswDeletedColor"
#define ExmNeditEnable "seiswEditEnable"
#define ExmCEditEnable "SeiswEditEnable"
#define ExmNbtn2Callback "seiswBtn2Callback"
#define ExmNbtn3Callback "seiswBtn3Callback"
#define ExmNdisplayAttrCallback "seiswDisplayAttrCallback"
#define ExmNdisplayOnly "seiswDisplayOnly"
#define ExmCDisplayOnly "SeiswDisplayOnly"


/* Specify the API for this widget. */
extern Widget ExmCreateSeisw(Widget    parent,
                              char     *name,
                              Arg      *arglist,
                              Cardinal  argCount);


/* Allow for C++ compilation. */
#ifdef __cplusplus
} /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _ExmSeisw_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */

