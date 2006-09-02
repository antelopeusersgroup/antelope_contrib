/*----------------------------------------------------------------------------
 * SciPlot	A generalized plotting widget
 *
 * Copyright (c) 1996 Robert W. McMullen
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * Author: Rob McMullen <rwmcm@mail.ae.utexas.edu>
 *         http://www.ae.utexas.edu/~rwmcm
 */

#ifndef _SCIPLOT_H
#define _SCIPLOT_H

#ifdef __cplusplus
extern "C" 
{
#endif
  
#include <X11/Core.h>

#define _SCIPLOT_WIDGET_VERSION	1.36

#ifndef XtIsSciPlot
#define XtIsSciPlot(w) XtIsSubclass((Widget)w, sciplotWidgetClass)
#endif 

#include <math.h>
#include <float.h>
  /* NOTE:  float.h is required by POSIX */
#define SCIPLOT_SKIP_VAL (-FLT_MAX)

typedef float real;

typedef struct {
	real x,y;
} realpair;


#define XtNchartType	"chartType"
#define XtNdegrees	"degrees"
#define XtNdefaultMarkerSize "defaultMarkerSize"
#define XtNdrawMajor	"drawMajor"
#define XtNdrawMajorTics	"drawMajorTics"
#define XtNdrawMinor	"drawMinor"
#define XtNdrawMinorTics	"drawMinorTics"
#define XtNxAutoScale	"xAutoScale"
#define XtNyAutoScale	"yAutoScale"
#define XtNxAxisNumbers	"xAxisNumbers"
#define XtNyAxisNumbers	"yAxisNumbers"
#define XtNxLog		"xLog"
#define XtNyLog		"yLog"
#define XtNxOrigin	"xOrigin"
#define XtNyOrigin	"yOrigin"
#define XtNxLabel	"xLabel"
#define XtNyLabel	"yLabel"
#define XtNplotTitle	"plotTitle"
#define XtNmargin	"margin"
#define XtNmonochrome	"monochrome"
#define XtNtitleMargin	"titleMargin"
#define XtNshowLegend	"showLegend"
#define XtNshowTitle	"showTitle"
#define XtNshowXLabel	"showXLabel"
#define XtNshowYLabel	"showYLabel"
#define XtNlegendLineSize	"legendLineSize"
#define XtNlegendMargin	"legendMargin"
#define XtNlegendThroughPlot	"legendThroughPlot"
#define XtNtitleFont	"titleFont"
#define XtNlabelFont	"labelFont"
#define XtNaxisFont	"axisFont"
#define XtNyNumbersHorizontal	"yNumbersHorizontal"

#define XtPOLAR		0
#define XtCARTESIAN	1

#define XtMARKER_NONE		0
#define XtMARKER_CIRCLE		1
#define XtMARKER_SQUARE		2
#define XtMARKER_UTRIANGLE	3
#define XtMARKER_DTRIANGLE	4
#define XtMARKER_LTRIANGLE	5
#define XtMARKER_RTRIANGLE	6
#define XtMARKER_DIAMOND	7
#define XtMARKER_HOURGLASS	8
#define XtMARKER_BOWTIE		9
#define XtMARKER_FCIRCLE	10
#define XtMARKER_FSQUARE	11
#define XtMARKER_FUTRIANGLE	12
#define XtMARKER_FDTRIANGLE	13
#define XtMARKER_FLTRIANGLE	14
#define XtMARKER_FRTRIANGLE	15
#define XtMARKER_FDIAMOND	16
#define XtMARKER_FHOURGLASS	17
#define XtMARKER_FBOWTIE	18
#define XtMARKER_DOT		19

#define XtFONT_SIZE_MASK	0xff
#define XtFONT_SIZE_DEFAULT	12

#define XtFONT_NAME_MASK	0xf00
#define XtFONT_TIMES		0x000
#define XtFONT_COURIER		0x100
#define XtFONT_HELVETICA	0x200
#define XtFONT_LUCIDA		0x300
#define XtFONT_LUCIDASANS	0x400
#define XtFONT_NCSCHOOLBOOK	0x500
#define XtFONT_NAME_DEFAULT	XtFONT_TIMES

#define XtFONT_ATTRIBUTE_MASK	0xf000
#define XtFONT_BOLD		0x1000
#define XtFONT_ITALIC		0x2000
#define XtFONT_BOLD_ITALIC	0x3000
#define XtFONT_ATTRIBUTE_DEFAULT 0


#define XtLINE_NONE	0
#define XtLINE_SOLID	1
#define XtLINE_DOTTED	2
#define XtLINE_WIDEDOT	3
#define XtLINE_USERDASH	4



extern WidgetClass sciplotWidgetClass;

typedef struct _SciPlotClassRec *SciPlotWidgetClass;
typedef struct _SciPlotRec      *SciPlotWidget;


/*
** Public function declarations
*/

/* Old compatibility functions */
#define SciPlotListCreateFromFloat SciPlotListCreateFloat
#define SciPlotListUpdateFromFloat SciPlotListUpdateFloat
#define SciPlotListCreateFromDouble SciPlotListCreateDouble
#define SciPlotListUpdateFromDouble SciPlotListUpdateDouble
/* Stock version of this include had an odd define to handle
non-ansi compilers.  Assume this is archaic are remove.
glp  Sept. 2, 2006 */

/* SciPlot.c */
Boolean SciPlotPSCreate (Widget wi, char *filename);
Boolean SciPlotPSCreateColor (Widget wi, char *filename);
int SciPlotAllocNamedColor (Widget wi, char *name);
int SciPlotAllocRGBColor (Widget wi, int r, int g, int b);
void SciPlotSetBackgroundColor (Widget wi, int color);
void SciPlotSetForegroundColor (Widget wi, int color);
void SciPlotListDelete (Widget wi, int idnum);
int SciPlotListCreateFromData (Widget wi, int num, real *xlist, real *ylist, char *legend, int pcolor, int pstyle, int lcolor, int lstyle);
int SciPlotListCreateFloat (Widget wi, int num, float *xlist, float *ylist, char *legend);
void SciPlotListUpdateFloat (Widget wi, int idnum, int num, float *xlist, float *ylist);
void SciPlotListAddFloat (Widget wi, int idnum, int num, float *xlist, float *ylist);
int SciPlotListCreateDouble (Widget wi, int num, double *xlist, double *ylist, char *legend);
void SciPlotListUpdateDouble (Widget wi, int idnum, int num, double *xlist, double *ylist);
void SciPlotListAddDouble (Widget wi, int idnum, int num, double *xlist, double *ylist);
void SciPlotListSetStyle (Widget wi, int idnum, int pcolor, int pstyle, int lcolor, int lstyle);
void SciPlotListSetMarkerSize (Widget wi, int idnum, float size);
void SciPlotSetXAutoScale (Widget wi);
void SciPlotSetXUserScale (Widget wi, double min, double max);
void SciPlotSetYAutoScale (Widget wi);
void SciPlotSetYUserScale (Widget wi, double min, double max);
void SciPlotPrintStatistics (Widget wi);
void SciPlotExportData (Widget wi, FILE *fd);
void SciPlotUpdate (Widget wi);
Boolean SciPlotQuickUpdate (Widget wi);

#ifdef __cplusplus
};
#endif

#endif /* _SCIPLOT_H */
