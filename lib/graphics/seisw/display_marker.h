#ifndef __DISPLAY_MARKER_H

#define __DISPLAY_MARKER_H
using namespace SEISPP;

#include <string.h>
#include <TimeWindow.h>

#define ATTR_STR 0
#define ATTR_INT 1
#define ATTR_DOUBLE 2

typedef struct {
  TimeWindow beam_tw;
  string beam_color;
  TimeWindow robust_tw;
  string robust_color;
  string title;
} DisplayMarkerDataRec, *DisplayMarkerData;

typedef struct {
  float x2begb, x2endb;
  int *str_origin;
} DisplayAttributesRec, *DisplayAttributes;

typedef struct {
  Widget w;
  string name;
  unsigned short type;
  bool use_graph;
  Widget graph_widget;
  int line; //this is for SciPlot to keep track of the line #, although they are probably 0
  bool enabled;
  string display_name;
  bool available_before_analysis;
} AttributeInfoRec, *AttributeInfo;

#endif

