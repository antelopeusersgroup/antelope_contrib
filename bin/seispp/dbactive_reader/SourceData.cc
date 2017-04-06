#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include "coords.h"
#include "SourceData.h"
#include "seispp.h"
using namespace std;
SourceData::SourceData(const char* inp)
{
  stringstream ss(inp);
  /* Note this blindly assumes time is set as an epoch time.   The
  test is used to handle this as this could easily happen. */
  ss >> time;
  ss >> ffid;
  if(ss.fail())
    throw SeisppError(string("SourceData constructor:  format error.\n")
                    + "Time field of input stream must be a epoch time.");
  /* Note these are assumed in degrees.   We convert the to radians to mesh
  with RegionalCoordinates */
  ss >> lat;
  ss >> lon;
  ss >> elev;
  lat=rad(lat);
  lon=rad(lon);
}
