LIB=libseispp.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=dbpp.h interpolator1d.h metadata.h seispp.h ray1d.h resample.h 
# This is uses for Attribute_Map constructor and was built with 
# an external program
PF=seispp_attribute_maps.pf
MAN3=Basic_Time_Series.3 \
  Datascope_Handle.3 \
  GCLrays.3 \
  Three_Component_Seismogram.3 \
  Time_Series.3 \
  interpolate1d.3 \
  metadata.3 \
  metadata_error.3 \
  resample.3 \
  slowness.3 \
  seispp_error.3 
#ldlibs=$(DBLIBS) -lpfstream -lbrttutil -lgclgrid -lperf
DBG=-g

include $(ANTELOPEMAKE)  	

OBJS=basic_time_series.o \
  create_destroy.o \
  converters.o \
  dbpp.o \
  freetran.o \
  gaps.o \
  interpolate.o \
  mdlist.o \
  metadata.o \
  mute.o \
  operators.o \
  raytrace.o \
  readwrite.o \
  resample.o \
  rotate_procedures.o \
  rotation.o \
  slowness.o \
  spherical_coordinate.o \
  statics.o \
  three_component_helpers.o \
  ttime.o \
  vmod1d.o
# not included for now
#  pfstream_interface.o 

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR)  $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(CXXDLD)  $(CXXDLDFLAGS) -o $(DLIB) $(OBJS) $(DLDLIBS) 

