LIB=libseispp.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
DIRS=htdocs
INCLUDE=dbpp.h filter++.h interpolator1d.h metadata.h seispp.h stack.h ray1d.h resample.h 
# This is uses for Attribute_Map constructor and was built with 
# an external program
PF=seispp_attribute_maps.pf metadata_defaults.pf
MAN3=Basic_Time_Series.3 \
  Datascope_Handle.3 \
  GCLrays.3 \
  Three_Component_Seismogram.3 \
  Time_Series.3 \
  interpolate1d.3 \
  metadata.3 \
  metadata_error.3 \
  resample.3 \
  slowness_object.3 \
  seispp_error.3 
ldlibs=$(DBLIBS) -lpfstream -lbrttutil -lgclgrid -lperf
DBG=-g

include $(ANTELOPEMAKE)  	

OBJS=basic_time_series.o \
  create_destroy.o \
  converters.o \
  dbpp.o \
  dsseis.o \
  filter.o \
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
  slice_and_dice.o \
  slowness.o \
  spherical_coordinate.o \
  stack.o \
  statics.o \
  three_component_helpers.o \
  tseries_helpers.o \
  time_window.o \
  timewindow_reader.o \
  ttime.o \
  vmod1d.o
# not included for now
#  pfstream_interface.o 

# include dependencies
filter.o : filter++.h
stack.o : stack.h
$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR)  $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(CXXDLD)  $(CXXDLDFLAGS) -g -o $(DLIB) $(OBJS) $(DLDLIBS) 
# New special build for html documentation using ccdoc
CCDOC=ccdoc.exe
htdocs/seispp.db : htdocs seispp.h dbpp.h metadata.h ray1d.h resample.h
	$(CCDOC) -db htdocs/seispp.db -pkg seispp seispp.h
	$(CCDOC) -db htdocs/seispp.db -pkg dbpp dbpp.h
	$(CCDOC) -db htdocs/seispp.db -pkg metadata metadata.h
	$(CCDOC) -db htdocs/seispp.db -pkg ray1d ray1d.h
	$(CCDOC) -db htdocs/seispp.db -pkg resample resample.h
	$(CCDOC) -db htdocs/seispp.db -pkg filter++ filter++.h
	$(CCDOC) -db htdocs/seispp.db -pkg stack stack.h
	$(CCDOC) -db htdocs/seispp.db -index
	$(CCDOC) -db htdocs/seispp.db -html htdocs/ \
		-root seispp -rootfile htdocs/index.html
#htdocs : ; @mkdir $@
