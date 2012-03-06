LIB=libseispp.a
# DLIB=$(LIB:.a=$(DSUFFIX)) 
# BUNDLE=$(LIB:.a=.bundle)
DIRS=htdocs
INCLUDE=ArrivalUpdater.h \
  AttributeCrossReference.h \
  AttributeMap.h \
  BasicTimeSeries.h\
  ComplexTimeSeries.h\
  EventCatalog.h \
  FixedFormatTrace.h \
  GenericFileHandle.h \
  HeaderMap.h \
  Hypocenter.h\
  Metadata.h\
  MultichannelCorrelator.h\
  ProcessingQueue.h \
  SeisppError.h\
  SeisppKeywords.h \
  SignalToNoise.h \
  SimpleWavelets.h \
  SphericalCoordinate.h\
  StationChannelMap.h\
  ThreeComponentChannelMap.h\
  ThreeComponentSeismogram.h\
  TimeSeries.h\
  TimeVariableWeight.h\
  TimeWindow.h\
  VectorStatistics.h\
  VelocityModel_1d.h\
  XcorAnalysisSetting.h \
  XcorProcessingEngine.h \
  databasehandle.h\
  dbpp.h\
  ensemble.h\
  filter++.h\
  interpolator1d.h\
  mute.h\
  ray1d.h\
  resample.h\
  seismicarray.h\
  seispp.h\
  slowness.h\
  stack.h
# This is uses for Attribute_Map constructor and was built with 
# an external program
PF=seispp_attribute_maps.pf \
  metadata_defaults.pf \
  HeaderMap.pf
MAN3=Basic_Time_Series.3 \
  Datascope_Handle.3 \
  GCLrays.3 \
  Three_Component_Seismogram.3 \
  Time_Series.3 \
  interpolate1d.3 \
  Metadata.3 \
  metadata_error.3 \
  resample.3 \
  slowness_object.3 \
  seispp_error.3 \
  Top_Mute.3
DBG=-g

include $(ANTELOPEMAKE)  	

OBJS=ArrivalUpdater.o \
  AttributeCrossReference.o \
  AttributeMap.o \
  BasicTimeSeries.o \
  ComplexTimeSeries.o \
  EventCatalog.o \
  FixedFormatTrace.o  \
  GenericFileHandle.o \
  HeaderMap.o \
  Hypocenter.o \
  Metadata.o \
  MultichannelCorrelator.o \
  ProcessingQueue.o \
  SignalToNoise.o \
  SimpleWavelets.o \
  StationChannelMap.o \
  ThreeComponentChannelMap.o \
  ThreeComponentSeismogram.o \
  TimeSeries.o \
  VelocityModel_1d.o \
  VelocityModel_3d.o \
  XcorAnalysisSetting.o \
  XcorProcessingEngine.o \
  array_get_data_3c.o \
  array_get_data.o \
  byteswap.o \
  converters.o \
  correlation.o \
  dbpp.o \
  dbpp_matchhandle.o \
  ensemble.o \
  ensemble_helpers.o \
  filter.o \
  interpolator1d.o \
  mdlist.o \
  mute.o \
  nint.o \
  pfstream_interface.o \
  ray1d.o \
  readwrite.o \
  resample.o \
  rotate_procedures.o \
  seismicarray.o \
  slice_and_dice.o \
  slowness.o \
  sparse_convolve.o \
  sort_ensembles.o \
  spherical_coordinate.o \
  stack.o \
  statics.o \
  three_component_helpers.o \
  tseries_helpers.o \
  dsseis.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR)  $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

#$(DLIB) : $(OBJS)
#	$(RM) $@
#	$(CXXDLD)  $(CXXDLDFLAGS) -o $(DLIB) $(OBJS) $(DLDLIBS) 
#$(BUNDLE) : $(OBJS)
#	$(RM) $@
#	$(DLD)  -flat_namespace -bundle -undefined suppress -o $@   $(OBJS) $(LDFLAGS) $(DLDLIBS)
