LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h dmatrix.h
MAN3=gclgrid.3 dmatrix.3 pathintegral.3 ustrans.3 extract_gridline.3

#  seispp dependency needed for pathintegral.o and ustrans.o
#
ldlibs= $(PERFLIBS) $(DBLIBS) -lm

include $(ANTELOPEMAKE)  	

OBJS=converters.o \
    create_destroy.o \
    dbsubs.o \
    ellipsoid.o \
    error_handlers.o \
    fme_weights.o \
    initialize1d.o \
    interpolate.o \
    lookup.o \
    operators.o  \
    dmatrix.o \
    extract_gridline.o \
    pathintegral.o \
    ustrans.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(CXXDLD) $(CXXDLDFLAGS) -o $(DLIB) $(OBJS) $(DLDLIBS) 



