LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h 
MAN3=gclgrid.3 pathintegral.3 ustrans.3 extract_gridline.3

ldlibs= $(DBLIBS) -lperf -lseispp

include $(ANTELOPEMAKE)  	

OBJS=converters.o create_destroy.o dbsubs.o ellipsoid.o error_handlers.o \
    extract_gridline.o fme_weights.o interpolate.o lookup.o operators.o \
    pathintegral.o ustrans.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
