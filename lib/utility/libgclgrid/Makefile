LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h 
MAN3=gclgrid.3 pathintegral.3 ustrans.3 extract_gridline.3

ldlibs= $(DBLIBS) -lperf

include $(ANTELOPEMAKE)  	

#    fme_weights.o GCLproject_ray.o interpolate.o lookup.o operators.o \
CXX=g++
OBJS=converters.o create_destroy.o dbsubs.o ellipsoid.o \
    extract_gridline.o interpolate.o lookup.o operators.o \
    pathintegral.o ustrans.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
