LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h
MAN3=gclgrid.3

ldlibs= $(DBLIBS) -lperf

include $(ANTELOPEMAKE)  	

OBJS=	converters.o create_destroy.o dbsubs.o ellipsoid.o \
    fme_weights.o interpolate.o lookup.o operators.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
