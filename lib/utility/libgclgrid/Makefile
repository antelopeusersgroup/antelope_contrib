LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h dmatrix.h
MAN3=gclgrid.3 dmatrix.3 pathintegral.3 ustrans.3 extract_gridline.3

#  seispp dependency needed for pathintegral.o and ustrans.o
#
#ldlibs= $(DBLIBS) -lperf -lseispp 
ldlibs= $(DBLIBS) -lperf 

include $(ANTELOPEMAKE)  	
#
#  essential for Solaris.  Will break under linux
#  -Kpic is essential for Solaris to make templated funtions (in this 
#  case the stdlib vector object) work.  Links against this library
#  in solaris fail without this.
#
DLD=CC -G -Kpic

OBJS=converters.o create_destroy.o dbsubs.o ellipsoid.o error_handlers.o \
    fme_weights.o interpolate.o lookup.o operators.o  \
    dmatrix.o extract_gridline.o pathintegral.o ustrans.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
