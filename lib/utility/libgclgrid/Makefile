LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
DATADIR=schemas
#DATA=PWMIG1.0
INCLUDE=gclgrid.h
MAN3=gclgrid.3
MAN5=		# man pages for file formats
CC=CC -Kpic
AR=CC -xar

cflags=-g
#ldlibs=$(DBLIBS) -library=sunperf
#ldlibs= $(DBLIBS) -xlic_lib=sunperf
ldlibs= -L/opt/SUNWspro/lib $(DBLIBS) -lperf

include $(ANTELOPEMAKE)  	
DLD=CC -G -Kpic


OBJS=	converters.o create_destroy.o dbsubs.o ellipsoid.o \
    fme_weights.o interpolate.o lookup.o operators.o
$(LIB) : $(OBJS)
	rm -f $@
#	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(AR) -o $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
	#CC -G -Kpic $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
