LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
DATADIR=schemas
DATA=PWMIG1.0
INCLUDE=gclgrid.h
MAN3=gclgrid.3
MAN5=		# man pages for file formats

cflags=-g

include $(ANTELOPEMAKE)  	


OBJS=	create_destroy.o dbsubs.o ellipsoid.o fme_interpolate.o method_subs.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
