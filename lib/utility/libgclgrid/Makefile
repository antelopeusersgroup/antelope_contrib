LIB=libgclgrid.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

DIRS=htdocs
DATADIR=schemas
#DATA=PWMIG1.0

INCLUDE=gclgrid.h dmatrix.h
MAN3=gclgrid.3 dmatrix.3 pathintegral.3 ustrans.3 extract_gridline.3

#  seispp dependency needed for pathintegral.o and ustrans.o
#
#ldlibs= $(PERFLIBS) $(DBLIBS) -lm

#include $(ANTELOPEMAKE)  	
include /opt/antelope/4.6/include/antelopemake.solaris
#CXX=g++ -g

OBJS=converters.o \
    create_destroy.o \
    dbsubs.o \
    dx_output.o \
    ellipsoid.o \
    error_handlers.o \
    flatten.o \
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
# New special build for html documentation using ccdoc
CCDOC=ccdoc.exe
htdocs/gclgrid.db : htdocs gclgrid.h dmatrix.h
	$(CCDOC) -db htdocs/gclgrid.db \
		-pkg GCLgrid gclgrid.h
	$(CCDOC) -db htdocs/gclgrid.db \
		-pkg dmatrix dmatrix.h
	$(CCDOC) -db htdocs/gclgrid.db \
		-index 
	$(CCDOC) -db htdocs/gclgrid.db \
		-html htdocs/ -root GCLgrid -rootfile htdocs/index.html

htdocs : ; @mkdir $@



