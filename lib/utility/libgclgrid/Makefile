LIB=libgclgrid.a

DIRS=htdocs
DATADIR=schemas

INCLUDE=gclgrid.h dmatrix.h
MAN3=gclgrid.3 dmatrix.3 pathintegral.3 ustrans.3 extract_gridline.3

CXX=g++ -shared


include $(ANTELOPEMAKE)  	

OBJS=converters.o \
    create_destroy.o \
    dbsubs.o \
    dx_output.o \
    ellipsoid.o \
    extract_component.o \
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
    remapgrid.o \
    ustrans.o \
    zero.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

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



