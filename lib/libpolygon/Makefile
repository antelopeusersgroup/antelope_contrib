LIB=	libpolygon.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE=polygon.h

MAN3= \
	 distanceToPolygon.3 \
	 distance_to_polygon.3 \
	 inWhichPolygons.3 \
	 isGeographicallyInside.3 \
	 polycode.3 \
	 polygon.3 \
	 polytype.3 \
	 readPolygon.3 \
	 windrose.3 \
	 writePolygonData.3

#	 polygon.so.3 \

ldlibs=	$(DBLIBS)
cflags = -g

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=schema

OBJS=polygon.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(CONTRIBDLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(LDFLAGS) $(DLDLIBS) 
