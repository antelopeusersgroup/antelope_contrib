#INCLUDE=
LIB = libttregions.a
DLIB=$(LIB:.a=$(DSUFFIX))

#$MAN3= 
$REQUIRE_DATA = models0.4
$DATADIR = $(ANTELOPE)/data/schemas2

ldlibs=-lresponse -lcoords -ltt -lstock -lposix4 -ldl -lgenloc -ltt -lm -lds

include $(ANTELOPEMAKE)
DIRS=

OBJS=ttregions.o winding.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

        
$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -L$(ANTELOPE)/lib -lcoords -lstock -lposix4 -lm -o $@ $(LORDER) $(OBJS) $(TSORT)


# $Id$ 
