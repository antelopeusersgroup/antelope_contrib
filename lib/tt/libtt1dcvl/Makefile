#INCLUDE=
LIB = libtt1dcvl.a
DLIB=$(LIB:.a=$(DSUFFIX))

#$MAN3= 
#$REQUIRE_DATA = taup_ttimes
#$DATADIR = tables

ldlibs=-lresponse -lcoords -ltt -lstock -lposix4 -ldl -lgenloc -ltt -lm -lds

include $(ANTELOPEMAKE)
DIRS=

OBJS=tt1dcvl.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

        
$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -L$(ANTELOPE)/lib $(ldlibs) -o $@ $(LORDER) $(OBJS) $(TSORT)
