LIB = libtt1dcvl.a
DLIB=$(LIB:.a=$(DSUFFIX))

DATADIR=tables/genloc/db
#
#  only the mod1d table is used by tt1dcvl.  ttregions adds two more
#  this is a maintenance problem, but better left this way for now.
#
DATA=vmodel vmodel.mod1d
MAN3=tt1dcvl.3

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
