LIB = libttregions.a
DLIB=$(LIB:.a=$(DSUFFIX))

DATA=
DATADIR = 

ldlibs=$(DBLIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS=ttregions.o winding.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

        
$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -L$(ANTELOPE)/lib $(LDLIBS) -o $@ $(LORDER) $(OBJS) $(TSORT)


# $Id$ 
