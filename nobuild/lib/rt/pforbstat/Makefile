LIB= libpforbstat.a
DLIB=$(LIB:.a=$(DSUFFIX))
DATADIR=
DATA=
INCLUDE= pforbstat.h
PF= libpforbstat.pf

MAN3= pforbstat.3

CLEAN=

cflags= 
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)

DIRS=
DBG= -g

OBJS= pforbstat.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
