LIB=libpmel.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=pmel.h
MAN3=pmel.3
cflags=-g
ldflags=
ldlibs=

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=fixlist_utilities.o phase_handle_utilities.o \
   pmel.o scmatrixsubs.o station.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
