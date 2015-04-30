LIB=libpmel.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=pmel.h
MAN3=pmel.3
LICENSES=license_libpmel.txt
cflags=-g
ldflags=
ldlibs=

CLEAN = $(LICENSES)

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

$(LICENSES) : LICENSE
	cp $< $@
