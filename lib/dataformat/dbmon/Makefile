
LIB= libdbmon.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE= dbmon.h

MAN3= dbmon.3

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= dbmon.o 

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
