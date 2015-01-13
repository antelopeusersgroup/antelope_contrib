
LIB= libdbptolemy.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE= dbptolemy.h

MAN3= db2ptolemy.3

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= db2ptolemy.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
