LIB= libpfxml.a
DLIB=$(LIB:.a=$(DSUFFIX))
DATADIR=
DATA=
INCLUDE= pfxml.h
PF=

MAN3= pf2xml.3	

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)

DIRS=
DBG= -g

OBJS= pfxml.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
