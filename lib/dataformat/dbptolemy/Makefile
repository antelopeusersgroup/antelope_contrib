
LIB= libdbxml.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE= dbxml.h

MAN3= db2xml.3

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= db2xml.o xml2db.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
