
LIB= libdb2sql.a
DLIB=$(LIB:.a=$(DSUFFIX))
INCLUDE= db2sql.h

MAN3= db2sql.3

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= db2sql.o 

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
