DLIB=libmex_orb$(DSUFFIX)
INCLUDE=mex_orb.h

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/include/mex_antelope_makerules
OBJS= mex_orb.o

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
