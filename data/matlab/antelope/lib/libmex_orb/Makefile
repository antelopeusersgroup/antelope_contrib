INCLUDE=mex_orb.h

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include ../../antelope_mex_make
OBJS= mex_orb.o

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
