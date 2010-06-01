DLIB=libmex_orb.$(MATLAB_RELEASE)$(DSUFFIX)
INCLUDE=mex_orb.h

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/local/include/mex_antelope_makerules
OBJS= mex_orb.o

$(DLIB) : $(OBJS) dsymutil_dir
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

dsymutil_dir:
	mkdir -p $(ANTELOPE)/dsymutil
