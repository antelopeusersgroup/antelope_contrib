LIB=libmatlabprocessor.a
#DLIB=$(LIB:.a=$(DSUFFIX))
#BUNDLE=$(LIB:.a=.bundle)
INCLUDE=MatlabProcessor.h

MAN3=MatlabProcessor.3
DBG=-g

SUBDIR=/contrib
include $(ANTELOPEMAKE)

CXXFLAGS += -I$(MATLAB)/extern/include
LDLIBS += -L$(MATLAB)/bin/maci64 -R$(MATLAB)/bin/maci64 -leng -lmx -lut -lmat

OBJS=MatlabProcessor.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
