LIB=libmatlabprocessor.a
#DLIB=$(LIB:.a=$(DSUFFIX))
#BUNDLE=$(LIB:.a=.bundle)
INCLUDE=MatlabProcessor.h

MAN3=MatlabProcessor.3
DBG=-g

include $(ANTELOPEMAKE)  
include $(ANTELOPEMAKE).local
CXXFLAGS += $(MATLABINCLUDE)

OBJS=MatlabProcessor.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS)  
	$(RANLIB) $@

