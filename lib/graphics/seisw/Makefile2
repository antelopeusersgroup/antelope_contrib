LIB=libseisw.a
# DLIB=$(LIB:.a=$(DSUFFIX))
# BUNDLE=$(LIB:.a=.bundle) 
INCLUDE=Seisw.h SeismicPick.h xplot.h display_marker.h

#Needed eventually
#MAN3=

include $(ANTELOPEMAKE)  	
include $(ANTELOPEMAKE).local 

CXXFLAGS += $(XMOTIFINCLUDE)

OBJS=	SeismicPick.o Seisw.o colormap.o parameters.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS)  
	$(RANLIB) $@

