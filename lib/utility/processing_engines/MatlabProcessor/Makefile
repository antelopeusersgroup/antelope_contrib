LIB=libmatlabprocessor.a
#DLIB=$(LIB:.a=$(DSUFFIX))
#BUNDLE=$(LIB:.a=.bundle)
INCLUDE=MatlabProcessor.h

MAN3=MatlabProcessor.3
DBG=-g
#
#  This needs to be set to the root location for matlab's extern
#  directory.  They keep the external interface definitions
#  under an include directory and the library directory is lib
#
MATLAB=/opt/matlab6p1/extern
cflags=-I$(MATLAB)/include
#
#  To link with this library the following is required:
#
#ldlibs= -L$(MATLAB)/lib/sol2 -R$(MATLAB)/lib/sol2 -lmatlabprocessor -lgclgrid -lseispp -leng -lmx -lut -lmat $(TRLIBS) -lperf


include $(ANTELOPEMAKE)  

OBJS=MatlabProcessor.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS)  
	$(RANLIB) $@

#$(DLIB) : $(OBJS)
#	$(RM) $@
#	$(CXXDLD) $(CXXDLDFLAGS) -o $(DLIB) $(OBJS) $(DLDLIBS) 


#$(BUNDLE) : $(OBJS)
#	$(RM) $@
#	$(DLD)  -flat_namespace -bundle -undefined suppress -o $@   $(OBJS) #$(LDFLAGS) $(DLDLIBS)
