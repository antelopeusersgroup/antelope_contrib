LIB=libseispp.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=metadata.h seispp.h ray1d.h vmodel.h
MAN3=metadata.3 GCLrays.3
ldlibs=$(DBLIBS) -lpfstream -lbrttutil -lgclgrid -lperf

include $(ANTELOPEMAKE)  	

OBJS=create_destroy.o gaps.o \
mdlist.o metadata.o mute.o operators.o pfstream_interface.o \
raytrace.o readwrite.o rotate_procedures.o rotation.o statics.o ttime.o vmod1d.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(CXXAR) $(CXXARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(CXXDLD) $(CXXDLDFLAGS) -o $(DLIB) $(OBJS) $(DLDLIBS) 

