LIB=libseispp.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=metadata.h seispp.h dmatrix.h ray1d.h vmodel.h
MAN3=metadata.3 dmatrix.3 GCLrays.3

cflags=-g
ldlibs= -L/opt/SUNWspro/lib $(DBLIBS) -lpfstream -lbrttutil -ltrvltm  -lperf -lm 

include $(ANTELOPEMAKE)  	
#DLD=CC -G -Kpic
#CC=CC -Kpic
#AR=CC -xar
#AR=ar -c

OBJS=create_destroy.o dmatrix.o gaps.o \
mdlist.o metadata.o mute.o operators.o pfstream_interface.o \
raytrace.o readwrite.o rotate_procedures.o rotation.o statics.o ttime.o vmod1d.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
	#CC -G -Kpic $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
