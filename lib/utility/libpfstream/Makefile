LIB=libpfstream.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
INCLUDE=pfstream.h
MAN3=pfstream.3
MAN5=pfstream.5

#ldlibs= -lbrttutil $(THREAD) $(DBLIBS) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	


OBJS=pfstream.o pfget_Attribute_map.o pthread_routines.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
