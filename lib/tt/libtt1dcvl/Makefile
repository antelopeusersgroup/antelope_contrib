LIB = libtt1dcvl.a
DLIB=$(LIB:.a=$(DSUFFIX))

MAN3=tt1dcvl.3

ldlibs = $(DBLIBS) 

include $(ANTELOPEMAKE)
DIRS=

OBJS=tt1dcvl.o ttlvz.o

ttlvz.o : ttlvz.c
	$(CC) -c $(CFLAGS) ttlvz.c

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

        
$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(ldlibs) -o $@ $(LORDER) $(OBJS) $(TSORT)
