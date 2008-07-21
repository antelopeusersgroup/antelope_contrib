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

        
$(DLIB) : $(OBJS) _init.o
	$(RM) $@
	case `uname -s` in \
	    SunOS) $(CC) $(DLDFLAGS) -o $@ $(DLDLIBS) $(OBJS) ;; \
	    Linux) $(DLD) $(DLDFLAGS) -o $@ $(DLDLIBS) $(OBJS) _init.o ;; \
	    Darwin) $(DLD) $(DLDFLAGS) -o $@ -init _tt1dcvl_init $(DLDLIBS) $(OBJS) ;; \
	    *)  echo dynamic $(DLIB) not created: System type `uname -s` not recognized. ;;\
	esac

