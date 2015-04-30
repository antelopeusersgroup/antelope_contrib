LIB = libtt1dcvl.a
DLIB=$(LIB:.a=$(DSUFFIX))

MAN3=tt1dcvl.3

ldlibs = $(DBLIBS) 

SUBDIR=/contrib
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
	$(RM) $@
	case `uname -s` in \
	    SunOS) $(CC) $(CONTRIBDLDFLAGS) -o $@ $(DLDLIBS) $(OBJS) ;; \
	    Linux) $(DLD) $(CONTRIBDLDFLAGS) -o $@ $(DLDLIBS) -Wl,-init,_tt1dcvl_init $(OBJS) ;; \
	    Darwin) $(DLD) $(CONTRIBDLDFLAGS) -o $@ $(DLDLIBS) $(OBJS) ;; \
	    *)  echo dynamic $(DLIB) not created: System type `uname -s` not recognized. ;;\
	esac

