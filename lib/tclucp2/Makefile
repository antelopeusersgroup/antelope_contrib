DLIB = libtclucp2$(DSUFFIX)

cflags =  $(TCLINCLUDE)
ldlibs = -lrtucp2 $(TKLIBS) $(TCLLIBS) $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=ucp2_init.o
$(DLIB) : $(OBJS)
	$(DLD) $(CONTRIBDLDFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


install :: pkgIndex

pkgIndex : pkgIndex.tcl
	deposit  pkgIndex.tcl $(DEST)/lib/ucp2
	        
