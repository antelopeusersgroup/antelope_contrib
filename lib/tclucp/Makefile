DLIB = libtclucp$(DSUFFIX)

cflags =  $(TCLINCLUDE)
ldlibs = -lrtucp $(TKLIBS) $(TCLLIBS) -ldefunctpkt -lorb $(TRLIBS) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=ucp_init.o
$(DLIB) : $(OBJS)
	$(DLD) $(CONTRIBDLDFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


install :: pkgIndex

pkgIndex : pkgIndex.tcl
	deposit  pkgIndex.tcl $(DEST)/lib/ucp
	        
