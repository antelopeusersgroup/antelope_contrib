DLIB = libtclucp$(DSUFFIX)

cflags =  $(TCLINCLUDE)
ldlibs = -lrtucp $(TKLIBS) $(TCLLIBS) -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
DIRS=

OBJS=ucp_init.o
$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


install :: pkgIndex

pkgIndex : pkgIndex.tcl
	deposit  pkgIndex.tcl $(DEST)/lib/ucp
	        
