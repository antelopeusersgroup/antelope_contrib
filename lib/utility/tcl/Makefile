DATADIR=tcl/library_contrib
MAN3T= \
    CommandCheckoff.3t 

DATA=$(MAN3T:.3t=.tcl) 
LICENSES = license_CommandCheckoff.txt

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=
			
install :: $(ANTELOPE)/data/$(DATADIR)/tclIndex 

tclIndex : $(DATA) 
	tclindex $(DATA) > $@

$(LICENSES) : LICENSE
	cp $< $@
