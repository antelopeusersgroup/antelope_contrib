BIN=ucp2 check_pid 
PF=ucp2.pf

REQUIRE=
REQUIRE_DATA=tcl
MAN1= ucp2.1

CLEAN=tclIndex

TCL= daspar.tcl \
	dcpar.tcl \
	dccmd.tcl \
	gainpar.tcl \
	radcmd.tcl \
	showgain.tcl \
	showpar.tcl \
	util.tcl
DATADIR=tcl/ucp2
DATA=tclIndex $(TCL)

include $(ANTELOPEMAKE) 
DIRS=

tclIndex : $(TCL)
	tclindex $(TCL) > $@
