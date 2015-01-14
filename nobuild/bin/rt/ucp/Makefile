BIN=ucp check_pid 
PF=ucp.pf

REQUIRE=
REQUIRE_DATA=tcl
MAN1= ucp.1

CLEAN=tclIndex

TCL= anzadp.tcl \
	daspar.tcl \
	dcpar.tcl \
	dccmd.tcl \
	gainpar.tcl \
	radcmd.tcl \
	showanza.tcl \
	showgain.tcl \
	showpar.tcl \
	util.tcl
DATADIR=tcl/ucp
DATA=tclIndex $(TCL)

SUBDIR=/contrib
include $(ANTELOPEMAKE) 
DIRS=

tclIndex : $(TCL)
	tclindex $(TCL) > $@
