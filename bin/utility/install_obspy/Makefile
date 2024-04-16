BIN= install_obspy
MAN1= install_obspy.1

CLEAN= $(MAN1) $(BIN).xpl

SUBDIR=/contrib
include $(ANTELOPEMAKE)

$(BIN).xpl: $(BIN).xpl.id
	produce $@

$(MAN1): $(MAN1).id
	produce $@
