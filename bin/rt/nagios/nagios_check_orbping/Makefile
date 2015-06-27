BIN  = nagios_check_orbping
MAN1 = $(BIN).1

DATA = edu.ucsd.cmd.orbstat-ping
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

% : %.id
	getid < $@.id > $@
