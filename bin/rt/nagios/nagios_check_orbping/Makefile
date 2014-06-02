BIN = nagios_check_orbping
MAN1 = nagios_check_orbping.1
DATA = edu.ucsd.cmd.orbstat-ping
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA)

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

% : %.id
	getid < $@.id > $@
