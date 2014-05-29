BIN = nagios_check_orbstat
MAN1 = nagios_check_orbstat.1
DATA = edu.ucsd.cmd.orbstat-exists
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA)

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

% : %.id
	getid < $@.id > $@
