BIN = nagios_check_orblag
MAN1 = nagios_check_orblag.1
DATA = edu.ucsd.cmd.orblag
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA)

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

% : %.id
	getid < $@.id > $@
