BIN   = nagios_check_orbstat
PMAN1 = $(BIN)

DATA = edu.ucsd.cmd.orbstat-exists edu.ucsd.cmd.orbstat-client
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA) *.pd.1 *.pd.3p

SUBDIR=/contrib
include $(ANTELOPEMAKE)

% : %.id
	getid < $@.id > $@
