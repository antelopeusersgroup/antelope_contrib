BIN = nagios_check_orblag
PMAN1 = $(BIN)
DATA = edu.ucsd.cmd.orblag
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA) *.pd.1 *.pd.3p

SUBDIR=/contrib
include $(ANTELOPEMAKE)

% : %.id
	getid < $@.id > $@
