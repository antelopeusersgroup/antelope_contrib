BIN = nagios_check_dlcmdping
PMAN1 = $(BIN)
DATA = edu.ucsd.cmd.dlcmd-ping
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA) *.pd.1 *.pd.3p

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)
include ../perldoc.mk

% : %.id
	getid < $@.id > $@
