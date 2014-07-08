BIN = nagios_check_q330_ping
PMAN1 = $(BIN)
DATA = edu.ucsd.cmd.q330ping
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA) *.pd.1 *.pd.3p

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)
include ../perldoc.mk

% : %.id
	getid < $@.id > $@
