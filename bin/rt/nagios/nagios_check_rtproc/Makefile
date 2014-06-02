BIN = nagios_check_rtproc
PMAN1 = $(BIN)
DATA = edu.ucsd.cmd.rtprocs-nrpe
DATADIR = intermapper/probes
CLEAN = $(BIN) *.pd.1 *.pd.3p

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)
include ../perldoc.mk
