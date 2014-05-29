BIN = nagios_check_orblag
MAN1 = nagios_check_orblag.1
DATA = edu.ucsd.cmd.orblag
DATADIR = intermapper/probes

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

edu.ucsd.cmd.orblag: edu.ucsd.cmd.orblag.id
	getid < $@.id > $@
