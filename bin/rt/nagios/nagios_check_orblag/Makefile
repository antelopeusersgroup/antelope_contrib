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

%.1 : %
	pod2man -s 1 -c "User Commands" -r "Antelope Contrib SW" $< $@

$(DEST)/man/man1/% : %
	install $< $(DEST)/man/man1
