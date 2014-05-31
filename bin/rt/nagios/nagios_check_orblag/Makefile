BIN = nagios_check_orblag
PMAN1 = nagios_check_orblag.xpls
DATA = edu.ucsd.cmd.orblag
DATADIR = intermapper/probes
CLEAN = $(BIN) $(DATA) *.pd.1 *.pd.3p

ANTELOPEMAKELOCAL = $(ANTELOPE)/local/include/antelopemake.local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

% : %.id
	getid < $@.id > $@

ALL : $(PMAN1:%.xpls=%.pd.1) $(PMAN1:%.xpl=%.pd.1) $(PMAN3:%.pm=%.pd.3p)

MAN :: $(PMAN1:%.xpls=$(DEST)/man/man1/%.1)
MAN :: $(PMAN1:%.xpl=$(DEST)/man/man1/%.1)
MAN :: $(PMAN3:%.pm=$(DEST)/man/man3/%.3p)

%.pd.1 : %.xpls
	pod2man -n $* -s 1 -c "User Commands" -r "Antelope Contrib SW" $< $@

%.pd.1 : %.xpl
	pod2man -n $* -s 1 -c "User Commands" -r "Antelope Contrib SW" $< $@

%.pd.3p : %.pm
	pod2man -n $* -s 3p -c "Perl Extensions Commands" -r "Antelope Contrib SW" $< $@

$(DEST)/man/man1/%.1 : %.pd.1
	install $< $@

$(DEST)/man/man3/%.3p : %.pd.3p
	install $< $@

