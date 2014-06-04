# This Makefile is intended to be included after $(ANTELOPEMAKE) in a master
# Makefile. It generates Antelope style man pages from Perl code
ALL : $(PMAN1:%.xpls=%.pd.1) $(PMAN1:%.xpl=%.pd.1) $(PMAN1:%=%.pd.1)
ALL : $(PMAN3:%.pm=%.pd.3p)

MAN :: $(PMAN1:%=$(DEST)/man/man1/%.1)
MAN :: $(PMAN1:%.xpls=$(DEST)/man/man1/%.1)
MAN :: $(PMAN1:%.xpl=$(DEST)/man/man1/%.1)
MAN :: $(PMAN3:%.pm=$(DEST)/man/man3/%.3p)

PODARGS1=-s 1 -c "User Commands" -r "Antelope Contrib SW"
PODARGS3=-s 3p -c "Perl Extensions Commands" -r "Antelope Contrib SW"

%.pd.1 : %.xpls
	pod2man -n $* $(PODARGS1) $< $@

%.pd.1 : %.xpl
	pod2man -n $* $(PODARGS1) $< $@

%.pd.1 : %
	pod2man $(PODARGS1) $< $@

%.pd.3p : %
	pod2man -n $* $(PODARGS3) $< $@

$(DEST)/man/man1/%.1 : %.pd.1
	install $< $@

$(DEST)/man/man3/%.3p : %.pd.3p
	install $< $@

