BIN=q330_baler
PF=q330_baler.pf
MAN1=q330_baler.1
UNINSTALL=$(ANTELOPE)/data/perl/baler.pm

include $(ANTELOPEMAKE)
DIRS=

install:: $(ANTELOPE)/data/perl/baler.pm

$(ANTELOPE)/data/perl/baler.pm : baler.pm
	$(RM) $@
	deposit baler.pm $(ANTELOPE)/data/perl
