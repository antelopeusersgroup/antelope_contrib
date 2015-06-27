#
# Create missing directories under $ANTELOPE for local compilation
#
SUBDIR=/contrib

DIRNAMES += $(ANTELOPE)$(SUBDIR)/man
DIRNAMES += $(ANTELOPE)$(SUBDIR)/man/man1
DIRNAMES += $(ANTELOPE)$(SUBDIR)/man/man3
DIRNAMES += $(ANTELOPE)$(SUBDIR)/man/man5
DIRNAMES += $(ANTELOPE)$(SUBDIR)/bin
DIRNAMES += $(ANTELOPE)$(SUBDIR)/include
DIRNAMES += $(ANTELOPE)$(SUBDIR)/lib
DIRNAMES += $(ANTELOPE)$(SUBDIR)/data
DIRNAMES += $(ANTELOPE)$(SUBDIR)/static

all Include install installMAN pf relink :: dirs

dirs: $(DIRNAMES)

$(ANTELOPE)$(SUBDIR) :
	mkdir -p $@

$(DIRNAMES) : $(ANTELOPE)$(SUBDIR)
	mkdir -p $@
