SUBDIR=/contrib
include $(ANTELOPEMAKE)
FIRSTDIRS=first
DIRS=first lib bin data java adm

TOPDOCS=README.md

install::
	$(INSTALL) $(TOPDOCS) $(DEST)

uninstall::
	@echo uninstalling $(TOPDOCS)
	$(RM) $(TOPDOCS)
