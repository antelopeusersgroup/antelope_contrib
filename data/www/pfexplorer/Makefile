DATADIR= www/cgibin

DATA= pfexplorer.php

MAN3H= pfexplorer.3h

CLEAN = *.php

include $(ANTELOPEMAKE)

.SUFFIXES: .SUFFIXES .wphp .php

.wphp.php : $(ANTELOPE)/data/templates/wphp
	$(RM) $@
	cat $(ANTELOPE)/data/templates/wphp $*.wphp > $@




