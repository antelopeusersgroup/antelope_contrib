DATADIR= www

DATA= wfplotter.php

MAN3H= wfplotter.3h

CLEAN = *.php

include $(ANTELOPEMAKE)

.SUFFIXES: .SUFFIXES .wphp .php

.wphp.php : $(ANTELOPE)/data/templates/wphp
	$(RM) $@
	cat $(ANTELOPE)/data/templates/wphp $*.wphp > $@
