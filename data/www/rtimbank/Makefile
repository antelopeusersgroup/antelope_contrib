DATADIR= www/cgibin

DATA= rtimbank.php \
      rtimbank.pf

MAN3H= rtimbank.3h

CLEAN = *.php

include $(ANTELOPEMAKE)

.SUFFIXES: .SUFFIXES .wphp .php

.wphp.php : $(ANTELOPE)/data/templates/wphp
	$(RM) $@
	cat $(ANTELOPE)/data/templates/wphp $*.wphp > $@




