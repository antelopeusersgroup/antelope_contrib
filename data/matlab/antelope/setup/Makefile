
CLEAN= setup.m

include $(ANTELOPE)/local/include/mex_antelope_makerules

all :: setup.m

install :: $(ANTELOPE)/setup.m

setup.m: setup.m.id
	getid < setup.m.id > $@

$(ANTELOPE)/setup.m: setup.m
	deposit setup.m $(ANTELOPE)	
