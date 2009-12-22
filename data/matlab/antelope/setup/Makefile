
CLEAN= setup.m

include $(ANTELOPE)/include/mex_antelope_makerules

all :: setup.m

install :: $(ANTELOPE)/setup.m

setup.m: setup.m.id
	getid < $< > $@

$(ANTELOPE)/setup.m: setup.m
	deposit $< $(ANTELOPE)	
