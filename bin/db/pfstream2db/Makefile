BIN  = pfstream2db
PF   = $(BIN).pf
MAN1 = $(BIN).1

#ldlibs=-lglputil  -xlic_lib=sunperf,fui,fai,fsu $(DBLIBS)
#ldlibs=-lglputil  -xlic_lib=sunperf $(DBLIBS) -lpfstream -lbrttutil
# ldlibs=-lglputil -lperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=$(TRLIBS) -lpfstream -lbrttutil

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=
