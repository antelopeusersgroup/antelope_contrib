BIN=pfstream2db
PF=pfstream2db.pf
MAN1=pfstream2db.1

#ldlibs=-lglputil  -xlic_lib=sunperf,fui,fai,fsu $(DBLIBS)
#ldlibs=-lglputil  -xlic_lib=sunperf $(DBLIBS) -lpfstream -lbrttutil
# ldlibs=-lglputil -lperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=$(TRLIBS) -lpfstream -lbrttutil

include $(ANTELOPEMAKE)
DIRS=
