BIN  = db2pfstream
PF   = $(BIN).pf
MAN1 = $(BIN).1

DLIB=$(LIB:.a=$(DSUFFIX))
#ldlibs=-lglputil  -xlic_lib=sunperf,fui,fai,fsu $(DBLIBS)
#ldlibs=-lglputil  -xlic_lib=sunperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=-lglputil  -lperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=-lglputil $(PERFLIBS) -lpfstream $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
