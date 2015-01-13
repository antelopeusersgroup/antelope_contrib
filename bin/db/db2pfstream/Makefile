BIN=db2pfstream
DLIB=$(LIB:.a=$(DSUFFIX))
PF=db2pfstream.pf

MAN1=db2pfstream.1
#ldlibs=-lglputil  -xlic_lib=sunperf,fui,fai,fsu $(DBLIBS)
#ldlibs=-lglputil  -xlic_lib=sunperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=-lglputil  -lperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=-lglputil $(PERFLIBS) -lpfstream $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS=db2pfstream.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
