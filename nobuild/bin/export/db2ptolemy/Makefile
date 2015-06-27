
BIN= db2ptolemy
PF=

MAN1= db2ptolemy.1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -ldbptolemy $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DBG= -g

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

