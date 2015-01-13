BIN  = dbfix_phase
PF   = $(BIN).pf
MAN1 = $(BIN).1
CLEAN=$(BIN)
cflags=
ldflags=
ldlibs=$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=
OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
