BIN  = orb2rrdc
PF   = $(BIN).pf
MAN1 = $(BIN).1

CLEAN=

cflags=
fflags=
ldflags=
ldlibs= -lbrttpkt $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=


OBJS= $(BIN).o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
