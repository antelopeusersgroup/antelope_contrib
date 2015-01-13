BIN  = ia2orb
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs= -lbrttpkt -lbrttutil -lpthread $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
