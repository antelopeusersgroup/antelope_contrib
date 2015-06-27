BIN  = dccmd
MAN1 = dccmd.1

ldlibs= -ldefunctpkt -lorb $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJSM= $(BIN).o sendcmd.o util.o

$(BIN):$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
