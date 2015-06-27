BIN  = orblist
MAN1 = $(BIN).1

ldlibs=  -ldefunctpkt -lorb $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIR=

OBJST= orblist.o
$(BIN): $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
