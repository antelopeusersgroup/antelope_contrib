BIN  = $(BIN)
MAN1 = $(BIN).1

ldlibs= -ldefunctpkt $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJSM= $(BIN).o\
	  openIP.o\
	  read_in_port.o

$(BIN):$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
