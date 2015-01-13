BIN  = ref2db
MAN1 = $(BIN).1

ldlibs = -ldefunctpkt $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS  = $(BIN).o
OBJS += openIP.o
OBJS += parfile.o
OBJS += read_input.o
OBJS += read_psclIP.o
OBJS += record.o
OBJS += save_seed.o
OBJS += util.o
OBJS += valid_pkt.o
OBJS += wrt2db.o

$(BIN):	$(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)
