BIN  = mseed2seedpkt
MAN1 = $(BIN).1

ldlibs= $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS=seedutil.o mseed2orbpkt.o $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
