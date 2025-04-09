BIN     = slink2orb
PF      = $(BIN).pf
MAN1    = $(BIN).1

cflags  = -Ilibslink -Imbedtls/include -Ilibmseed
ldflags = -Llibslink -Llibmseed
ldlibs  = $(ORBLIBS) -lslink -lmseed

SUBDIR  = /contrib
include $(ANTELOPEMAKE)

DIRS    = libslink libmseed

OBJS    = mseed2orbpkt.o $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
