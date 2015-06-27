BIN = dbserver
MAN1= $(BIN).1

ldlibs=-ldbl2 -ldbptolemy -ldbxml $(DBLIBS) $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

