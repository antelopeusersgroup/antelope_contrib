BIN = dbserver
MAN1= dbserver.1

ldlibs=	-ldbl2 -ldbptolemy -ldbxml $(DBLIBS) $(ORBLIBS)

include $(ANTELOPEMAKE)

OBJS = dbserver.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

