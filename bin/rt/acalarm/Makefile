BIN = acalarm battalarm
MAN1 = acalarm.1

ldlibs= $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

acalarm: acalarm.o
	$(CC) $(CFLAGS) -o acalarm acalarm.o $(LDFLAGS) $(LDLIBS)
battalarm: battalarm.o
	$(CC) $(CFLAGS) -o battalarm battalarm.o $(LDFLAGS) $(LDLIBS)
