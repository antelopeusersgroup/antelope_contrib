# Makefile for svd

BIN = acalarm battalarm
MAN1 = rtalarm.1
ldlibs	= -lsocket -lnsl -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
acalarm:	acalarm.o
	$(CC) $(CFLAGS) -o acalarm acalarm.o $(LDFLAGS) $(LDLIBS)
battalarm:	battalarm.o
	$(CC) $(CFLAGS) -o battalarm battalarm.o $(LDFLAGS) $(LDLIBS)
