BIN = rddas
MAN1=rddas.1

ldlibs	= -ldefunctpkt $(ORBLIBS)

include $(ANTELOPEMAKE)
OBJSM	= rddas.o\
	  openIP.o\
	  read_in_port.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
