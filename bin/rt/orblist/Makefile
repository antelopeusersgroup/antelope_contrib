BIN = orblist
MAN1=orblist.1
ldlibs	=  -ldefunctpkt -lorb $(TRLIBS)
include $(ANTELOPEMAKE)
DIR=

OBJST	= orblist.o
$(BIN):	 $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
