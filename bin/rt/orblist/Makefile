BIN = orblist

ldlibs	=  -lsocket -lnsl -lpkt -lorb $(TRLIBS)
include $(ANTELOPEMAKE)
DIR=

OBJST	= orblist.o
$(BIN):	 $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
