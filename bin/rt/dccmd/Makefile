BIN = dccmd
MAN1=dccmd.1

ldlibs	= -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
OBJSM	= dccmd.o\
	  sendcmd.o\
	  util.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
