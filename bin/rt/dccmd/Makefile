BIN = dccmd
MAN1=dccmd.1
#cflags  = -g
#ldflags	= -Bstatic               
ldlibs	= -lsocket -lnsl -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
OBJSM	= dccmd.o\
	  collect_dases.o\
	  open_dc.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
