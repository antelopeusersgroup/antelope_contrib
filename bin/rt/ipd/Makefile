# %W% %G% 
# Makefile for svd

BIN = ipd
MAN1=ipd.1   
ldflags=-L.
ldlibs	= -lsocket -lnsl -Bstatic -lpkt -lorb -Bdynamic $(TRLIBS)

include $(ANTELOPEMAKE)
OBJSM	= ipd.o\
	  newpf.o\
	  openIP.o\
	  read_in_port.o\
	  send2orb.o\
	  sig_hdlr.o\
	  valid_pkt.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
