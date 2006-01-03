# %W% %G% 
# Makefile for svd

BIN = ipd2
MAN1= ipd2.1   
ldflags=-L.
ldlibs	= -ldefunctpkt2 -lorb $(TRLIBS)

include $(ANTELOPEMAKE)
OBJSM	= ipd2.o\
	  newpf.o\
	  openIP.o\
	  read_in_port.o\
	  send2orb.o\
	  sig_hdlr.o\
	  valid_pkt.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -g -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
