# Makefile for svd

BIN = mrc
MAN1=mrc.1
#cflags  = -g
#ldflags	= -Bstatic               
ldlibs	= -lsocket -lnsl -lpkt -lorb $(TRLIBS) 

include $(DSAPMAKE)
OBJSM	= mrc.o\
	  open_dc.o\
	  proc_chan.o\
	  sendmrc.o\
	  util.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
