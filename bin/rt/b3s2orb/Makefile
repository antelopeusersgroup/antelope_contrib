# %W% %G% 
# Makefile for b3s2
BIN = b3s2orb
#MAN1=b3s2.1

ldlibs	=  -lsocket -lnsl -lpkt -lorb -lxview -lolgx $(TRLIBS) 
include $(DSAPMAKE)
DIR=

OBJST	= b3s2.o\
	  send2orb.o\
	  uic_ui.o
$(BIN):	 $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
