# %W% %G% 
# Makefile for b3s2
PF=b3s2orb.pf
BIN = b3s2orb
MAN1=b3s2orb.1

ldlibs	=  -lsocket -lnsl -lpkt -lorb -lxview -lolgx -lX11 $(TRLIBS) 
include $(ANTELOPEMAKE)
DIR=

OBJST	= b3s2.o\
	  send2orb.o\
	  uic_ui.o
$(BIN):	 $(OBJST)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJST) $(LDFLAGS) $(LDLIBS)
