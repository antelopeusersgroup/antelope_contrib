# Makefile for svd

BIN = sendMRC
MAN1= sendMRC.1   
PF  = sendMRC.pf

ldlibs	= -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
OBJSM	= mrc.o\
	  collect_dases.o\
	  open_dc.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
