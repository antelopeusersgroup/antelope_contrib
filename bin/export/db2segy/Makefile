BIN=db2segy
MAN1=db2segy.1
INCLUDE=segy.h
PF=db2segy.pf
DATADIR=schemas
DATA=segy1.0

ldlibs=$(TRLIBS) $(F77LIBS)

include $(ANTELOPEMAKE)
DIRS=
OBJS= db2segy.o
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) $(F77LIBS)
