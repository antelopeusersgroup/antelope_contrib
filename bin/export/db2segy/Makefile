BIN     = db2segy
MAN1    = $(BIN).1
INCLUDE = segy.h
PF      = $(BIN).pf
DATADIR = schemas
DATA    = segy1.0

ldlibs=$(TRLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) $(F77LIBS)
