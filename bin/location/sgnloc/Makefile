BIN  = sgnloc
PF   = $(BIN).pf
MAN1 = $(BIN).1

cflags =
ldflags =
ldlibs= -lgenloc $(DBLIBS) $(PERFLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = $(BIN).o
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
