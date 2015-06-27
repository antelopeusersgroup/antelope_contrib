BIN  = guralplocate
MAN1 = $(BIN).1

cflags=-g
ldlibs=$(DBLIBS) -lmultiwavelet $(TRLIBS) -lgenloc -lperf


SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
