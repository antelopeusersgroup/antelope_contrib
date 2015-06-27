BIN  = reftek_dbtiming
MAN1 = $(BIN).1

cflags=-g
ldflags=
ldlibs=$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

