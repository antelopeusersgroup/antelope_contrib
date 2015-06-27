BIN  = dbdec
MAN1 = $(BIN).1

cflags=-g
ldlibs=-lscv2 -ldbl2 $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS=$(BIN).o merge_db.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
