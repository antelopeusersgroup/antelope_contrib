BIN  = setdne
MAN1 = $(BIN).1

cflags=-g
ldlibs=$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS = $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
