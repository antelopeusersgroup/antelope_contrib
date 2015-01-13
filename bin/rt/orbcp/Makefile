BIN  = orbcp
MAN1 = $(BIN).1

cflags = -g
ldflags =
ldlibs= $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
