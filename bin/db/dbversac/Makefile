BIN  = dbversac
MAN1 = $(BIN).1
ldflags =
ldlibs = $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

SRCS = $(BIN).c chksac.c

$(BIN) : $(SRCS:.c=.o)
	$(CC) -o $@ $(CFLAGS) $(SRCS:.c=.o) $(LDFLAGS) $(LDLIBS)
