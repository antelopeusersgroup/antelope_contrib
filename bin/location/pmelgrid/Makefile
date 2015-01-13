BIN  = pmelgrid
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs=-lpfstream -lpmel -lgenloc -lglputil $(PERFLIBS) $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o pfutilities.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
