BIN  = pmelavg
PF   = $(BIN).pf
MAN1 = $(BIN).1

ldlibs  = -lgenloc -lglputil $(DBLIBS)
ldlibs += -ltrvltm -ldl -lm -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS=$(BIN).o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
