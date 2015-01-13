BIN=q330gpslocate

MAN1=q330gpslocate.1
cflags=-g
ldlibs=$(DBLIBS) -lmultiwavelet $(TRLIBS) -lgenloc -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)
OBJS=q330gpslocate.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
