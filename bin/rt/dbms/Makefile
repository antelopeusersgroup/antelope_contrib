BIN  = dbms
MAN1 = $(BIN).1

ldlibs = $(TRLIBS) $(F77LIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = $(BIN).o dbutil.o domag.o proc_sta.o save_ms.o util.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
