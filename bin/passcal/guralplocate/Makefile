BIN=guralplocate

MAN1=guralplocate.1
cflags=-g
ldlibs=$(DBLIBS) -lmultiwavelet $(TRLIBS) -lgenloc -lperf


include $(ANTELOPEMAKE)
OBJS=guralplocate.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
