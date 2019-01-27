BIN=dbimportQCmetrics
cflags=-g
ldlibs=$(DBLIBS)
SUBDIR=/contrib

include $(ANTELOPEMAKE) 
#include $(ANTELOPEMAKELOCAL)


OBJS=dbimportQCmetrics.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
