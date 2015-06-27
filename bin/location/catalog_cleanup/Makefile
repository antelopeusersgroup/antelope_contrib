BIN  = catalog_cleanup
MAN1 = catalog_cleanup.1

cflags=-g
ldlibs= $(DBLIBS) -lseispp

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = $(BIN).o HypocenterCSS30.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
