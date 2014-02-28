BIN=catalog_cleanup

MAN1=catalog_cleanup.1

cflags=-g
ldlibs= $(DBLIBS) -lseispp

include $(ANTELOPEMAKE)

DIRS=



OBJS=catalog_cleanup.o HypocenterCSS30.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
