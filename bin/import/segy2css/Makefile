BIN  = segy2css
MAN1 = $(BIN).1
ldlibs = $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

OBJS = decode.o \
	init.o \
	main.o \
	rd_segfil.o \
	tmutils.o \
	util.o \
	wrt_segdata.o \
	wrt_segwfd.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
