BIN  = extrd
MAN1 = $(BIN).1

cflags  = -g
ldlibs  = -lscv2 -ldbl2 $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJS  = $(BIN).o
OBJS += create_dir.o
OBJS += dtsplit.o
OBJS += find_path.o
OBJS += get_data.o
OBJS += mkfname.o
OBJS += parse_name.o
OBJS += steim_util.o
OBJS += sig_hdlr.o
OBJS += util.o
OBJS += wrt_data.o

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)
