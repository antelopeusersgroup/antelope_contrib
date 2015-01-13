BIN  = dbtimerf
MAN1 = $(BIN).1

ldlibs= $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS  = $(BIN).o
OBJS += write_my_wf.o
OBJS += gaussj.o
OBJS += get_my_wf_.o

$(BIN): $(BIN).f $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LDLIBS) -o $@
