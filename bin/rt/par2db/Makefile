BIN  = par2db
MAN1 = par2db.1

ldlibs = -ldefunctpkt $(ORBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

SRCS= \
    adc_par.c \
    anza_par.c \
    bba_par.c \
    dc_clock_stat.c \
    dc_par.c \
    gps_coord.c \
    par2db.c \
    record.c \
    save_seed.c \
    unstuffpar.c \
    util.c

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
