BIN=par2db

cflags=
ldlibs=	-lpkt -lorb $(TRLIBS) $(SVR4LIBS) -ldl

include $(DSAPMAKE)
DIRS=

SRCS= \
    anza_par.c \
    bba2_par.c \
    flush2db.c \
    get_match.c \
    gps_coord.c \
    init.c \
    par2db.c \
    new_free_util.c \
    pkt2db.c \
    seg_append.c \
    unstuffpar.c \
    usage.c

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


