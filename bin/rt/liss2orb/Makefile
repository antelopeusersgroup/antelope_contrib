BIN=liss2orb
MAN1=liss2orb.1

cflags=
ldlibs=	$(ORBLIBS)

include $(ANTELOPEMAKE)
DIRS=

SRCS= \
    seed.o \
    csteim.o \
    newsteim.o \
    usteim.o \
    liss2orb.c \
    liss2orbpkt.c \
    socket.c \
    unseed.c

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

