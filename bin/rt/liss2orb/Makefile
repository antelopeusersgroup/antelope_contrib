#
#PF=liss.pf
MAN1=liss2orb.1
BIN=liss2orb
#
cflags=
ldlibs=	-lpkt -lorb $(TRLIBS) $(SVR4LIBS) -ldl

include $(ANTELOPEMAKE)
DIRS=

SRCS= \
    liss2orb.c \
    open_socket.c \
    read_server.c \
    stuff_liss.c \
    util.c 

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


