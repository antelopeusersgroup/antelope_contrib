BIN = orbcp

MAN1 = orbcp.1

cflags =  -g
ldflags = 
ldlibs= $(ORBLIBS)

include $(ANTELOPEMAKE)
DIRS=

OBJS= \
	orbcp.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
