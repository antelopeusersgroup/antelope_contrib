# Standard compiler parameters
CFLAGS = -O -I..

# GCC specific parameters
GCC = gcc
GCCFLAGS = -O2 -Wall -I..
 
LDFLAGS = -L..
LDLIBS = -lslink

# For SunOS/Solaris uncomment the following line
#LDLIBS = -lslink -lsocket -lnsl -lrt

BIN = slclient
 
OBJS = slclient.o

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)

static: $(OBJS)
	$(CC) -static $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)

cc:
	@$(MAKE) "CC=$(CC)" "CFLAGS=$(CFLAGS)"

gcc:
	@$(MAKE) "CC=$(GCC)" "CFLAGS=$(GCCFLAGS)"

debug:
	$(MAKE) "CC=$(CC)" "CFLAGS=-g $(CFLAGS)"

gccdebug:
	$(MAKE) "CC=$(GCC)" "CFLAGS=-g $(GCCFLAGS)"

clean:
	rm -f $(OBJS) $(BIN)
