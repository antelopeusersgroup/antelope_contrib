
# Build environment can be configured the following
# environment variables:
#   CC : Specify the C compiler to use
#   CFLAGS : Specify compiler options to use

# GCC specific parameters
GCC = gcc
GCCFLAGS = -O2 -Wall -I..

# Required compiler parameters
CFLAGS += -I..
 
LDFLAGS = -L..
LDLIBS = -ldali

# For SunOS/Solaris uncomment the following line
#LDLIBS = -ldali -lsocket -lnsl -lrt

BIN = daliclient
 
OBJS = daliclient.o

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
