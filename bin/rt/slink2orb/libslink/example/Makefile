CC = gcc
CFLAGS = -O2 -Wall -I..

# For using another compiler (e.g. the Sun compiler)
#CC = cc
#CFLAGS = -O -I..
 
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

clean:
	rm -f $(OBJS) $(BIN)

