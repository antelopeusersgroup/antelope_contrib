CC = cc
CFLAGS = -O 

# For using another compiler (e.g. the Sun compiler)
#CC = cc
#CFLAGS = -O 

LDFLAGS =
LDLIBS =

LIB_OBJS = tswap.o unpack.o msrecord.o slutils.o strutils.o \
           logging.o network.o statefile.o config.o \
           slplatform.o main.o

LIB_A = libslink.a
LIB_SO = libslink.so.0.1
LIB_SO_ALIAS = libslink.so

all: static

static: $(LIB_A)

shared: $(LIB_SO)

clean:
	rm -f $(LIB_OBJS) $(LIB_A) $(LIB_SO) $(LIB_SO_ALIAS)

$(LIB_A): $(LIB_OBJS)
	ar -rc $(LIB_A) $(LIB_OBJS)

$(LIB_SO): $(LIB_OBJS)
	$(CC) -shared -Wl,-soname -Wl,$(LIB_SO_ALIAS) -o $(LIB_SO) $(LIB_OBJS)

install:
