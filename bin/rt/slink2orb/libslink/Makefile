
# Build environment can be configured the following
# environment variables:
#   CC : Specify the C compiler to use
#   CFLAGS : Specify compiler options to use

# GCC specific parameters
GCC = gcc
GCCFLAGS = -O2 -Wall

LIB_OBJS = gswap.o unpack.o msrecord.o genutils.o strutils.o \
           logging.o network.o statefile.o config.o \
           globmatch.o slplatform.o slutils.o

CURRENT_VER = 2.4
COMPAT_VER = 2.0

LIB_A = libslink.a
LIB_SO = libslink.so.$(CURRENT_VER)
LIB_SO_ALIAS = libslink.so
LIB_DYN = libslink.$(CURRENT_VER).dylib
LIB_DYN_ALIAS = libslink.dylib

all: static

static: $(LIB_A)

shared: $(LIB_SO)

dynamic: $(LIB_DYN)

$(LIB_A): $(LIB_OBJS)
	ar -rcs $(LIB_A) $(LIB_OBJS)

$(LIB_SO): $(LIB_OBJS)
	$(CC) -shared -Wl,-soname -Wl,$(LIB_SO_ALIAS) -o $(LIB_SO) $(LIB_OBJS)
	ln -s $(LIB_SO) $(LIB_SO_ALIAS)

$(LIB_DYN): $(LIB_OBJS)
	$(CC) -dynamiclib -compatibility_version $(COMPAT_VER) -current_version $(CURRENT_VER) -install_name $(LIB_DYN_ALIAS) -o $(LIB_DYN) $(LIB_OBJS)
	ln -s $(LIB_DYN) $(LIB_DYN_ALIAS)

cc:
	@$(MAKE) "CC=$(CC)" "CFLAGS=$(CFLAGS)"

gcc:
	@$(MAKE) "CC=$(GCC)" "CFLAGS=$(GCCFLAGS)"

debug:
	$(MAKE) "CC=$(CC)" "CFLAGS=-g $(CFLAGS)"

gccdebug:
	$(MAKE) "CC=$(GCC)" "CFLAGS=-g $(GCCFLAGS)"

clean:
	rm -f $(LIB_OBJS) $(LIB_A) $(LIB_SO) $(LIB_SO_ALIAS) $(LIB_DYN) $(LIB_DYN_ALIAS)

install:
	@echo
	@echo "No install method, copy the library, header file, and"
	@echo "documentation to the preferred install location"
	@echo
