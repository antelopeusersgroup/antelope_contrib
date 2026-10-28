BIN     = slink2orb
PF      = $(BIN).pf
MAN1    = $(BIN).1

cflags  = -Ilibslink -Imbedtls/include -Ilibmseed
ldflags = -Llibslink -Llibmseed
ldlibs  = $(ORBLIBS) -lslink -lmseed

SUBDIR  = /contrib
include $(ANTELOPEMAKE)

export CC CPPFLAGS CFLAGS LDFLAGS

OBJS    = mseed2orbpkt.o $(BIN).o

LIBSLINK_A = libslink/libslink.a
LIBMSEED_A = libmseed/libmseed.a

$(BIN): $(OBJS) $(LIBSLINK_A) $(LIBMSEED_A)

$(LIBSLINK_A):
	@$(MAKE) -C libslink static

$(LIBMSEED_A):
	@$(MAKE) -C libmseed static
