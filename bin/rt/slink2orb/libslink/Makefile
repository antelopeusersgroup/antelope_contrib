
# Special Makefile for Antelope build system

LIB = libslink.a

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

OBJS = gswap.o unpack.o msrecord.o genutils.o strutils.o \
       logging.o network.o statefile.o config.o \
       slplatform.o slutils.o globmatch.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
