LIB = libslink.a

include $(ANTELOPEMAKE)
DIRS=

OBJS = tswap.o unpack.o msrecord.o genutils.o strutils.o \
       logging.o network.o statefile.o config.o \
       slplatform.o slutils.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@
