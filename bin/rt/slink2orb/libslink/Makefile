
LIB = libslink.a

include $(ANTELOPEMAKE)
DIRS=

OBJS = tswap.o unpack.o msrecord.o slutils.o strutils.o \
           logging.o network.o statefile.o config.o \
           slplatform.o main.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@


