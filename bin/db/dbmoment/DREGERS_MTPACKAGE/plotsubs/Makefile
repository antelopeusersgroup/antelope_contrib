LIB = libstd.a

include $(ANTELOPEMAKE)


SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
	\cp $(LIB) ../
