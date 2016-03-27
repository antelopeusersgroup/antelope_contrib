LIB = c_libs.a

SUBDIR=/contrib
include $(ANTELOPEMAKE)

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)


$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
	\cp $(LIB) ../
