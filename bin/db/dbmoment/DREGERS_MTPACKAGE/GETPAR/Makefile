LIB = libget.a

include $(ANTELOPEMAKE)

CFLAGS=-Wno-implicit -Wno-return-type -c -O -DENVIRONMENT

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)


$(LIB) : $(OBJS)
	gcc $(CFLAGS) $(SRCS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
	\cp $(LIB) ../

