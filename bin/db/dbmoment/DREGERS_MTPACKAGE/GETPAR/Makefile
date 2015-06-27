LIB = libget.a

SUBDIR=/contrib
include $(ANTELOPEMAKE)

#CFLAGS=-Wno-implicit -Wno-return-type -c -O -DENVIRONMENT

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)


#	gcc $(CFLAGS) $(SRCS)
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
	\cp $(LIB) ../

