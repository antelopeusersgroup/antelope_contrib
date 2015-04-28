LIB = nrcv2.a

CFLAGS= -arch x86_64

include $(ANTELOPEMAKE)

SRCS=$(wildcard *.c)

OBJS=$(SRCS:.c=.o)


$(LIB) : $(OBJS)
	ar cr $@ $^
	\mv $(LIB) ../

.c.o:
	$(CC) -c $< -o $@
