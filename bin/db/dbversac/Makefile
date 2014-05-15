
BIN=dbversac
MAN1=dbversac.1
ldflags = 
ldlibs = $(DBLIBS)

include $(ANTELOPEMAKE)

SRCS = dbversac.c chksac.c

dbversac : $(SRCS:.c=.o)
	$(CC) -o $@ $(CFLAGS) $(SRCS:.c=.o) $(LDFLAGS) $(LDLIBS)


# $Id$ 
